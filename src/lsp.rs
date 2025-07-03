use lsp_types::notification::{Notification, PublishDiagnostics};
use lsp_types::{
    ClientCapabilities, DidOpenTextDocumentParams, Hover, HoverContents, InitializeParams,
    InitializeResult, InitializedParams, Range, TextDocumentItem, Url, WorkDoneProgressParams,
    WorkspaceFolder,
};
use molt_lib::{NodeId, ParsingMode, Pattern};
use rust_grammar::parse::ParsePat;
use rust_grammar::{Field, FieldNamed, Pat, Stmt, Type};
use serde::de::DeserializeOwned;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::{HashSet, VecDeque};
use std::io::{BufRead, BufReader, Write};
use std::path::Path;
use std::process::{Child, Command, Stdio};
use std::time::{Duration, Instant};
use uuid::Uuid;

use crate::type_check::LspType;
use crate::{Ctx, Error};

type Result<T, E = Box<dyn std::error::Error>> = std::result::Result<T, E>;

const RUST_ANALYZER_TIMEOUT: u64 = 10;

#[derive(Debug, Clone, Serialize, Deserialize)]
struct LspRequest {
    jsonrpc: String,
    id: String,
    method: String,
    params: Value,
}

impl LspRequest {
    fn new(method: &str, params: Value) -> LspRequest {
        LspRequest {
            jsonrpc: "2.0".to_string(),
            id: Uuid::new_v4().to_string(),
            method: method.to_string(),
            params,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct LspResponse {
    jsonrpc: String,
    id: Option<String>,
    result: Option<Value>,
    error: Option<LspError>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct LspError {
    code: i32,
    message: String,
    data: Option<Value>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct LspNotification {
    jsonrpc: String,
    method: String,
    params: Value,
}

impl LspNotification {
    fn new(method: &str, params: Value) -> LspNotification {
        LspNotification {
            jsonrpc: "2.0".to_string(),
            method: method.to_string(),
            params,
        }
    }
}

struct RealLspClient {
    process: Child,
    reader: BufReader<std::process::ChildStdout>,
    notification_queue: VecDeque<LspNotification>,
    opened_files: HashSet<std::path::PathBuf>,
}

pub struct LspClient {
    inner: Option<RealLspClient>,
}

impl RealLspClient {
    fn new() -> Result<Self> {
        let mut process = Command::new("rust-analyzer")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()?;

        let stdout = process.stdout.take().unwrap();
        let reader = BufReader::new(stdout);

        Ok(RealLspClient {
            process,
            reader,
            notification_queue: VecDeque::new(),
            opened_files: HashSet::new(),
        })
    }

    fn read_line(&mut self) -> Result<String> {
        let mut line = String::new();
        self.reader.read_line(&mut line)?;
        Ok(line)
    }

    fn send_request<T: Serialize, S: DeserializeOwned>(
        &mut self,
        method: &str,
        params: T,
    ) -> Result<Option<S>> {
        let params = serde_json::to_value(params)?;
        let request = LspRequest::new(method, params);
        self.send_lsp_message(request)?;
        let response = self.read_response()?;
        if let Some(error) = response.error {
            return Err(error.message.into());
        }
        Ok(response
            .result
            .map(|result| serde_json::from_value(result))
            .transpose()?)
    }

    fn send_notification<T: Serialize>(&mut self, method: &str, params: T) -> Result<()> {
        let params = serde_json::to_value(params)?;
        let notification = LspNotification::new(method, params);
        self.send_lsp_message(notification)?;
        Ok(())
    }

    fn send_lsp_message<T: Serialize>(&mut self, t: T) -> Result<()> {
        let body = serde_json::to_string(&t)?;
        let message = format!("Content-Length: {}\r\n\r\n{}", body.len(), body);
        let stdin = self.process.stdin.as_mut().unwrap();
        stdin.write_all(message.as_bytes())?;
        stdin.flush()?;
        Ok(())
    }

    fn read_response(&mut self) -> Result<LspResponse> {
        loop {
            if let Some(response) = self.read_single_response()? {
                return Ok(response);
            }
        }
    }

    fn read_single_response(&mut self) -> Result<Option<LspResponse>> {
        let header = self.read_line()?;
        let content_length = header
            .trim()
            .strip_prefix("Content-Length: ")
            .ok_or("Invalid Content-Length header")?
            .parse::<usize>()?;
        let _empty_line = self.read_line()?;
        let mut body = vec![0u8; content_length];
        std::io::Read::read_exact(&mut self.reader, &mut body)?;
        if let Ok(response) = serde_json::from_slice::<LspResponse>(&body) {
            // If it has an ID, it's a response to a request
            if response.id.is_some() {
                return Ok(Some(response));
            }
        }
        if let Ok(notification) = serde_json::from_slice::<LspNotification>(&body) {
            // Store notification in queue for later processing
            self.notification_queue.push_back(notification);
        }
        Ok(None)
    }

    fn drain_notifications(&mut self) -> Vec<LspNotification> {
        self.notification_queue.drain(..).collect()
    }

    fn wait_for_publish_diagnostics(&mut self, timeout: Duration) -> Result<()> {
        let start_time = Instant::now();
        while start_time.elapsed() < timeout {
            self.read_single_response()?;
            for notification in self.drain_notifications() {
                if notification.method == PublishDiagnostics::METHOD {
                    return Ok(());
                }
            }
            std::thread::sleep(Duration::from_millis(10));
        }

        Err("Timeout waiting for progress completion".into())
    }

    pub fn initialize(&mut self, root_path: &Path) -> Result<()> {
        let root_uri = Url::from_file_path(root_path.canonicalize().unwrap())
            .map_err(|_| "Invalid file path")?;
        let initialization_options = serde_json::json!({
            "hover": {
                "documentation": {
                    "enable": false
                },
                "dropGlue": {
                    "enable": false
                },
                "memoryLayout": {
                    "enable": false
                },
                "links": {
                    "enable": false
                },
                "show": {
                    "enumVariants": null,
                    "fields": null,
                    "traitAssocItems": null,
                },
                "maxSubstitutionLength": null,

            }
        });
        let params = InitializeParams {
            process_id: None,
            initialization_options: Some(initialization_options),
            capabilities: ClientCapabilities::default(),
            trace: None,
            workspace_folders: Some(vec![WorkspaceFolder {
                uri: root_uri,
                name: "workspace".to_string(),
            }]),
            client_info: None,
            locale: None,
            work_done_progress_params: WorkDoneProgressParams::default(),
            ..Default::default()
        };
        self.send_request::<_, InitializeResult>("initialize", params)?;
        self.send_notification("initialized", serde_json::to_value(InitializedParams {})?)?;

        Ok(())
    }

    pub fn did_open(&mut self, file_path: &Path, content: &str) -> Result<()> {
        let canonical_path = file_path.canonicalize()?;

        if self.opened_files.contains(&canonical_path) {
            return Ok(());
        }

        let uri = Url::from_file_path(&canonical_path).map_err(|_| "Invalid file path")?;
        let params = DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: uri.clone(),
                language_id: "rust".to_string(),
                version: 1,
                text: content.to_string(),
            },
        };
        self.send_notification("textDocument/didOpen", params)?;
        self.wait_for_publish_diagnostics(Duration::from_secs(RUST_ANALYZER_TIMEOUT))
            .unwrap();

        self.opened_files.insert(canonical_path);
        Ok(())
    }

    pub fn get_type_at_range(&mut self, file_path: &Path, range: Range) -> Result<Option<LspType>> {
        let uri = Url::from_file_path(file_path.canonicalize().unwrap())
            .map_err(|_| "Invalid file path")?;
        // rust-analyzer supports passing ranges instead of
        // just positions. However, this is an LSP extension
        // and so it is not supported by the strongly typed
        // lsp-types equivalent (HoverParams).
        // Therefore, we create a custom json.
        let params = serde_json::json!({
            "textDocument": {
                "uri": uri
            },
            "position": {
                "start": range.start,
                "end": range.end
            }
        });
        let response =
            self.send_request::<_, Hover>("textDocument/hover", serde_json::to_value(params)?)?;
        response
            .and_then(|result| self.extract_type_from_hover(result).transpose())
            .transpose()
    }

    fn extract_type_from_hover(&self, hover: Hover) -> Result<Option<LspType>> {
        if let HoverContents::Markup(str) = hover.contents {
            let mut lines = str.value.lines();
            let line = lines.next().ok_or_else(|| "Expected line")?;
            Ok(parse_type_from_str(line))
        } else {
            Err("Unexpected hover contents format.".into())
        }
    }
}

fn try_parse_as<T: ParsePat>(
    line: &str,
    f: impl Fn(&Ctx, &<T as ParsePat>::Target) -> Option<NodeId<Type>>,
) -> Option<(NodeId<Type>, Ctx)> {
    let (id, ctx) =
        rust_grammar::parse_ctx(|input| input.parse_id::<T>(), line, ParsingMode::Real).ok()?;
    f(&ctx, ctx.get(id).unwrap_real()).map(|item| (item, ctx))
}

fn parse_type_from_str(line: &str) -> Option<LspType> {
    let result = try_parse_as::<FieldNamed>(&line, |_, field: &Field| Some(field.ty))
        .or_else(|| {
            // Ugly: Add a semicolon to allow parsing into a statement.
            let line = format!("{line};");
            try_parse_as::<Stmt>(&line, |ctx: &Ctx, stmt: &Stmt| match stmt {
                Stmt::Local(local) => match ctx.get(local.pat) {
                    Pattern::Real(Pat::Type(type_)) => Some(type_.ty),
                    _ => None,
                },
                _ => None,
            })
        })
        .or_else(|| {
            rust_grammar::parse_ctx(|input| input.parse_id::<Type>(), &line, ParsingMode::Real).ok()
        });
    if result.is_none() {
        println!("Failed to parse type in LSP response. LSP response: {line}");
        return None;
    }
    let (type_, ctx) = result.unwrap();
    Some(LspType {
        ctx,
        type_,
        src: line.to_string(),
    })
}

impl LspClient {
    pub(crate) fn uninitialized() -> Self {
        Self { inner: None }
    }

    pub(crate) fn new(root: &Path) -> Result<Self, Error> {
        let mut client = RealLspClient::new()
            .map_err(|e| Error::Misc(format!("Failed to spawn LSP process: {}", e)))?;
        client
            .initialize(&root)
            .map_err(|e| Error::Misc(format!("Failed to initialize LSP client: {}", e)))?;
        Ok(Self {
            inner: Some(client),
        })
    }

    pub fn did_open(&mut self, file_path: &Path, content: &str) -> Result<()> {
        self.inner.as_mut().unwrap().did_open(file_path, content)
    }

    pub fn get_type_at_position(
        &mut self,
        file_path: &Path,
        range: Range,
    ) -> Result<Option<LspType>> {
        self.inner
            .as_mut()
            .unwrap()
            .get_type_at_range(file_path, range)
    }
}

impl Drop for RealLspClient {
    fn drop(&mut self) {
        let _ = self.process.kill();
    }
}
