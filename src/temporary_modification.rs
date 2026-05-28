use std::collections::HashMap;
use std::io;
use std::path::{Path, PathBuf};
use std::sync::{Mutex, OnceLock};

/// Handles temporary file modifications and ensures that the original file state is
/// restored when SIGINT is received (ctrl-c pressed).
pub struct TemporaryModification {
    path: PathBuf,
}

impl TemporaryModification {
    pub fn new(f: PathBuf, old: String, new: String) -> io::Result<Self> {
        let mut handler = CtrlCHandler::global().lock().unwrap();
        handler.files.insert(f.clone(), old);
        std::fs::write(&f, new)?;
        Ok(Self { path: f })
    }

    pub fn restore(self) {
        let mut handler = CtrlCHandler::global().lock().unwrap();
        let contents = handler.files.remove(&self.path).unwrap();
        restore_file(&self.path, contents);
    }
}

#[derive(Default)]
pub struct CtrlCHandler {
    files: HashMap<PathBuf, String>,
}

static CTRL_C_HANDLER: OnceLock<Mutex<CtrlCHandler>> = OnceLock::new();

impl CtrlCHandler {
    pub fn setup() {
        ctrlc::set_handler({
            move || {
                let mut lock = CtrlCHandler::global().lock().unwrap();
                lock.restore_all()
            }
        })
        .unwrap();
    }

    fn restore_all(&mut self) {
        for (path, original_code) in self.files.drain() {
            restore_file(&path, original_code);
        }
    }

    fn global<'a>() -> &'a Mutex<Self> {
        CTRL_C_HANDLER.get_or_init(|| Mutex::new(CtrlCHandler::default()))
    }
}

fn restore_file(path: &Path, original_code: String) {
    if let Err(e) = std::fs::write(path, original_code) {
        eprintln!("Failed to restore file {:?}: {}", path, e);
    }
}
