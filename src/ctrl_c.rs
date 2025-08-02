use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::OnceLock;
use std::sync::mpsc::{Receiver, Sender, channel};
use std::time::Duration;
use std::{io, thread};

#[derive(Debug, thiserror::Error)]
#[error("Channel disconnected.")]
struct ChannelDisconnected;

#[derive(Debug)]
enum Command {
    RememberFileState(PathBuf, String),
    KeepFile(PathBuf),
    RestoreAll,
}

/// Handles temporary file modifications (for modifications with the
/// `--check` flag) and ensures that the original file state is
/// restored when SIGINT is received (ctrl-c pressed).
pub struct FileRestorer {
    sender: Sender<Command>,
}

static CTRL_C_HANDLER: OnceLock<FileRestorer> = OnceLock::new();

struct CtrlCHandlerThread {
    files: HashMap<PathBuf, String>,
    receiver: Receiver<Command>,
}

impl FileRestorer {
    fn new() -> Self {
        let (sender, receiver) = channel();
        thread::spawn(move || {
            let thread = CtrlCHandlerThread {
                files: HashMap::new(),
                receiver,
            };
            thread.start().unwrap();
        });
        ctrlc::set_handler({
            let sender = sender.clone();
            move || {
                sender.send(Command::RestoreAll).unwrap();
            }
        })
        .unwrap();
        Self { sender }
    }

    pub fn global() -> &'static FileRestorer {
        CTRL_C_HANDLER.get_or_init(|| FileRestorer::new())
    }

    pub fn remember_original_file_contents(
        &self,
        path: &Path,
        original_code: String,
    ) -> Result<(), io::Error> {
        self.sender
            .send(Command::RememberFileState(path.to_owned(), original_code))
            .unwrap();
        Ok(())
    }

    pub fn forget_file(&self, path: &Path) -> Result<(), io::Error> {
        self.sender
            .send(Command::KeepFile(path.to_owned()))
            .unwrap();
        Ok(())
    }
}

impl CtrlCHandlerThread {
    pub fn start(mut self) -> Result<(), ChannelDisconnected> {
        thread::spawn(move || -> Result<(), ChannelDisconnected> {
            loop {
                if let Some(command) = self.recv()? {
                    match command {
                        Command::RememberFileState(path, original_code) => {
                            self.files.insert(path, original_code);
                        }
                        Command::KeepFile(path) => {
                            self.files.remove(&path);
                        }
                        Command::RestoreAll => {
                            self.restore_all();
                            std::process::exit(130); // Standard exit code for SIGINT
                        }
                    }
                }
                thread::sleep(Duration::from_millis(10));
            }
        });
        Ok(())
    }

    fn recv(&self) -> Result<Option<Command>, ChannelDisconnected> {
        match self.receiver.try_recv() {
            Ok(c) => Ok(Some(c)),
            Err(std::sync::mpsc::TryRecvError::Empty) => Ok(None),
            Err(std::sync::mpsc::TryRecvError::Disconnected) => Err(ChannelDisconnected),
        }
    }

    fn restore_all(self) {
        for (path, original_code) in self.files.into_iter() {
            restore_file(&path, original_code);
        }
    }
}

fn restore_file(path: &Path, original_code: String) {
    if let Err(e) = std::fs::write(&path, original_code) {
        eprintln!("Failed to restore file {:?}: {}", path, e);
    }
}
