use flate2::read::GzDecoder;
use std::fs;
use std::path::{Path, PathBuf};
use tar::Archive;
use tempfile::TempDir;

use crate::RustFile;
use crate::input::{Contents, Input, MoltSource};

#[test]
#[ignore]
fn parse_syn() {
    parse_crate("syn", "2.0.87")
}

#[test]
#[ignore]
fn parse_tokio() {
    parse_crate("tokio", "1.41.1")
}

#[test]
#[ignore]
fn parse_clap() {
    parse_crate("clap", "4.5.21")
}

#[test]
#[ignore]
fn parse_rayon() {
    parse_crate("rayon", "1.10.0")
}

fn url(name: &str, version: &str) -> String {
    format!("https://crates.io/api/v1/crates/{name}/{version}/download")
}

fn folder_name(name: &str, version: &str) -> String {
    format!("{name}-{version}")
}

fn parse_crate(name: &str, version: &str) {
    let temp_dir = TempDir::new().expect("Failed to create temp directory");
    let temp_path = temp_dir.path();

    let response = reqwest::blocking::get(url(name, version)).expect("Failed to download crate");
    let bytes = response.bytes().expect("Failed to get response bytes");

    let tar_gz = GzDecoder::new(bytes.as_ref());
    let mut archive = Archive::new(tar_gz);
    archive
        .unpack(temp_path)
        .expect("Failed to extract archive");

    let syn_dir = temp_path.join(folder_name(name, version));
    assert!(syn_dir.exists());

    let rust_files = find_rust_files(&syn_dir);
    assert!(!rust_files.is_empty());

    let mut input = Input::new(MoltSource::FromCli(Contents::new("".to_string())));
    for rust_file_path in rust_files {
        input = input.with_rust_src_file(&rust_file_path).unwrap();
    }
    for rust_file_id in input.iter_rust_src() {
        RustFile::new(&input, rust_file_id).unwrap();
    }
}

fn find_rust_files(dir: &Path) -> Vec<PathBuf> {
    let entries = fs::read_dir(dir).unwrap();
    entries
        .into_iter()
        .flat_map(|entry| {
            let entry = entry.unwrap();
            let path = entry.path();
            if path.is_dir() {
                find_rust_files(&path).into_iter()
            } else if path.extension().is_some_and(|ext| ext == "rs") {
                vec![path.to_owned()].into_iter()
            } else {
                vec![].into_iter()
            }
        })
        .collect()
}
