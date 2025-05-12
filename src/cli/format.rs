use std::{
    fs,
    path::PathBuf,
    sync::{Arc, atomic::AtomicI32},
};

use futures::future::join_all;
use ropey::Rope;

use crate::{QUERY_LANGUAGE, handlers::formatting};

use super::get_scm_files;

pub async fn format_directories(directories: &[PathBuf], check: bool) -> i32 {
    if directories.is_empty() {
        eprintln!("No directories were specified to be formatted. No work was done.");
        return 1;
    }

    let scm_files = get_scm_files(directories);
    let exit_code = Arc::new(AtomicI32::new(0));

    let tasks = scm_files.into_iter().map(|path| {
        let exit_code = exit_code.clone();
        tokio::spawn(async move {
            if let Ok(contents) = fs::read_to_string(&path) {
                let mut parser = tree_sitter::Parser::new();
                parser
                    .set_language(&QUERY_LANGUAGE)
                    .expect("Error loading Query grammar");
                let tree = parser.parse(contents.as_str(), None).unwrap();
                let rope = Rope::from(contents.as_str());
                if let Some(formatted) = formatting::format_document(&rope, &tree.root_node()) {
                    if check {
                        let edits = formatting::diff(&contents, &formatted, &rope);
                        if !edits.is_empty() {
                            exit_code.store(1, std::sync::atomic::Ordering::Relaxed);
                            eprintln!(
                                "Improper formatting detected for {:?}",
                                path.canonicalize().unwrap()
                            );
                        }
                    } else if fs::write(&path, formatted).is_err() {
                        exit_code.store(1, std::sync::atomic::Ordering::Relaxed);
                        eprint!("Failed to write to {:?}", path.canonicalize().unwrap())
                    }
                }
            } else {
                eprintln!("Failed to read {:?}", path.canonicalize().unwrap());
                exit_code.store(1, std::sync::atomic::Ordering::Relaxed);
            }
        })
    });
    join_all(tasks).await;
    exit_code.load(std::sync::atomic::Ordering::Relaxed)
}
