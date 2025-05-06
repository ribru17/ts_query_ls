use std::{fs, path::PathBuf, sync::atomic::AtomicI32};

use rayon::iter::{IntoParallelRefIterator as _, ParallelIterator as _};
use ropey::Rope;

use crate::{QUERY_LANGUAGE, handlers::formatting};

use super::get_scm_files;

pub fn format_directories(directories: &[PathBuf], check: bool) -> i32 {
    if directories.is_empty() {
        eprintln!("No directories were specified to be formatted. No work was done.");
        return 1;
    }

    let scm_files = get_scm_files(directories);
    let exit_code = AtomicI32::new(0);

    scm_files.par_iter().for_each(|path| {
        if let Ok(contents) = fs::read_to_string(path) {
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
                } else if fs::write(path, formatted).is_err() {
                    exit_code.store(1, std::sync::atomic::Ordering::Relaxed);
                    eprint!("Failed to write to {:?}", path.canonicalize().unwrap())
                }
            }
        } else {
            eprintln!("Failed to read {:?}", path.canonicalize().unwrap());
            exit_code.store(1, std::sync::atomic::Ordering::Relaxed);
        }
    });
    exit_code.load(std::sync::atomic::Ordering::Relaxed)
}
