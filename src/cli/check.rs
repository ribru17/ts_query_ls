use std::{env, fs, path::PathBuf, sync::atomic::AtomicI32};

use rayon::iter::{IntoParallelRefIterator as _, ParallelIterator as _};
use tower_lsp::lsp_types::Url;
use tree_sitter::{Query, QueryErrorKind};
use ts_query_ls::Options;

use crate::util;

use super::{format::format_directories, get_scm_files, lint::lint_file};

pub fn check_directories(directories: &[PathBuf], config: String, format: bool, lint: bool) -> i32 {
    let Ok(options) = serde_json::from_str::<Options>(&config) else {
        eprintln!("Could not parse the provided configuration");
        return 1;
    };
    let exit_code = AtomicI32::new(0);
    // If directories are not specified, check all files in the current directory
    let scm_files = if directories.is_empty() {
        get_scm_files(&[env::current_dir().expect("Failed to get current directory")])
    } else {
        get_scm_files(directories)
    };
    scm_files.par_iter().for_each(|path| {
        let uri = Url::from_file_path(path.canonicalize().unwrap()).unwrap();
        if let Some(lang) = util::get_language(&uri, &options) {
            if let Ok(source) = fs::read_to_string(path) {
                if let Err(err) = Query::new(&lang, source.as_str()) {
                    match err.kind {
                        QueryErrorKind::Predicate => {
                            // Ignore predicate errors, which depend on the implementation.
                        }
                        _ => {
                            eprintln!("In {:?}:\n{}\n", path.canonicalize().unwrap(), err);
                            exit_code.store(1, std::sync::atomic::Ordering::Relaxed);
                        }
                    }
                }
                if lint {
                    lint_file(path, &uri, &source, &options, &exit_code);
                }
            } else {
                eprintln!("Failed to read {:?}", path.canonicalize().unwrap());
                exit_code.store(1, std::sync::atomic::Ordering::Relaxed);
            }
        } else {
            exit_code.store(1, std::sync::atomic::Ordering::Relaxed);
            eprintln!(
                "Could not retrieve language for {:?}",
                path.canonicalize().unwrap()
            )
        };
    });
    if format && format_directories(directories, true) != 0 {
        exit_code.store(1, std::sync::atomic::Ordering::Relaxed);
    }
    exit_code.load(std::sync::atomic::Ordering::Relaxed)
}
