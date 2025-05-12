use std::{
    env, fs,
    path::PathBuf,
    sync::{Arc, atomic::AtomicI32},
};

use futures::future::join_all;
use tower_lsp::lsp_types::Url;
use tree_sitter::{Query, QueryErrorKind};
use ts_query_ls::Options;

use crate::util;

use super::{format::format_directories, get_scm_files, lint::lint_file};

pub async fn check_directories(
    directories: &[PathBuf],
    config: String,
    format: bool,
    lint: bool,
) -> i32 {
    let Ok(options) = serde_json::from_str::<Options>(&config) else {
        eprintln!("Could not parse the provided configuration");
        return 1;
    };
    let options_arc: Arc<tokio::sync::RwLock<Options>> = Arc::new(options.clone().into());

    let exit_code = Arc::new(AtomicI32::new(0));
    // If directories are not specified, check all files in the current directory
    let scm_files = if directories.is_empty() {
        get_scm_files(&[env::current_dir().expect("Failed to get current directory")])
    } else {
        get_scm_files(directories)
    };
    let tasks = scm_files.into_iter().filter_map(|path| {
        let options_arc = options_arc.clone();
        let exit_code = exit_code.clone();
        let uri = Url::from_file_path(path.canonicalize().unwrap()).unwrap();
        let language_name = util::get_language_name(&uri, &options);
        if let Some(lang) = language_name.and_then(|name| util::get_language(&name, &options)) {
            if let Ok(source) = fs::read_to_string(&path) {
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
                    return Some(tokio::spawn(async move {
                        lint_file(&path, &uri, &source, options_arc.clone(), &exit_code).await;
                    }));
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
        None
    });
    join_all(tasks).await;
    if format && format_directories(directories, true).await != 0 {
        exit_code.store(1, std::sync::atomic::Ordering::Relaxed);
    }
    exit_code.load(std::sync::atomic::Ordering::Relaxed)
}
