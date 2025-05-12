use std::{
    env, fs,
    path::PathBuf,
    sync::{Arc, LazyLock, atomic::AtomicI32},
};

use dashmap::DashMap;
use futures::future::join_all;
use tower_lsp::lsp_types::Url;
use ts_query_ls::Options;

use crate::{LanguageData, handlers::did_open::init_language_data, util};

use super::{format::format_directories, get_scm_files, lint::lint_file};

static LANGUAGE_CACHE: LazyLock<DashMap<String, Arc<LanguageData>>> = LazyLock::new(DashMap::new);

pub async fn check_directories(directories: &[PathBuf], config: String, format: bool) -> i32 {
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
        let language_data = language_name.and_then(|name| {
            LANGUAGE_CACHE.get(&name).as_deref().cloned().or_else(|| {
                util::get_language(&name, &options).map(|lang| Arc::new(init_language_data(lang)))
            })
        });
        if let Some(lang) = language_data {
            if let Ok(source) = fs::read_to_string(&path) {
                return Some(tokio::spawn(async move {
                    lint_file(
                        &path,
                        &uri,
                        &source,
                        options_arc.clone(),
                        Some(lang),
                        &exit_code,
                    )
                    .await;
                }));
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
