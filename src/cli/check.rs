use std::{
    env, fs,
    path::PathBuf,
    sync::{Arc, LazyLock, atomic::AtomicI32},
};

use dashmap::DashMap;
use futures::future::join_all;
use tower_lsp::lsp_types::Url;
use ts_query_ls::Options;

use crate::{
    LanguageData,
    handlers::did_open::init_language_data,
    util::{self, get_scm_files},
};

use super::{format::format_directories, lint::lint_file};

static LANGUAGE_CACHE: LazyLock<DashMap<String, Arc<LanguageData>>> = LazyLock::new(DashMap::new);

pub async fn check_directories(
    directories: &[PathBuf],
    config: String,
    workspace: Option<PathBuf>,
    format: bool,
    fix: bool,
) -> i32 {
    let Ok(options) = serde_json::from_str::<Options>(&config) else {
        eprintln!("Could not parse the provided configuration");
        return 1;
    };
    let options_arc: Arc<tokio::sync::RwLock<Options>> = Arc::new(options.clone().into());

    let exit_code = Arc::new(AtomicI32::new(0));
    // If directories are not specified, check all files in the current directory
    let directories = if directories.is_empty() {
        &[env::current_dir().expect("Failed to get current directory")]
    } else {
        directories
    };
    let workspace = workspace
        .unwrap_or(env::current_dir().expect("Failed to get current directory"))
        .canonicalize()
        .expect("Workspace path should be valid");
    let workspace = Arc::new(workspace);
    let scm_files = get_scm_files(directories);
    let tasks = scm_files.into_iter().filter_map(|path| {
        let options_arc = options_arc.clone();
        let exit_code = exit_code.clone();
        let absolute_path = path.canonicalize().expect("Path should be valid");
        let uri = Url::from_file_path(&absolute_path).expect("Path should be absolute");
        let language_name = util::get_language_name(&uri, &options);
        let language_data = language_name.and_then(|name| {
            LANGUAGE_CACHE.get(&name).as_deref().cloned().or_else(|| {
                util::get_language(&name, &options)
                    .map(|lang| Arc::new(init_language_data(lang, name)))
            })
        });
        let Ok(source) = fs::read_to_string(&path) else {
            eprintln!("Failed to read {absolute_path:?}");
            exit_code.store(1, std::sync::atomic::Ordering::Relaxed);
            return None;
        };
        let workspace = workspace.clone();
        let ignore_missing_language = false;
        Some(tokio::spawn(async move {
            if let Some(new_source) = lint_file(
                &path,
                &workspace,
                &uri,
                &source,
                options_arc.clone(),
                fix,
                ignore_missing_language,
                language_data,
                &exit_code,
            )
            .await
                && fs::write(&path, new_source).is_err()
            {
                eprintln!("Failed to write {absolute_path:?}");
                exit_code.store(1, std::sync::atomic::Ordering::Relaxed);
            };
        }))
    });
    join_all(tasks).await;
    if format && format_directories(directories, true).await != 0 {
        exit_code.store(1, std::sync::atomic::Ordering::Relaxed);
    }
    exit_code.load(std::sync::atomic::Ordering::Relaxed)
}
