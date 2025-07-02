use std::{
    env, fs,
    path::PathBuf,
    sync::{Arc, LazyLock},
    time::Instant,
};

use dashmap::DashMap;
use futures::future::join_all;
use tower_lsp::lsp_types::Url;
use tree_sitter::{Parser, Query, QueryCursor, StreamingIterator as _};
use ts_query_ls::Options;

use crate::{
    LanguageData, QUERY_LANGUAGE,
    handlers::did_open::init_language_data,
    util::{self, get_scm_files},
};

static LANGUAGE_CACHE: LazyLock<DashMap<String, Arc<LanguageData>>> = LazyLock::new(DashMap::new);

static PATTERN_DEFINITION_QUERY: LazyLock<Query> =
    LazyLock::new(|| Query::new(&QUERY_LANGUAGE, "(program (definition) @def)").unwrap());

pub async fn profile_directories(directories: &[PathBuf], config: String, per_file: bool) {
    let Ok(options) = serde_json::from_str::<Options>(&config) else {
        eprintln!("Could not parse the provided configuration");
        return;
    };
    let directories = if directories.is_empty() {
        &[env::current_dir().expect("Failed to get current directory")]
    } else {
        directories
    };
    let scm_files = get_scm_files(directories);
    let tasks = scm_files.into_iter().filter_map(|path| {
        let uri = Url::from_file_path(path.canonicalize().unwrap()).unwrap();
        let path_str = path.to_string_lossy().to_string();
        let language_name = util::get_language_name(&uri, &options);
        let language_data = language_name.and_then(|name| {
            LANGUAGE_CACHE.get(&name).as_deref().cloned().or_else(|| {
                util::get_language(&name, &options)
                    .map(|lang| Arc::new(init_language_data(lang, name)))
            })
        });
        let Some(lang_data) = language_data else {
            eprintln!(
                "Could not retrieve language for {:?}",
                path.canonicalize().unwrap()
            );
            return None;
        };
        let lang = lang_data.language.clone().unwrap();
        let Ok(source) = fs::read_to_string(&path) else {
            eprintln!("Failed to read {:?}", path.canonicalize().unwrap());
            return None;
        };
        Some(tokio::spawn(async move {
            if per_file {
                let now = Instant::now();
                let _ = Query::new(&lang, &source);
                return vec![(path_str.clone(), 1, now.elapsed().as_micros())];
            }

            let mut results = Vec::new();
            let mut parser = Parser::new();
            parser.set_language(&QUERY_LANGUAGE).unwrap();
            let tree = parser.parse(&source, None).expect("Tree should exist");
            let mut cursor = QueryCursor::new();
            let source_bytes = source.as_bytes();
            let mut matches =
                cursor.matches(&PATTERN_DEFINITION_QUERY, tree.root_node(), source_bytes);

            while let Some(match_) = matches.next() {
                for capture in match_.captures {
                    let now = Instant::now();
                    let _ = Query::new(
                        &lang,
                        capture
                            .node
                            .utf8_text(source_bytes)
                            .expect("Source should be UTF-8"),
                    );
                    results.push((
                        path_str.clone(),
                        capture.node.start_position().row + 1,
                        now.elapsed().as_micros(),
                    ));
                }
            }
            results
        }))
    });
    let results = join_all(tasks).await;
    let mut results = results
        .into_iter()
        .flat_map(|r| r.unwrap_or_default())
        .collect::<Vec<_>>();
    results.sort_unstable_by(|a, b| a.2.cmp(&b.2));
    for (path, row, time) in results {
        let time = format!("{:.2}ms", time as f64 / 1000.0);
        if per_file {
            println!("{time:<10} {path}");
        } else {
            println!("{time:<10} {path}:{row}");
        }
    }
}
