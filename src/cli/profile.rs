use std::{
    env,
    ffi::c_char,
    fs,
    path::PathBuf,
    sync::{Arc, LazyLock},
    time::Instant,
};

use dashmap::DashMap;
use futures::future::join_all;
use tower_lsp::lsp_types::Url;
use tree_sitter::{Parser, Query, QueryCursor, StreamingIterator as _, ffi};
use ts_query_ls::Options;

use crate::{LanguageData, QUERY_LANGUAGE, handlers::did_open::init_language_data, util};

use super::get_scm_files;

static LANGUAGE_CACHE: LazyLock<DashMap<String, Arc<LanguageData>>> = LazyLock::new(DashMap::new);

static PATTERN_DEFINITION_QUERY: LazyLock<Query> =
    LazyLock::new(|| Query::new(&QUERY_LANGUAGE, "(program (definition) @def)").unwrap());

/// Measure the time of a query parse, without including potentially expensive logic specific to
/// the rust bindings
fn time_query_raw(language: *const tree_sitter::ffi::TSLanguage, source: &str) -> u128 {
    let mut error_offset = 0u32;
    let mut error_type: ffi::TSQueryError = 0;
    let now = Instant::now();
    let _ptr = unsafe {
        ffi::ts_query_new(
            language,
            source.as_bytes().as_ptr().cast::<c_char>(),
            source.len() as u32,
            core::ptr::addr_of_mut!(error_offset),
            core::ptr::addr_of_mut!(error_type),
        )
    };
    now.elapsed().as_millis()
}

pub async fn profile_directories(directories: &[PathBuf], config: String, broad: bool) {
    let Ok(options) = serde_json::from_str::<Options>(&config) else {
        eprintln!("Could not parse the provided configuration");
        return;
    };
    let scm_files = if directories.is_empty() {
        get_scm_files(&[env::current_dir().expect("Failed to get current directory")])
    } else {
        get_scm_files(directories)
    };
    let tasks = scm_files.into_iter().filter_map(|path| {
        let uri = Url::from_file_path(path.canonicalize().unwrap()).unwrap();
        let path_str = path.to_string_lossy().to_string();
        let language_name = util::get_language_name(&uri, &options);
        let language_data = language_name.and_then(|name| {
            LANGUAGE_CACHE.get(&name).as_deref().cloned().or_else(|| {
                util::get_language(&name, &options).map(|lang| Arc::new(init_language_data(lang)))
            })
        });
        if let Some(lang_data) = language_data {
            let lang = lang_data.language.clone().unwrap();
            if let Ok(source) = fs::read_to_string(&path) {
                Some(tokio::spawn(async move {
                    let mut results = Vec::new();
                    if broad {
                        results.push((
                            path_str.clone(),
                            1,
                            time_query_raw(lang.into_raw(), &source),
                        ));
                    } else {
                        let mut parser = Parser::new();
                        parser.set_language(&QUERY_LANGUAGE).unwrap();
                        let tree = parser.parse(&source, None).expect("Tree should exist");
                        let mut cursor = QueryCursor::new();
                        let source_bytes = source.as_bytes();
                        let mut matches = cursor.matches(
                            &PATTERN_DEFINITION_QUERY,
                            tree.root_node(),
                            source_bytes,
                        );
                        let lang_ptr = lang.into_raw();
                        while let Some(match_) = matches.next() {
                            for capture in match_.captures {
                                results.push((
                                    path_str.clone(),
                                    capture.node.start_position().row + 1,
                                    time_query_raw(
                                        lang_ptr,
                                        capture
                                            .node
                                            .utf8_text(source_bytes)
                                            .expect("Source should be UTF-8"),
                                    ),
                                ));
                            }
                        }
                    }
                    results
                }))
            } else {
                eprintln!("Failed to read {:?}", path.canonicalize().unwrap());
                None
            }
        } else {
            eprintln!(
                "Could not retrieve language for {:?}",
                path.canonicalize().unwrap()
            );
            None
        }
    });
    let results = join_all(tasks).await;
    let mut results = results
        .into_iter()
        .flat_map(|r| r.unwrap_or_default())
        .collect::<Vec<_>>();
    results.sort_unstable_by(|a, b| a.2.cmp(&b.2));
    for (path, row, time) in results {
        if broad {
            println!("Query at {path} took {time}ms to compile");
        } else {
            println!("Pattern in {path} at line {row} took {time}ms to compile");
        }
    }
}
