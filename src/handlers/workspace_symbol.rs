use std::fs;

use ropey::Rope;
use tower_lsp::{
    jsonrpc::Result,
    lsp_types::{Location, SymbolInformation, SymbolKind, Url, WorkspaceSymbolParams},
};
use tree_sitter::{Parser, QueryCursor, StreamingIterator};

use crate::{
    Backend, QUERY_LANGUAGE,
    util::{CAPTURES_QUERY, NodeUtil, TextProviderRope, get_scm_files, is_subsequence},
};

pub async fn symbol(
    backend: &Backend,
    params: WorkspaceSymbolParams,
) -> Result<Option<Vec<SymbolInformation>>> {
    let mut symbols = Vec::new();
    let query = params.query;

    let dirs = backend
        .workspace_uris
        .read()
        .as_deref()
        .cloned()
        .unwrap_or_default()
        .into_iter()
        .filter_map(|uri| uri.to_file_path().ok())
        .collect::<Vec<_>>();

    let mut parser = Parser::new();
    parser
        .set_language(&QUERY_LANGUAGE)
        .expect("Language should load");

    for path in get_scm_files(&dirs) {
        if let (Ok(content), Ok(uri)) = (fs::read_to_string(&path), Url::from_file_path(path)) {
            let rope = &Rope::from_str(&content);
            let tree = parser.parse(&content, None).expect("Tree should exist");

            let provider = &TextProviderRope(rope);
            let mut cursor = QueryCursor::new();
            let mut matches = cursor.matches(&CAPTURES_QUERY, tree.root_node(), provider);

            while let Some(match_) = matches.next() {
                for capture in match_.captures {
                    let capture_node = capture.node;
                    let node_text = capture_node.text(rope);

                    if is_subsequence(&query, &node_text) {
                        symbols.push(SymbolInformation {
                            name: node_text,
                            kind: SymbolKind::VARIABLE,
                            location: Location::new(uri.clone(), capture_node.lsp_range(rope)),
                            container_name: None,
                            tags: None,
                            #[allow(deprecated)]
                            deprecated: None,
                        });
                    }
                }
            }
        }
    }

    Ok(Some(symbols))
}
