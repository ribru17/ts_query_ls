use std::fs;

use ropey::Rope;
use tower_lsp::{
    jsonrpc::Result,
    lsp_types::{Location, SymbolInformation, SymbolKind, Url, WorkspaceSymbolParams},
};
use tree_sitter::{QueryCursor, StreamingIterator};

use crate::{
    Backend,
    util::{CAPTURES_QUERY, NodeUtil, TextProviderRope, get_scm_files, is_subsequence, parse},
};

pub async fn symbol(
    backend: &Backend,
    params: WorkspaceSymbolParams,
) -> Result<Option<Vec<SymbolInformation>>> {
    let mut symbols = Vec::new();
    let query = params.query;

    let dirs = backend
        .workspace_paths
        .read()
        .as_deref()
        .cloned()
        .unwrap_or_default();

    for path in get_scm_files(&dirs) {
        let container_name = path
            .file_name()
            .and_then(|f| f.to_str())
            .map(ToString::to_string);
        if let (Ok(content), Ok(uri)) = (fs::read_to_string(&path), Url::from_file_path(path)) {
            let rope = &Rope::from_str(&content);
            let tree = parse(rope, None);

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
                            container_name: container_name.clone(),
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

#[cfg(test)]
mod test {
    use pretty_assertions::assert_eq;
    use tower_lsp::lsp_types::{
        Location, PartialResultParams, Position, Range, SymbolInformation, SymbolKind, Url,
        WorkDoneProgressParams, WorkspaceSymbolParams, WorkspaceSymbolResponse,
        request::WorkspaceSymbolRequest,
    };

    use crate::test_helpers::helpers::{TestService, initialize_server};

    #[tokio::test(flavor = "current_thread")]
    async fn document_symbol() {
        // Arrange
        let mut service = initialize_server(&[], &Default::default()).await;
        fn make_range(start_line: u32, start_col: u32, end_line: u32, end_col: u32) -> Range {
            Range::new(
                Position::new(start_line, start_col),
                Position::new(end_line, end_col),
            )
        }

        // Act
        let actual_tokens = service
            .request::<WorkspaceSymbolRequest>(WorkspaceSymbolParams {
                query: String::from(""),
                partial_result_params: PartialResultParams::default(),
                work_done_progress_params: WorkDoneProgressParams::default(),
            })
            .await;

        // Assert
        let cpp_folds_uri = Url::from_file_path(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/tests/fixtures/test_workspace/queries/cpp/folds.scm"
        ))
        .unwrap();
        let other_highlights_uri = Url::from_file_path(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/tests/fixtures/test_workspace/queries/other/highlights.scm"
        ))
        .unwrap();
        #[allow(deprecated)]
        let expected_tokens = Some(WorkspaceSymbolResponse::Flat(vec![
            SymbolInformation {
                name: String::from("@fold.region"),
                kind: SymbolKind::VARIABLE,
                location: Location {
                    uri: cpp_folds_uri.clone(),
                    range: make_range(0, 22, 0, 34),
                },
                deprecated: None,
                tags: None,
                container_name: Some(String::from("folds.scm")),
            },
            SymbolInformation {
                name: String::from("@fold.imports"),
                kind: SymbolKind::VARIABLE,
                location: Location {
                    uri: cpp_folds_uri.clone(),
                    range: make_range(2, 20, 2, 33),
                },
                deprecated: None,
                tags: None,
                container_name: Some(String::from("folds.scm")),
            },
            SymbolInformation {
                name: String::from("@variable"),
                kind: SymbolKind::VARIABLE,
                location: Location {
                    uri: other_highlights_uri.clone(),
                    range: make_range(5, 2, 5, 11),
                },
                deprecated: None,
                tags: None,
                container_name: Some(String::from("highlights.scm")),
            },
            SymbolInformation {
                name: String::from("@function"),
                kind: SymbolKind::VARIABLE,
                location: Location {
                    uri: other_highlights_uri.clone(),
                    range: make_range(7, 11, 7, 20),
                },
                deprecated: None,
                tags: None,
                container_name: Some(String::from("highlights.scm")),
            },
            SymbolInformation {
                name: String::from("@constant"),
                kind: SymbolKind::VARIABLE,
                location: Location {
                    uri: other_highlights_uri.clone(),
                    range: make_range(13, 2, 13, 11),
                },
                deprecated: None,
                tags: None,
                container_name: Some(String::from("highlights.scm")),
            },
        ]));
        assert_eq!(expected_tokens, actual_tokens);
    }
}
