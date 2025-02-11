use log::{error, warn};
use tower_lsp::{
    jsonrpc::Result,
    lsp_types::{DocumentSymbol, DocumentSymbolParams, DocumentSymbolResponse, SymbolKind},
};
use tree_sitter::{Query, QueryCursor, StreamingIterator};

use crate::{
    util::{get_node_text, ts_node_to_lsp_range, TextProviderRope},
    Backend, QUERY_LANGUAGE,
};

pub async fn document_symbol(
    backend: &Backend,
    params: DocumentSymbolParams,
) -> Result<Option<DocumentSymbolResponse>> {
    let uri = &params.text_document.uri;
    let mut document_symbols = vec![];

    let (Some(rope), Some(tree)) = (&backend.document_map.get(uri), backend.cst_map.get(uri))
    else {
        warn!("No document found for URI: {uri} when searching for document symbols.");
        return Ok(None);
    };

    let provider = &TextProviderRope(rope);
    let mut cursor = QueryCursor::new();
    let query = Query::new(&QUERY_LANGUAGE, "(capture) @cap").unwrap();
    let mut matches = cursor.matches(&query, tree.root_node(), provider);

    while let Some(match_) = matches.next() {
        for capture in match_.captures {
            let capture_node = capture.node;
            let node_text = get_node_text(&capture_node, rope);
            let Some(parent) = capture_node.parent() else {
                error!("Error parsing capture {node_text}");
                continue;
            };
            #[allow(deprecated)]
            document_symbols.push(DocumentSymbol {
                name: node_text,
                kind: SymbolKind::VARIABLE,
                range: ts_node_to_lsp_range(&parent, rope),
                selection_range: ts_node_to_lsp_range(&capture_node, rope),
                detail: None,
                // TODO: Structure this hierarchically
                children: None,
                tags: None,
                deprecated: None,
            });
        }
    }

    Ok(Some(DocumentSymbolResponse::Nested(document_symbols)))
}

#[cfg(test)]
mod test {
    use pretty_assertions::assert_eq;
    use rstest::rstest;
    use tower::{Service, ServiceExt};
    use tower_lsp::lsp_types::{
        request::DocumentSymbolRequest, DocumentSymbol, DocumentSymbolParams,
        DocumentSymbolResponse, PartialResultParams, Position, Range, SymbolKind,
        TextDocumentIdentifier, WorkDoneProgressParams,
    };

    use crate::test_helpers::helpers::{
        initialize_server, lsp_request_to_jsonrpc_request, lsp_response_to_jsonrpc_response,
        SIMPLE_FILE, TEST_URI,
    };

    type DocSymbol = (String, Range, Range);

    #[rstest]
    #[case(SIMPLE_FILE.clone(), vec![(
        String::from("@constant"),
        Range {
            start: Position {
                line: 0,
                character: 14
            },
            end: Position {
                line: 0,
                character: 23
            }
        },
        Range {
            start: Position {
                line: 0,
                character: 1
            },
            end: Position {
                line: 0,
                character: 23
            }
        },
    ), (
        String::from("@constant"),
        Range {
            start: Position {
                line: 1,
                character: 10
            },
            end: Position {
                line: 1,
                character: 19
            }
        },
        Range {
            start: Position {
                line: 1,
                character: 10
            },
            end: Position {
                line: 1,
                character: 29
            }
        },
    ), (
        String::from("@constant"),
        Range {
            start: Position {
                line: 1,
                character: 20
            },
            end: Position {
                line: 1,
                character: 29
            }
        },
        Range {
            start: Position {
                line: 1,
                character: 10
            },
            end: Position {
                line: 1,
                character: 29
            }
        },
    )])]
    #[tokio::test(flavor = "current_thread")]
    async fn document_symbol(#[case] source: &str, #[case] symbols: Vec<DocSymbol>) {
        // Arrange
        let mut service =
            initialize_server(&[(TEST_URI.clone(), source, Vec::new(), Vec::new(), Vec::new())])
                .await;

        // Act
        let tokens = service
            .ready()
            .await
            .unwrap()
            .call(lsp_request_to_jsonrpc_request::<DocumentSymbolRequest>(
                DocumentSymbolParams {
                    text_document: TextDocumentIdentifier {
                        uri: TEST_URI.clone(),
                    },
                    partial_result_params: PartialResultParams::default(),
                    work_done_progress_params: WorkDoneProgressParams::default(),
                },
            ))
            .await
            .unwrap();

        // Assert
        let actual = Some(DocumentSymbolResponse::Nested(
            #[allow(deprecated)]
            symbols
                .iter()
                .map(|s| DocumentSymbol {
                    name: s.0.clone(),
                    selection_range: s.1,
                    range: s.2,
                    kind: SymbolKind::VARIABLE,
                    detail: None,
                    children: None,
                    tags: None,
                    deprecated: None,
                })
                .collect(),
        ));
        assert_eq!(
            tokens,
            Some(lsp_response_to_jsonrpc_response::<DocumentSymbolRequest>(
                actual
            ))
        );
    }
}
