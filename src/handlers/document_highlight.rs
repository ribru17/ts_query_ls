use log::warn;
use streaming_iterator::StreamingIterator;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{DocumentHighlight, DocumentHighlightKind, DocumentHighlightParams};
use tree_sitter::{Parser, Query, QueryCursor};

use crate::util::{
    get_node_text, get_references, lsp_position_to_ts_point, ts_node_to_lsp_range, TextProviderRope,
};
use crate::{Backend, QUERY_LANGUAGE};

pub async fn document_highlight(
    backend: &Backend,
    params: DocumentHighlightParams,
) -> Result<Option<Vec<DocumentHighlight>>> {
    let uri = &params.text_document_position_params.text_document.uri;

    let Some(tree) = backend.cst_map.get(uri) else {
        warn!("No CST built for URI: {uri:?}");
        return Ok(None);
    };
    let Some(rope) = backend.document_map.get(uri) else {
        warn!("No document built for URI: {uri:?}");
        return Ok(None);
    };
    let cur_pos = lsp_position_to_ts_point(params.text_document_position_params.position, &rope);

    // Get the current node: if we are in a capture's identifier, move the current node to the
    // entire capture
    let current_node = tree
        .root_node()
        .named_descendant_for_point_range(cur_pos, cur_pos)
        .map(|node| {
            node.parent()
                .filter(|p| p.kind() == "capture")
                .unwrap_or(node)
        })
        .unwrap();

    let capture_query = Query::new(&QUERY_LANGUAGE, "(capture) @cap").unwrap();
    let ident_query = Query::new(&QUERY_LANGUAGE, "(identifier) @name").unwrap();
    let mut cursor = QueryCursor::new();
    let provider = TextProviderRope(&rope);

    let mut parser = Parser::new();
    parser
        .set_language(&QUERY_LANGUAGE)
        .expect("Error setting language for Query parser");

    if current_node.kind() == "capture" {
        Ok(Some(
            get_references(
                &tree.root_node(),
                &current_node,
                &capture_query,
                &mut cursor,
                &provider,
                &rope,
            )
            .map(|node| DocumentHighlight {
                kind: if node.parent().is_none_or(|p| p.kind() != "parameters") {
                    Some(DocumentHighlightKind::WRITE)
                } else {
                    Some(DocumentHighlightKind::READ)
                },
                range: ts_node_to_lsp_range(&node, &rope),
            })
            .collect(),
        ))
    } else if current_node.kind() == "identifier" {
        Ok(Some(
            cursor
                .matches(&ident_query, tree.root_node(), &provider)
                .map_deref(|match_| {
                    match_.captures.iter().filter_map(|cap| {
                        if cap.node.parent()?.kind() == current_node.parent()?.kind()
                            && get_node_text(&cap.node, &rope)
                                == get_node_text(&current_node, &rope)
                        {
                            return Some(cap.node);
                        }
                        None
                    })
                })
                .flatten()
                .map(|node| DocumentHighlight {
                    kind: Some(DocumentHighlightKind::TEXT),
                    range: ts_node_to_lsp_range(&node, &rope),
                })
                .collect(),
        ))
    } else {
        Ok(None)
    }
}

#[cfg(test)]
mod test {
    use pretty_assertions::assert_eq;
    use rstest::rstest;
    use tower::{Service, ServiceExt};
    use tower_lsp::lsp_types::{
        request::DocumentHighlightRequest, DocumentHighlight, DocumentHighlightKind,
        DocumentHighlightParams, PartialResultParams, Position, Range, TextDocumentIdentifier,
        TextDocumentPositionParams, WorkDoneProgressParams,
    };

    use crate::test_helpers::helpers::{
        initialize_server, lsp_request_to_jsonrpc_request, lsp_response_to_jsonrpc_response,
        COMPLEX_FILE, TEST_URI,
    };

    type Highlight = (DocumentHighlightKind, (u32, u32), (u32, u32));

    #[rstest]
    #[case(
        "(identifier) @variable",
        Position { line: 0, character: 0 },
        &[]
    )]
    #[case(
        "(identifier) @variable",
        Position { line: 0, character: 17 },
        &[(DocumentHighlightKind::WRITE, (0, 13), (0, 22))]
    )]
    #[case(
        r#"((identifier) @constant
(#match? @constant "^[A-Z][A-Z\\d_]*$"))

(boolean) @constant"#,
        Position { line: 0, character: 17 },
        &[
            (DocumentHighlightKind::WRITE, (0, 14), (0, 23)),
            (DocumentHighlightKind::READ, (1, 9), (1, 18)),
        ]
    )]
    #[case(
        r"(variable) @variable

(variable
  (type_specifier)) @variable.typed",
        Position { line: 1, character: 6 },
        &[
            (DocumentHighlightKind::TEXT, (0, 1), (0, 9)),
            (DocumentHighlightKind::TEXT, (2, 1), (2, 9)),
        ]
    )]
    #[case(
        r"expression: (number) @number

expression: (boolean) @boolean",
        Position { line: 2, character: 2 },
        &[
            (DocumentHighlightKind::TEXT, (0, 0), (0, 10)),
            (DocumentHighlightKind::TEXT, (2, 0), (2, 10)),
        ]
    )]
    #[case(
        r"expression: (number) @number

expression: (boolean) @boolean",
        Position { line: 0, character: 10 },
        &[]
    )]
    #[case(
        &COMPLEX_FILE,
        Position { line: 1, character: 5 },
        &[
            (DocumentHighlightKind::TEXT, (1, 4), (1, 7)),
            (DocumentHighlightKind::TEXT, (13, 4), (13, 7)),
            (DocumentHighlightKind::TEXT, (30, 4), (30, 7)),
            (DocumentHighlightKind::TEXT, (31, 4), (31, 7)),
        ]
    )]
    #[case(
        &COMPLEX_FILE,
        Position { line: 13, character: 5 },
        &[
            (DocumentHighlightKind::TEXT, (1, 4), (1, 7)),
            (DocumentHighlightKind::TEXT, (13, 4), (13, 7)),
            (DocumentHighlightKind::TEXT, (30, 4), (30, 7)),
            (DocumentHighlightKind::TEXT, (31, 4), (31, 7)),
        ]
    )]
    #[tokio::test(flavor = "current_thread")]
    async fn document_highlight(
        #[case] input: &str,
        #[case] position: Position,
        #[case] highlights: &[Highlight],
    ) {
        // Arrange
        let mut service =
            initialize_server(&[(TEST_URI.clone(), input, Vec::new(), Vec::new(), Vec::new())])
                .await;

        // Act
        let refs = service
            .ready()
            .await
            .unwrap()
            .call(lsp_request_to_jsonrpc_request::<DocumentHighlightRequest>(
                DocumentHighlightParams {
                    partial_result_params: PartialResultParams {
                        partial_result_token: None,
                    },
                    work_done_progress_params: WorkDoneProgressParams::default(),
                    text_document_position_params: TextDocumentPositionParams {
                        text_document: TextDocumentIdentifier {
                            uri: TEST_URI.clone(),
                        },
                        position,
                    },
                },
            ))
            .await
            .unwrap();

        // Assert
        let actual = if highlights.is_empty() {
            None
        } else {
            Some(
                highlights
                    .iter()
                    .map(|(kind, p0, p1)| DocumentHighlight {
                        kind: Some(*kind),
                        range: Range {
                            start: Position {
                                line: p0.0,
                                character: p0.1,
                            },
                            end: Position {
                                line: p1.0,
                                character: p1.1,
                            },
                        },
                    })
                    .collect(),
            )
        };
        assert_eq!(
            refs,
            Some(lsp_response_to_jsonrpc_response::<DocumentHighlightRequest>(actual))
        );
    }
}
