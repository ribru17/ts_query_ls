use log::warn;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{Location, ReferenceParams};
use tree_sitter::{Parser, Query, QueryCursor};

use crate::{
    util::{
        get_current_capture_node, get_references, lsp_position_to_ts_point,
        ts_node_to_lsp_location, TextProviderRope,
    },
    Backend, QUERY_LANGUAGE,
};

pub async fn references(
    backend: &Backend,
    params: ReferenceParams,
) -> Result<Option<Vec<Location>>> {
    let uri = &params.text_document_position.text_document.uri;

    let Some(tree) = backend.cst_map.get(uri) else {
        warn!("No CST built for URI: {uri:?}");
        return Ok(None);
    };
    let Some(rope) = backend.document_map.get(uri) else {
        warn!("No document built for URI: {uri:?}");
        return Ok(None);
    };
    let cur_pos = lsp_position_to_ts_point(params.text_document_position.position, &rope);
    let current_node = match get_current_capture_node(tree.root_node(), cur_pos) {
        None => return Ok(None),
        Some(value) => value,
    };

    let include_def = params.context.include_declaration;
    let query = Query::new(&QUERY_LANGUAGE, "(capture) @cap").unwrap();
    let mut cursor = QueryCursor::new();
    let provider = TextProviderRope(&rope);

    let mut parser = Parser::new();
    parser
        .set_language(&QUERY_LANGUAGE)
        .expect("Error setting language for Query parser");

    Ok(Some(
        get_references(
            &tree.root_node(),
            &current_node,
            &query,
            &mut cursor,
            &provider,
            &rope,
        )
        .filter_map(|node| {
            if include_def || node.parent().is_some_and(|p| p.kind() == "parameters") {
                Some(ts_node_to_lsp_location(uri, &node, &rope))
            } else {
                None
            }
        })
        .collect(),
    ))
}

#[cfg(test)]
mod test {
    use pretty_assertions::assert_eq;
    use rstest::rstest;
    use tower::{Service, ServiceExt};
    use tower_lsp::lsp_types::{
        request::References, Location, PartialResultParams, Position, Range, ReferenceContext,
        ReferenceParams, TextDocumentIdentifier, TextDocumentPositionParams,
        WorkDoneProgressParams,
    };

    use crate::test_helpers::helpers::{
        initialize_server, lsp_request_to_jsonrpc_request, lsp_response_to_jsonrpc_response,
        COMPLEX_FILE, TEST_URI,
    };

    type Coordinate = ((u32, u32), (u32, u32));

    #[rstest]
    #[case(
        "(identifier) @variable",
        Position { line: 0, character: 17 },
        true,
        &[((0, 13), (0, 22))]
    )]
    #[case(
        r#"((identifier) @constant
(#match? @constant "^[A-Z][A-Z\\d_]*$"))"#,
        Position { line: 0, character: 17 },
        true,
        &[((0, 14), (0, 23)), ((1, 9), (1, 18))]
    )]
    #[case(
        r"(type_definition declarator: (type_identifier) @name) @definition.type",
        Position { line: 0, character: 61 },
        true,
        &[((0, 54), (0, 70))]
    )]
    #[case(
        r"(call_expression
function: (identifier) @function)",
        Position { line: 0, character: 1 },
        true,
        &[]
    )]
    #[case(
        &COMPLEX_FILE,
        Position { line: 5, character: 25 },
        true,
        &[((5, 25), (5, 44)), ((11, 15), (11, 34)), ((17, 16), (17, 35))]
    )]
    #[case(
        &COMPLEX_FILE,
        Position { line: 12, character: 13 },
        false,
        &[((12, 12), (12, 30)), ((18, 16), (18, 34))]
    )]
    #[tokio::test(flavor = "current_thread")]
    async fn capture_references(
        #[case] input: &str,
        #[case] position: Position,
        #[case] include_declaration: bool,
        #[case] ranges: &[Coordinate],
    ) {
        // Arrange
        let mut service =
            initialize_server(&[(TEST_URI.clone(), input, Vec::new(), Vec::new())]).await;

        // Act
        let refs = service
            .ready()
            .await
            .unwrap()
            .call(lsp_request_to_jsonrpc_request::<References>(
                ReferenceParams {
                    context: ReferenceContext {
                        include_declaration,
                    },
                    partial_result_params: PartialResultParams {
                        partial_result_token: None,
                    },
                    work_done_progress_params: WorkDoneProgressParams::default(),
                    text_document_position: TextDocumentPositionParams {
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
        let actual = if ranges.is_empty() {
            None
        } else {
            Some(
                ranges
                    .iter()
                    .map(|r| Location {
                        uri: TEST_URI.clone(),
                        range: Range {
                            start: Position {
                                line: r.0 .0,
                                character: r.0 .1,
                            },
                            end: Position {
                                line: r.1 .0,
                                character: r.1 .1,
                            },
                        },
                    })
                    .collect(),
            )
        };
        assert_eq!(
            refs,
            Some(lsp_response_to_jsonrpc_response::<References>(actual))
        );
    }
}
