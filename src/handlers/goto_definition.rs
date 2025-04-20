use tower_lsp::{
    jsonrpc::Result,
    lsp_types::{GotoDefinitionParams, GotoDefinitionResponse, Location},
};
use tracing::{info, warn};
use tree_sitter::{Parser, QueryCursor};

use crate::{
    util::{
        get_current_capture_node, get_references, NodeUtil, TextProviderRope, ToTsPoint,
        CAPTURES_QUERY,
    },
    Backend, QUERY_LANGUAGE,
};

pub async fn goto_definition(
    backend: &Backend,
    params: GotoDefinitionParams,
) -> Result<Option<GotoDefinitionResponse>> {
    info!("ts_query_ls goto_definition: {params:?}");
    let uri = &params.text_document_position_params.text_document.uri;
    let Some(tree) = backend.cst_map.get(uri) else {
        warn!("No CST built for URI: {uri:?}");
        return Ok(None);
    };
    let Some(rope) = &backend.document_map.get(uri) else {
        warn!("No document built for URI: {uri:?}");
        return Ok(None);
    };
    let cur_pos = params.text_document_position_params.position;
    let Some(current_node) = get_current_capture_node(tree.root_node(), cur_pos.to_ts_point(rope))
    else {
        return Ok(None);
    };

    let query = &CAPTURES_QUERY;
    let mut cursor = QueryCursor::new();
    let provider = TextProviderRope(rope);

    let mut parser = Parser::new();
    parser
        .set_language(&QUERY_LANGUAGE)
        .expect("Error setting language for Query parser");

    let defs = get_references(
        &tree.root_node(),
        &current_node,
        query,
        &mut cursor,
        &provider,
        rope,
    )
    .filter(|node| node.parent().is_none_or(|p| p.kind() != "parameters"))
    .map(|node| Location {
        uri: uri.clone(),
        range: node.lsp_range(rope),
    })
    .collect::<Vec<Location>>();

    Ok(Some(GotoDefinitionResponse::Array(defs)))
}

#[cfg(test)]
mod test {
    use pretty_assertions::assert_eq;
    use rstest::rstest;
    use tower::{Service, ServiceExt};
    use tower_lsp::lsp_types::{
        request::GotoDefinition, GotoDefinitionParams, GotoDefinitionResponse, Location,
        PartialResultParams, Position, Range, TextDocumentIdentifier, TextDocumentPositionParams,
        WorkDoneProgressParams,
    };

    use crate::test_helpers::helpers::{
        initialize_server, lsp_request_to_jsonrpc_request, lsp_response_to_jsonrpc_response,
        COMPLEX_FILE, SIMPLE_FILE, TEST_URI,
    };

    type Coordinate = ((u32, u32), (u32, u32));

    #[rstest]
    #[case(
        &SIMPLE_FILE,
        Position { line: 0, character: 4 },
        &[]
    )]
    #[case(
        &SIMPLE_FILE,
        Position { line: 0, character: 20 },
        &[((0, 14), (0, 23))]
    )]
    #[case(
        &COMPLEX_FILE,
        Position { line: 12, character: 14 },
        &[((8, 24), (8, 42)), ((9, 22), (9, 40))]
    )]
    #[tokio::test(flavor = "current_thread")]
    async fn goto_definition(
        #[case] input: &str,
        #[case] position: Position,
        #[case] locations: &[Coordinate],
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
            .call(lsp_request_to_jsonrpc_request::<GotoDefinition>(
                GotoDefinitionParams {
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
        let actual = if locations.is_empty() {
            None
        } else {
            Some(GotoDefinitionResponse::Array(
                locations
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
            ))
        };
        assert_eq!(
            refs,
            Some(lsp_response_to_jsonrpc_response::<GotoDefinition>(actual))
        );
    }
}
