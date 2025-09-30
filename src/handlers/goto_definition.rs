use tower_lsp::lsp_types::{GotoDefinitionParams, GotoDefinitionResponse, Location, Range};
use tracing::{info, warn};
use tree_sitter::QueryCursor;

use crate::{
    Backend, LspClient,
    util::{
        CAPTURES_QUERY, NodeUtil, PosUtil, TextProviderRope, get_current_capture_node,
        get_imported_module_under_cursor, get_references,
    },
};

pub fn goto_definition<C: LspClient>(
    backend: &Backend<C>,
    params: &GotoDefinitionParams,
) -> Option<GotoDefinitionResponse> {
    info!("ts_query_ls goto_definition: {params:?}");
    let uri = &params.text_document_position_params.text_document.uri;
    let Some(doc) = backend.document_map.get(uri) else {
        warn!("No document found for URI: {uri} when handling goto_definition");
        return None;
    };
    let rope = &doc.rope;
    let tree = &doc.tree;
    let cur_pos = params.text_document_position_params.position;

    if let Some(module) = get_imported_module_under_cursor(&doc, cur_pos) {
        return module.uri.clone().map(|uri| {
            Location {
                uri,
                range: Range::default(),
            }
            .into()
        });
    }

    let current_node = get_current_capture_node(tree.root_node(), cur_pos.to_ts_point(rope))?;

    let query = &CAPTURES_QUERY;
    let mut cursor = QueryCursor::new();
    let provider = TextProviderRope(rope);

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

    Some(GotoDefinitionResponse::Array(defs))
}

#[cfg(test)]
mod test {
    use pretty_assertions::assert_eq;
    use rstest::rstest;
    use tower_lsp::lsp_types::{
        GotoDefinitionParams, GotoDefinitionResponse, Location, PartialResultParams, Position,
        Range, TextDocumentIdentifier, TextDocumentPositionParams, Url, WorkDoneProgressParams,
        request::GotoDefinition,
    };

    use crate::{
        Options,
        test_helpers::helpers::{
            COMPLEX_FILE, SIMPLE_FILE, TEST_URI, TestService, initialize_server,
        },
    };

    type Coordinate = ((u32, u32), (u32, u32));

    #[rstest]
    #[case(
        &SIMPLE_FILE,
        Position { line: 0, character: 4 },
        (TEST_URI.clone(), [].as_slice())
    )]
    #[case(
        &SIMPLE_FILE,
        Position { line: 0, character: 20 },
        (TEST_URI.clone(), [((0, 14), (0, 23))].as_slice())
    )]
    #[case(
        &COMPLEX_FILE,
        Position { line: 12, character: 14 },
        (TEST_URI.clone(), [((8, 24), (8, 42)), ((9, 22), (9, 40))].as_slice())
    )]
    #[tokio::test(flavor = "current_thread")]
    async fn goto_definition(
        #[case] input: &str,
        #[case] position: Position,
        #[case] locations: (Url, &[Coordinate]),
    ) {
        // Arrange
        let mut service =
            initialize_server(&[(TEST_URI.clone(), input)], &Options::default()).await;

        // Act
        let refs = service
            .request::<GotoDefinition>(GotoDefinitionParams {
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
            })
            .await;

        // Assert
        let expected = if locations.1.is_empty() {
            None
        } else {
            Some(GotoDefinitionResponse::Array(
                locations
                    .1
                    .iter()
                    .map(|r| Location {
                        uri: locations.0.clone(),
                        range: Range {
                            start: Position {
                                line: r.0.0,
                                character: r.0.1,
                            },
                            end: Position {
                                line: r.1.0,
                                character: r.1.1,
                            },
                        },
                    })
                    .collect(),
            ))
        };
        assert_eq!(expected, refs);
    }
}
