use ropey::Rope;
use tower_lsp::{
    jsonrpc::Result,
    lsp_types::{GotoDefinitionParams, GotoDefinitionResponse, Location, Position},
};
use tracing::{info, warn};
use tree_sitter::{Parser, QueryCursor, Tree};

use crate::{
    Backend, QUERY_LANGUAGE,
    util::{
        CAPTURES_QUERY, INHERITS_REGEX, NodeUtil, TextProviderRope, ToTsPoint,
        get_current_capture_node, get_file_uris, get_references, lsp_position_to_byte_offset,
        uri_to_basename,
    },
};

fn get_imported_module_under_cursor(
    rope: &Rope,
    tree: &Tree,
    position: &Position,
) -> Option<String> {
    if position.line != 0 {
        return None;
    }
    let ts_point = position.to_ts_point(rope);
    let current_node = tree
        .root_node()
        .descendant_for_point_range(ts_point, ts_point)
        .filter(|node| node.kind() == "comment")?;
    let node_text = current_node.text(rope);
    let modules = INHERITS_REGEX.captures(&node_text).and_then(|c| c.get(1))?;
    let cursor_offset = lsp_position_to_byte_offset(*position, rope).ok()?;
    let mut comment_offset = current_node.start_byte() + modules.start();

    modules.as_str().split(',').find_map(|module| {
        let end = comment_offset + module.len();
        let cursor_in_module = cursor_offset >= comment_offset && cursor_offset < end;
        comment_offset = end + 1;
        cursor_in_module.then(|| module.to_string())
    })
}

pub async fn goto_definition(
    backend: &Backend,
    params: GotoDefinitionParams,
) -> Result<Option<GotoDefinitionResponse>> {
    info!("ts_query_ls goto_definition: {params:?}");
    let uri = &params.text_document_position_params.text_document.uri;
    let Some(doc) = backend.document_map.get(uri) else {
        warn!("No document found for URI: {uri} when handling goto_definition");
        return Ok(None);
    };
    let rope = &doc.rope;
    let tree = &doc.tree;
    let cur_pos = params.text_document_position_params.position;

    if let (Some(module), Some(query_name)) = (
        get_imported_module_under_cursor(rope, tree, &cur_pos),
        uri_to_basename(uri),
    ) {
        let workspace_uris = backend.workspace_uris.read().unwrap().clone();
        let options = backend.options.read().await;
        let uris = get_file_uris(&workspace_uris, &options, &module, &query_name);
        if uris.is_empty() {
            return Ok(None);
        }

        return Ok(Some(
            uris.into_iter()
                .map(|uri| Location {
                    uri,
                    range: Default::default(),
                })
                .collect::<Vec<_>>()
                .into(),
        ));
    }

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
        GotoDefinitionParams, GotoDefinitionResponse, Location, PartialResultParams, Position,
        Range, TextDocumentIdentifier, TextDocumentPositionParams, Url, WorkDoneProgressParams,
        request::GotoDefinition,
    };

    use crate::test_helpers::helpers::{
        COMPLEX_FILE, SIMPLE_FILE, TEST_URI, initialize_server, lsp_request_to_jsonrpc_request,
        lsp_response_to_jsonrpc_response,
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
        let mut service = initialize_server(
            &[(
                TEST_URI.clone(),
                input,
                Vec::new(),
                Vec::new(),
                Vec::new(),
                Vec::new(),
            )],
            &Default::default(),
        )
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
        let actual = if locations.1.is_empty() {
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
        assert_eq!(
            refs,
            Some(lsp_response_to_jsonrpc_response::<GotoDefinition>(actual))
        );
    }
}
