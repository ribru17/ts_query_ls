use log::{info, warn};
use tower_lsp::{
    jsonrpc::Result,
    lsp_types::{GotoDefinitionParams, GotoDefinitionResponse, Location},
};
use tree_sitter::{Parser, Query, QueryCursor};

use crate::{
    util::{
        get_current_capture_node, get_references, lsp_position_to_ts_point,
        ts_node_to_lsp_location, TextProviderRope,
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
    let Some(rope) = backend.document_map.get(uri) else {
        warn!("No document built for URI: {uri:?}");
        return Ok(None);
    };
    let cur_pos = params.text_document_position_params.position;
    let Some(current_node) =
        get_current_capture_node(tree.root_node(), lsp_position_to_ts_point(cur_pos, &rope))
    else {
        return Ok(None);
    };

    let query = Query::new(&QUERY_LANGUAGE, "(capture) @cap").unwrap();
    let mut cursor = QueryCursor::new();
    let provider = TextProviderRope(&rope);

    let mut parser = Parser::new();
    parser
        .set_language(&QUERY_LANGUAGE)
        .expect("Error setting language for Query parser");

    let defs = get_references(
        &tree.root_node(),
        &current_node,
        &query,
        &mut cursor,
        &provider,
        &rope,
    )
    .filter(|node| node.parent().is_none_or(|p| p.kind() != "parameters"))
    .map(|node| ts_node_to_lsp_location(uri, &node, &rope))
    .collect::<Vec<Location>>();

    Ok(Some(GotoDefinitionResponse::Array(defs)))
}
