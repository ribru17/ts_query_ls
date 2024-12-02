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
        .map(|node| ts_node_to_lsp_location(uri, &node, &rope))
        .collect(),
    ))
}
