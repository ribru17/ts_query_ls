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
