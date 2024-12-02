use std::cmp::Ordering;

use log::warn;
use regex::Regex;
use tower_lsp::{
    jsonrpc::{self, Result},
    lsp_types::{
        DocumentChanges, OneOf, OptionalVersionedTextDocumentIdentifier, RenameParams,
        TextDocumentEdit, TextEdit, WorkspaceEdit,
    },
};
use tree_sitter::{Query, QueryCursor};

use crate::{
    util::{
        get_current_capture_node, get_references, lsp_position_to_ts_point,
        ts_node_to_lsp_location, TextProviderRope,
    },
    Backend, QUERY_LANGUAGE,
};

pub async fn rename(backend: &Backend, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
    let uri = params.text_document_position.text_document.uri;
    let Some(tree) = backend.cst_map.get(&uri) else {
        warn!("No CST built for URI: {uri:?}");
        return Ok(None);
    };
    let Some(rope) = backend.document_map.get(&uri) else {
        warn!("No document built for URI: {uri:?}");
        return Ok(None);
    };
    let current_node = match get_current_capture_node(
        tree.root_node(),
        lsp_position_to_ts_point(params.text_document_position.position, &rope),
    ) {
        None => return Ok(None),
        Some(value) => value,
    };
    let query = Query::new(&QUERY_LANGUAGE, "(capture) @cap").unwrap();
    let mut cursor = QueryCursor::new();
    // Allow the new name to begin with "@"
    let new_name = params
        .new_name
        .strip_prefix('@')
        .unwrap_or(params.new_name.as_str());
    let identifier_pattern = Regex::new(r"^[a-zA-Z0-9.\-_\$]+$").unwrap();
    if !identifier_pattern.is_match(new_name) {
        return Err(jsonrpc::Error::invalid_params(
            "New name is not a valid identifier",
        ));
    }
    let mut text_document_edits: Vec<TextDocumentEdit> = vec![];
    let provider = TextProviderRope(&rope);
    get_references(
        &tree.root_node(),
        &current_node,
        &query,
        &mut cursor,
        &provider,
        &rope,
    )
    .map(|node| ts_node_to_lsp_location(&uri, &node, &rope))
    .for_each(|mut elem| {
        // Don't include the preceding `@`
        elem.range.start.character += 1;
        text_document_edits.push(TextDocumentEdit {
            text_document: OptionalVersionedTextDocumentIdentifier {
                uri: elem.uri,
                // TODO: Support versioned edits
                version: None,
            },
            edits: vec![OneOf::Left(TextEdit {
                range: elem.range,
                new_text: new_name.to_owned(),
            })],
        });
    });
    // Apply edits from end to start, to prevent offset inaccuracies
    text_document_edits.sort_by(|a, b| {
        if let OneOf::Left(a) = &a.edits[0] {
            if let OneOf::Left(b) = &b.edits[0] {
                let range_a = a.range;
                let range_b = b.range;
                range_b.start.cmp(&range_a.start)
            } else {
                Ordering::Equal
            }
        } else {
            Ordering::Equal
        }
    });

    Ok(Some(WorkspaceEdit {
        document_changes: Some(DocumentChanges::Edits(text_document_edits)),
        changes: None,
        change_annotations: None,
    }))
}
