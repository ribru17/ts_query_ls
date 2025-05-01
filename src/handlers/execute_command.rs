use serde_json::Value;
use tower_lsp::{
    jsonrpc::Result,
    lsp_types::{Diagnostic, DiagnosticSeverity, ExecuteCommandParams, Position, Range, Url},
};
use tracing::{error, warn};
use tree_sitter::{Point, Query, QueryError, QueryErrorKind};

use crate::{
    Backend,
    handlers::diagnostic::get_diagnostics,
    util::{self, NodeUtil as _, TextProviderRope},
};

pub async fn execute_command(
    backend: &Backend,
    params: ExecuteCommandParams,
) -> Result<Option<Value>> {
    if params.command == "ts-query-ls.checkImpossiblePatterns" {
        check_impossible_patterns(backend, params).await;
    }
    Ok(None)
}

async fn check_impossible_patterns(backend: &Backend, params: ExecuteCommandParams) {
    let uri = match params
        .arguments
        .first()
        .map(|v| serde_json::from_value::<Url>(v.clone()))
    {
        Some(Ok(uri)) => uri,
        arg => {
            error!("Invalid/missing argument '{arg:?}' for check impossible patterns command");
            return;
        }
    };

    let Some(doc) = &backend.document_map.get(&uri) else {
        warn!("No document built for URI '{uri}' when executing check impossible patterns command");
        return;
    };
    let rope = &doc.rope;
    let tree = &doc.tree;
    let options = &backend.options.read().await;
    let Some(lang) = util::get_language(&uri, options) else {
        warn!("Could not retrieve language for path: '{}'", uri.path());
        return;
    };
    let Ok(source) = std::fs::read_to_string(uri.path()) else {
        error!("Failed to read file: '{}'", uri.path());
        return;
    };

    let mut diagnostics = Vec::new();
    if let Err(err) = Query::new(&lang, source.as_str()) {
        match err.kind {
            // Ignore predicate errors, which depend on the implementation.
            QueryErrorKind::Predicate => {}
            _ => {
                let range = if err.message.is_empty() {
                    tree.root_node().lsp_range(rope)
                } else {
                    let point = Point::new(err.row, err.column);
                    tree.root_node()
                        .named_descendant_for_point_range(point, point)
                        .map_or_else(
                            || {
                                let pos = Position::new(err.row as u32, err.column as u32);
                                Range {
                                    start: pos,
                                    end: pos,
                                }
                            },
                            |node| node.lsp_range(rope),
                        )
                };
                diagnostics.push(Diagnostic {
                    range,
                    severity: Some(DiagnosticSeverity::ERROR),
                    source: Some("ts-query-ls".to_string()),
                    message: query_error_message(&err),
                    ..Default::default()
                })
            }
        }
    }
    let provider = &TextProviderRope(rope);
    diagnostics.append(&mut get_diagnostics(&uri, doc, options, provider));

    backend
        .client
        .publish_diagnostics(uri, diagnostics, None)
        .await;
}

// Because `QueryError`'s `Display` implementation includes the error's one-based error
// location, we need to slightly modify it to remove the duplicated/incorrect information.
fn query_error_message(err: &QueryError) -> String {
    let msg = match err.kind {
        QueryErrorKind::Field => "Invalid field name ",
        QueryErrorKind::NodeType => "Invalid node type ",
        QueryErrorKind::Capture => "Invalid capture name ",
        QueryErrorKind::Predicate => "Invalid predicate: ",
        QueryErrorKind::Structure => "Impossible pattern:\n",
        QueryErrorKind::Syntax => "Invalid syntax:\n",
        QueryErrorKind::Language => "",
    };
    if msg.is_empty() {
        err.message.to_string()
    } else {
        format!("{msg}{}", err.message)
    }
}
