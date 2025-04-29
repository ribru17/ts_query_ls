use tower_lsp::{
    jsonrpc::Result,
    lsp_types::{CodeActionOrCommand, CodeActionParams, CodeActionResponse},
};

use crate::Backend;

pub async fn code_action(
    backend: &Backend,
    params: CodeActionParams,
) -> Result<Option<CodeActionResponse>> {
    let uri = &params.text_document.uri;

    if !backend.document_map.contains_key(uri) {
        Ok(None)
    } else {
        Ok(Some(vec![CodeActionOrCommand::Command(
            tower_lsp::lsp_types::Command {
                title: "Impossible Pattern Check".to_string(),
                command: "ts-query-ls.checkImpossiblePatterns".to_string(),
                arguments: Some(vec![serde_json::to_value(uri).unwrap()]),
            },
        )]))
    }
}
