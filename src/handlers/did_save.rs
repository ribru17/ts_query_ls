use tower_lsp::lsp_types::DidSaveTextDocumentParams;
use tracing::info;

use crate::Backend;

pub async fn did_save(_: &Backend, params: DidSaveTextDocumentParams) {
    let uri = params.text_document.uri;
    info!("ts_query_ls saved document with URI: {uri}");
}
