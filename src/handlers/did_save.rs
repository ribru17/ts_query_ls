use tower_lsp::lsp_types::DidSaveTextDocumentParams;
use tracing::info;

use crate::{Backend, LspClient};

pub fn did_save<C: LspClient>(_: &Backend<C>, params: DidSaveTextDocumentParams) {
    let uri = params.text_document.uri;
    info!("ts_query_ls saved document with URI: {uri}");
}
