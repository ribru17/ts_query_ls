use tower_lsp::lsp_types::DidCloseTextDocumentParams;
use tracing::{info, warn};

use crate::{Backend, LspClient};

pub fn did_close<C: LspClient>(backend: &Backend<C>, params: &DidCloseTextDocumentParams) {
    let uri = &params.text_document.uri;
    info!("ts_query_ls did_close: {uri}");
    if backend.document_map.remove(uri).is_none() {
        warn!("Document with URI: {uri} was not being tracked");
    }
}
