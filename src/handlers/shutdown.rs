use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::MessageType;

use crate::Backend;

pub async fn shutdown(backend: &Backend) -> Result<()> {
    backend
        .client
        .log_message(MessageType::LOG, "ts_query_ls shutdown".to_owned())
        .await;
    Ok(())
}
