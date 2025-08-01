use tower_lsp::jsonrpc::Result;
use tracing::info;

use crate::{Backend, LspClient};

pub async fn shutdown<C: LspClient>(_backend: &Backend<C>) -> Result<()> {
    info!("ts_query_ls shutdown");
    Ok(())
}
