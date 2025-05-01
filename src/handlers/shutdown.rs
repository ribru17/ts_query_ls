use tower_lsp::jsonrpc::Result;
use tracing::info;

use crate::Backend;

pub async fn shutdown(_backend: &Backend) -> Result<()> {
    info!("ts_query_ls shutdown");
    Ok(())
}
