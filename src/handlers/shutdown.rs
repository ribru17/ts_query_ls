use tracing::info;

use crate::{Backend, LspClient};

pub fn shutdown<C: LspClient>(_backend: &Backend<C>) {
    info!("ts_query_ls shutdown");
}
