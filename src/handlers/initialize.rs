use std::{env::set_current_dir, path::PathBuf};

use log::{error, info};
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{InitializeParams, InitializeResult};

use crate::{Backend, SERVER_CAPABILITIES};

pub async fn initialize(_backend: &Backend, params: InitializeParams) -> Result<InitializeResult> {
    info!("ts_query_ls initialize: {params:?}");
    if let Some(root_uri) = params.root_uri {
        let root = PathBuf::from(root_uri.path());
        if set_current_dir(&root).is_err() {
            error!("Failed to set root directory to {:?}", root);
        };
    }

    Ok(InitializeResult {
        capabilities: SERVER_CAPABILITIES.clone(),
        ..Default::default()
    })
}
