use log::warn;
use tower_lsp::lsp_types::DidChangeConfigurationParams;

use crate::{Backend, Options};

pub async fn did_change_configuration(backend: &Backend, params: DidChangeConfigurationParams) {
    let Ok(changed_options) = serde_json::from_value::<Options>(params.settings) else {
        warn!("Unable to parse configuration settings!",);
        return;
    };
    let mut options = backend.options.write().unwrap();
    options.parser_install_directories = changed_options.parser_install_directories;
    options.parser_aliases = changed_options.parser_aliases;
    options.language_retrieval_patterns = changed_options.language_retrieval_patterns;
}
