use std::{env::set_current_dir, path::PathBuf};

use log::{error, info};
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{InitializeParams, InitializeResult, ServerInfo};

use crate::util::set_configuration_options;
use crate::{Backend, SERVER_CAPABILITIES};

pub async fn initialize(backend: &Backend, params: InitializeParams) -> Result<InitializeResult> {
    info!("ts_query_ls initialize: {params:?}");
    if let Some(root_uri) = params.root_uri {
        let root = PathBuf::from(root_uri.path());
        if set_current_dir(&root).is_err() {
            error!("Failed to set root directory to {:?}", root);
        };
    }

    if let Some(init_options) = params.initialization_options {
        set_configuration_options(backend, init_options);
    }

    Ok(InitializeResult {
        capabilities: SERVER_CAPABILITIES.clone(),
        server_info: Some(ServerInfo {
            name: String::from("ts_query_ls"),
            version: Some(env!("CARGO_PKG_VERSION").to_string()),
        }),
    })
}

#[cfg(test)]
mod test {
    use pretty_assertions::assert_eq;
    use tower::{Service, ServiceExt};
    use tower_lsp::{
        lsp_types::{
            request::Initialize, ClientCapabilities, InitializeParams, InitializeResult,
            ServerInfo, Url,
        },
        LspService,
    };

    use crate::{
        test_helpers::helpers::{lsp_request_to_jsonrpc_request, lsp_response_to_jsonrpc_response},
        Backend, Options, SERVER_CAPABILITIES,
    };

    #[tokio::test(flavor = "current_thread")]
    async fn server_initialize() {
        // Arrange
        let (mut service, _socket) = LspService::build(|client| Backend {
            client,
            document_map: Default::default(),
            cst_map: Default::default(),
            symbols_set_map: Default::default(),
            symbols_vec_map: Default::default(),
            fields_set_map: Default::default(),
            fields_vec_map: Default::default(),
            supertype_map_map: Default::default(),
            options: Default::default(),
        })
        .finish();
        let options = r#"
            {
              "parser_aliases": {
                "ecma": "javascript",
                "jsx": "javascript",
                "foolang": "barlang"
              },
              "parser_install_directories": [
                "/my/directory/",
                "/tmp/tree-sitter/parsers/"
              ],
              "language_retrieval_patterns": [
                "\\.ts\\-([^/]+)\\-parser\\.wasm"
              ]
            }
        "#;

        // Act
        let init_result = service
            .ready()
            .await
            .unwrap()
            .call(lsp_request_to_jsonrpc_request::<Initialize>(
                InitializeParams {
                    capabilities: ClientCapabilities::default(),
                    root_uri: Some(Url::parse("file:///tmp/").unwrap()),
                    initialization_options: Some(serde_json::from_str(options).unwrap()),
                    ..Default::default()
                },
            ))
            .await
            .unwrap();

        // Assert
        assert_eq!(
            init_result,
            Some(lsp_response_to_jsonrpc_response::<Initialize>(
                InitializeResult {
                    capabilities: SERVER_CAPABILITIES.clone(),
                    server_info: Some(ServerInfo {
                        name: String::from("ts_query_ls"),
                        version: Some(String::from("1.6.1")),
                    }),
                }
            ))
        );
        let backend = service.inner();
        let actual_options = backend.options.read().unwrap();
        let expected_options = serde_json::from_str::<Options>(options).unwrap();
        assert_eq!(
            actual_options.parser_aliases,
            expected_options.parser_aliases
        );
        assert_eq!(
            actual_options.parser_install_directories,
            expected_options.parser_install_directories
        );
        assert_eq!(
            actual_options.language_retrieval_patterns,
            expected_options.language_retrieval_patterns
        );
    }
}
