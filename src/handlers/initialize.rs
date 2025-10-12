use std::str::FromStr;

use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{InitializeParams, InitializeResult, ServerInfo, Url};
use tracing::info;

use crate::util::set_configuration_options;
use crate::{Backend, LspClient, SERVER_CAPABILITIES};

pub async fn initialize<C: LspClient>(
    backend: &Backend<C>,
    params: InitializeParams,
) -> Result<InitializeResult> {
    info!("ts_query_ls initialized");
    if let Ok(mut ws_uris) = backend.workspace_paths.write() {
        #[allow(deprecated)]
        if let Some(ws_folders) = params.workspace_folders {
            ws_uris.extend(
                ws_folders
                    .into_iter()
                    .filter_map(|folder| folder.uri.to_file_path().ok()),
            );
        } else if let Some(root_uri) = params
            .root_uri
            .or_else(|| {
                params
                    .root_path
                    .and_then(|p| Url::from_str(p.as_str()).ok())
            })
            .and_then(|uri| uri.to_file_path().ok())
        {
            ws_uris.push(root_uri);
        }
    }

    {
        let mut client_capabilities = backend.client_capabilities.write().await;
        *client_capabilities = params.capabilities;
    }

    set_configuration_options(
        backend,
        params.initialization_options,
        backend
            .workspace_paths
            .read()
            .map(|r| r.to_vec())
            .unwrap_or_default(),
    )
    .await;

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
    use std::{env, sync::Arc};

    use dashmap::DashMap;
    use pretty_assertions::assert_eq;
    use tower_lsp::{
        LspService,
        lsp_types::{
            ClientCapabilities, InitializeParams, InitializeResult, ServerInfo, Url,
            request::Initialize,
        },
    };

    use crate::{
        Backend, Options, SERVER_CAPABILITIES,
        test_helpers::helpers::{MockClient, TestService},
    };

    #[tokio::test(flavor = "current_thread")]
    async fn server_initialize() {
        // Arrange
        let (mut service, _socket) = LspService::build(|_client| Backend {
            client: MockClient::default(),
            client_capabilities: Arc::default(),
            document_map: DashMap::default(),
            language_map: DashMap::default(),
            workspace_paths: Arc::default(),
            dependents: DashMap::default(),
            options: Arc::default(),
        })
        .finish();
        unsafe { env::set_var("HOME", "/home/jdoe") };
        let options = r#"
            {
              "parser_aliases": {
                "ecma": "javascript",
                "jsx": "javascript",
                "foolang": "barlang"
              },
              "parser_install_directories": [
                "${HOME}/my/directory/",
                "/$tmp/tree-sitter/parsers/"
              ],
              "language_retrieval_patterns": [
                "\\.ts\\-([^/]+)\\-parser\\.wasm"
              ],
              "valid_captures": {
                "highlights": {
                  "variable": "Simple identifiers",
                  "variable.parameter": "Parameters of functions"
                }
              }
            }
        "#;

        // Act
        let init_result = service
            .request::<Initialize>(InitializeParams {
                capabilities: ClientCapabilities::default(),
                root_uri: Some(Url::parse("file:///tmp/").unwrap()),
                initialization_options: Some(serde_json::from_str(options).unwrap()),
                ..Default::default()
            })
            .await;

        // Assert
        assert_eq!(
            init_result,
            InitializeResult {
                capabilities: SERVER_CAPABILITIES.clone(),
                server_info: Some(ServerInfo {
                    name: String::from("ts_query_ls"),
                    version: Some(String::from("3.11.1")),
                }),
            }
        );
        let backend = service.inner();
        let actual_options = backend.options.read().await;
        let mut expected_options = serde_json::from_str::<Options>(options).unwrap();
        // Test that env vars are correctly substituted
        expected_options.parser_install_directories = vec![
            String::from("/home/jdoe/my/directory/"),
            String::from("/$tmp/tree-sitter/parsers/"),
        ];
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
        assert_eq!(
            actual_options.valid_captures,
            expected_options.valid_captures
        );
    }
}
