use std::str::FromStr;

use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{
    InitializeParams, InitializeResult, PositionEncodingKind, ServerInfo, Url,
};
use tracing::info;

use crate::util::set_configuration_options;
use crate::{Backend, SERVER_CAPABILITIES};

pub async fn initialize(backend: &Backend, params: InitializeParams) -> Result<InitializeResult> {
    info!("ts_query_ls initialized");
    if let Ok(mut ws_uris) = backend.workspace_uris.write() {
        #[allow(deprecated)]
        if let Some(root_uri) = params.root_uri.or(params
            .root_path
            .and_then(|p| Url::from_str(p.as_str()).ok()))
        {
            ws_uris.push(root_uri);
        } else if let Some(ws_folders) = params.workspace_folders {
            ws_uris.extend(ws_folders.iter().map(|folder| folder.uri.clone()));
        }
    }

    let mut server_capabilities = SERVER_CAPABILITIES.clone();

    if params
        .capabilities
        .general
        .and_then(|cap| cap.position_encodings)
        .is_some_and(|encodings| encodings.contains(&PositionEncodingKind::UTF8))
    {
        server_capabilities.position_encoding = Some(PositionEncodingKind::UTF8);
    }

    set_configuration_options(
        backend,
        params.initialization_options,
        backend
            .workspace_uris
            .read()
            .map(|r| r.to_vec())
            .unwrap_or_default(),
    )
    .await;

    Ok(InitializeResult {
        capabilities: server_capabilities,
        server_info: Some(ServerInfo {
            name: String::from("ts_query_ls"),
            version: Some(env!("CARGO_PKG_VERSION").to_string()),
        }),
    })
}

#[cfg(test)]
mod test {
    use std::env;

    use pretty_assertions::assert_eq;
    use tower::{Service, ServiceExt};
    use tower_lsp::{
        LspService,
        lsp_types::{
            ClientCapabilities, InitializeParams, InitializeResult, ServerInfo, Url,
            request::Initialize,
        },
    };

    use crate::{
        Backend, Options, SERVER_CAPABILITIES,
        test_helpers::helpers::{lsp_request_to_jsonrpc_request, lsp_response_to_jsonrpc_response},
    };

    #[tokio::test(flavor = "current_thread")]
    async fn server_initialize() {
        // Arrange
        let (mut service, _socket) = LspService::build(|client| Backend {
            _client: client,
            document_map: Default::default(),
            language_map: Default::default(),
            workspace_uris: Default::default(),
            options: Default::default(),
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
                        version: Some(String::from("3.3.0")),
                    }),
                }
            ))
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
