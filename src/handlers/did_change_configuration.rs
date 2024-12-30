use tower_lsp::lsp_types::DidChangeConfigurationParams;

use crate::{util::set_configuration_options, Backend};

pub async fn did_change_configuration(backend: &Backend, params: DidChangeConfigurationParams) {
    set_configuration_options(backend, params.settings);
}

#[cfg(test)]
mod test {
    use pretty_assertions::assert_eq;
    use std::collections::BTreeMap;
    use tower::{Service, ServiceExt};
    use tower_lsp::lsp_types::{
        notification::DidChangeConfiguration, DidChangeConfigurationParams,
    };

    use crate::{
        test_helpers::helpers::{initialize_server, lsp_notification_to_jsonrpc_request},
        Options,
    };

    #[tokio::test(flavor = "current_thread")]
    async fn server_did_change_configuration() {
        // Arrange
        let mut service = initialize_server(&[]).await.0;

        // Act
        service
            .ready()
            .await
            .unwrap()
            .call(
                lsp_notification_to_jsonrpc_request::<DidChangeConfiguration>(
                    DidChangeConfigurationParams {
                        settings: serde_json::from_str(
                            r#"
                    {
                      "parser_aliases": {
                        "ecma": "javascript",
                        "jsx": "javascript",
                        "foolang": "barlang"
                      }
                    }
                    "#,
                        )
                        .unwrap(),
                    },
                ),
            )
            .await
            .unwrap();

        // Assert
        let options = service.inner().options.read();
        assert!(options.is_ok());
        let options = options.unwrap();
        assert_eq!(
            *options,
            Options {
                parser_aliases: Some(BTreeMap::from([
                    ("ecma".to_string(), "javascript".to_string()),
                    ("jsx".to_string(), "javascript".to_string()),
                    ("foolang".to_string(), "barlang".to_string())
                ])),
                parser_install_directories: None,
                language_retrieval_patterns: None
            }
        );
    }
}
