use tower_lsp::lsp_types::DidChangeConfigurationParams;

use crate::{Backend, LspClient, util::set_configuration_options};

pub async fn did_change_configuration<C: LspClient>(
    backend: &Backend<C>,
    params: DidChangeConfigurationParams,
) {
    set_configuration_options(
        backend,
        Some(params.settings),
        backend
            .workspace_paths
            .read()
            .map(|uris| uris.to_vec())
            .unwrap_or_default(),
    )
    .await;
}

#[cfg(test)]
mod test {
    use pretty_assertions::assert_eq;
    use regex::Regex;
    use std::collections::BTreeMap;
    use tower_lsp::lsp_types::{
        DidChangeConfigurationParams, notification::DidChangeConfiguration,
    };

    use crate::{
        Options,
        test_helpers::helpers::{TestService, initialize_server},
    };

    #[tokio::test(flavor = "current_thread")]
    async fn server_did_change_configuration() {
        // Arrange
        let mut service = initialize_server(&[], &Options::default()).await;

        // Act
        service
            .notify::<DidChangeConfiguration>(DidChangeConfigurationParams {
                settings: serde_json::from_str(
                    r#"
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
                    "#,
                )
                .unwrap(),
            })
            .await;

        // Assert
        let options = service.inner().options.read().await;
        assert_eq!(
            *options,
            Options {
                parser_aliases: BTreeMap::from([
                    ("ecma".to_string(), "javascript".to_string()),
                    ("jsx".to_string(), "javascript".to_string()),
                    ("foolang".to_string(), "barlang".to_string())
                ]),
                parser_install_directories: vec![
                    String::from("/my/directory/"),
                    String::from("/tmp/tree-sitter/parsers/"),
                ],
                language_retrieval_patterns: vec![
                    Regex::new(r"\.ts\-([^/]+)\-parser\.wasm").unwrap().into(),
                    Regex::new("queries/([^/]+)/[^/]+\\.scm$").unwrap().into(),
                    Regex::new("tree-sitter-([^/]+)/queries/[^/]+\\.scm$",)
                        .unwrap()
                        .into(),
                ],
                ..Default::default()
            }
        );
    }
}
