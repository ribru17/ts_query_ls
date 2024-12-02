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

#[cfg(test)]
mod test {
    use pretty_assertions::assert_eq;
    use rstest::rstest;
    use tower_lsp::lsp_types::{request::Initialize, InitializeResult, Url};

    use crate::{
        test_helpers::helpers::{
            initialize_server, lsp_response_to_jsonrpc_response, COMPLEX_FILE, SIMPLE_FILE,
            TEST_URI, TEST_URI_2,
        },
        SymbolInfo, SERVER_CAPABILITIES,
    };

    #[rstest]
    #[case(&[])]
    #[case(&[(
        TEST_URI.clone(),
        SIMPLE_FILE.clone(),
        Vec::new(),
        Vec::new(),
    ), (
        TEST_URI_2.clone(),
        COMPLEX_FILE.clone(),
        vec![
            SymbolInfo { named: true, label: String::from("identifier") },
            SymbolInfo { named: false, label: String::from(";") }
        ],
        vec![
            "operator",
            "content",
        ],
    )])]
    #[tokio::test(flavor = "current_thread")]
    async fn test_server_initialize(#[case] documents: &[(Url, &str, Vec<SymbolInfo>, Vec<&str>)]) {
        // Act
        let (service, response) = initialize_server(documents).await;

        // Assert
        let backend = service.inner();
        assert_eq!(
            response,
            lsp_response_to_jsonrpc_response::<Initialize>(InitializeResult {
                capabilities: SERVER_CAPABILITIES.clone(),
                ..Default::default()
            })
        );
        assert_eq!(backend.document_map.len(), documents.len());
        assert_eq!(backend.cst_map.len(), documents.len());
        assert_eq!(backend.symbols_vec_map.len(), documents.len());
        assert_eq!(backend.symbols_set_map.len(), documents.len());
        assert_eq!(backend.fields_vec_map.len(), documents.len());
        assert_eq!(backend.fields_set_map.len(), documents.len());
        for (uri, doc, symbols, fields) in documents {
            assert_eq!(
                backend.document_map.get(uri).unwrap().to_string(),
                (*doc).to_string()
            );
            assert_eq!(
                backend
                    .cst_map
                    .get(uri)
                    .unwrap()
                    .root_node()
                    .utf8_text((*doc).to_string().as_bytes())
                    .unwrap(),
                (*doc).to_string()
            );
            assert!(backend
                .symbols_vec_map
                .get(uri)
                .is_some_and(|v| v.len() == symbols.len()));
            assert!(backend
                .symbols_set_map
                .get(uri)
                .is_some_and(|v| v.len() == symbols.len()));
            for symbol in symbols {
                assert!(backend.symbols_vec_map.get(uri).unwrap().contains(symbol));
                assert!(backend.symbols_set_map.get(uri).unwrap().contains(symbol));
            }
            assert!(backend
                .fields_vec_map
                .get(uri)
                .is_some_and(|v| v.len() == fields.len()));
            assert!(backend
                .fields_set_map
                .get(uri)
                .is_some_and(|v| v.len() == fields.len()));
            for field in fields {
                assert!(backend
                    .fields_vec_map
                    .get(uri)
                    .unwrap()
                    .contains(&field.to_string()));
                assert!(backend
                    .fields_set_map
                    .get(uri)
                    .unwrap()
                    .contains(&field.to_string()));
            }
        }
    }
}
