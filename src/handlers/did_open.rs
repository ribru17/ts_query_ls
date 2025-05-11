use std::collections::{BTreeSet, HashMap, HashSet};

use ropey::Rope;
use tower_lsp::lsp_types::DidOpenTextDocumentParams;
use tracing::info;
use tree_sitter::Parser;

use crate::{
    Backend, DocumentData, QUERY_LANGUAGE, SymbolInfo,
    util::{get_language, get_language_name},
};

pub async fn did_open(backend: &Backend, params: DidOpenTextDocumentParams) {
    let uri = &params.text_document.uri;
    info!("ts_query_ls did_open: {uri}");
    let contents = params.text_document.text;
    let rope = Rope::from_str(&contents);
    let mut parser = Parser::new();
    parser
        .set_language(&QUERY_LANGUAGE)
        .expect("Error loading Query grammar");
    let tree = parser.parse(&contents, None).unwrap();

    // Initialize language info
    let mut symbols_vec: Vec<SymbolInfo> = vec![];
    let mut symbols_set: HashSet<SymbolInfo> = HashSet::new();
    let mut fields_vec: Vec<String> = vec![];
    let mut fields_set: HashSet<String> = HashSet::new();
    let mut supertype_map: HashMap<SymbolInfo, BTreeSet<SymbolInfo>> = HashMap::new();
    let options = backend.options.read().await;
    let language_name = get_language_name(uri, &options);
    if let Some(lang) = language_name.and_then(|name| get_language(&name, &options)) {
        let error_symbol = SymbolInfo {
            label: "ERROR".to_owned(),
            named: true,
        };
        symbols_set.insert(error_symbol.clone());
        symbols_vec.push(error_symbol);
        for i in 0..lang.node_kind_count() as u16 {
            let supertype = lang.node_kind_is_supertype(i);
            let named = lang.node_kind_is_named(i) || supertype;
            let label = if named {
                lang.node_kind_for_id(i).unwrap().to_owned()
            } else {
                lang.node_kind_for_id(i)
                    .unwrap()
                    .replace('\\', r"\\")
                    .replace('"', r#"\""#)
                    .replace('\n', r"\n")
            };
            let symbol_info = SymbolInfo { label, named };
            if supertype {
                supertype_map.insert(
                    symbol_info.clone(),
                    lang.subtypes_for_supertype(i)
                        .iter()
                        .map(|s| SymbolInfo {
                            label: lang.node_kind_for_id(*s).unwrap().to_string(),
                            named: lang.node_kind_is_named(*s) || lang.node_kind_is_supertype(*s),
                        })
                        .collect(),
                );
            }
            if symbols_set.contains(&symbol_info) || !(lang.node_kind_is_visible(i) || supertype) {
                continue;
            }
            symbols_set.insert(symbol_info.clone());
            symbols_vec.push(symbol_info);
        }
        // Field IDs go from 1 to nfields inclusive (extra index 0 maps to NULL)
        for i in 1..=lang.field_count() as u16 {
            let field_name = lang.field_name_for_id(i).unwrap().to_owned();
            if !fields_set.contains(&field_name) {
                fields_set.insert(field_name.clone());
                fields_vec.push(field_name);
            }
        }
    }

    let version = params.text_document.version;

    backend.document_map.insert(
        uri.clone(),
        DocumentData {
            rope,
            tree,
            symbols_set,
            symbols_vec,
            fields_set,
            fields_vec,
            supertype_map,
            version,
        },
    );
}

#[cfg(test)]
mod test {
    use pretty_assertions::assert_eq;
    use tower::{Service, ServiceExt};
    use tower_lsp::lsp_types::{
        DidOpenTextDocumentParams, TextDocumentItem, notification::DidOpenTextDocument,
    };

    use crate::test_helpers::helpers::{
        TEST_URI, initialize_server, lsp_notification_to_jsonrpc_request,
    };

    #[tokio::test(flavor = "current_thread")]
    async fn server_did_open_document() {
        // Arrange
        let mut service = initialize_server(&[], &Default::default()).await;
        let source = r#""[" @cap"#;

        // Act
        service
            .ready()
            .await
            .unwrap()
            .call(lsp_notification_to_jsonrpc_request::<DidOpenTextDocument>(
                DidOpenTextDocumentParams {
                    text_document: TextDocumentItem {
                        uri: TEST_URI.clone(),
                        language_id: String::from("query"),
                        version: 0,
                        text: String::from(source),
                    },
                },
            ))
            .await
            .unwrap();

        // Assert
        let doc = service.inner().document_map.get(&TEST_URI).unwrap();
        let doc_rope = &doc.rope;
        assert_eq!(doc_rope.to_string(), source);
        let tree = &doc.tree;
        assert_eq!(
            tree.root_node().utf8_text(source.as_bytes()).unwrap(),
            doc_rope.to_string()
        );
    }
}
