use std::collections::{BTreeSet, HashMap, HashSet};

use log::info;
use ropey::Rope;
use tower_lsp::lsp_types::DidOpenTextDocumentParams;
use tree_sitter::Parser;

use crate::{
    util::{get_diagnostics, get_language, TextProviderRope},
    Backend, SymbolInfo, QUERY_LANGUAGE,
};

pub async fn did_open(backend: &Backend, params: DidOpenTextDocumentParams) {
    let uri = &params.text_document.uri;
    info!("ts_query_ls did_ops: {params:?}");
    let contents = params.text_document.text;
    let rope = Rope::from_str(&contents);
    let mut parser = Parser::new();
    parser
        .set_language(&QUERY_LANGUAGE)
        .expect("Error loading Query grammar");
    backend.document_map.insert(uri.clone(), rope.clone());
    backend
        .cst_map
        .insert(uri.clone(), parser.parse(&contents, None).unwrap());

    // Initialize language info
    let mut symbols_vec: Vec<SymbolInfo> = vec![];
    let mut symbols_set: HashSet<SymbolInfo> = HashSet::new();
    let mut fields_vec: Vec<String> = vec![];
    let mut fields_set: HashSet<String> = HashSet::new();
    let mut supertype_map: HashMap<SymbolInfo, BTreeSet<SymbolInfo>> = HashMap::new();
    if let Some(lang) = &backend
        .options
        .read()
        .ok()
        .as_ref()
        .and_then(|options| get_language(uri, options))
    {
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
            if symbols_set.contains(&symbol_info)
                || !(lang.node_kind_is_visible(i) || lang.node_kind_is_supertype(i))
            {
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
    backend.symbols_vec_map.insert(uri.to_owned(), symbols_vec);
    backend.symbols_set_map.insert(uri.to_owned(), symbols_set);
    backend.fields_vec_map.insert(uri.to_owned(), fields_vec);
    backend.fields_set_map.insert(uri.to_owned(), fields_set);
    backend
        .supertype_map_map
        .insert(uri.to_owned(), supertype_map);

    // Publish diagnostics
    if let (Some(tree), Some(symbols), Some(fields), Some(supertypes)) = (
        backend.cst_map.get(uri),
        backend.symbols_set_map.get(uri),
        backend.fields_set_map.get(uri),
        backend.supertype_map_map.get(uri),
    ) {
        let provider = TextProviderRope(&rope);
        backend
            .client
            .publish_diagnostics(
                uri.clone(),
                get_diagnostics(&tree, &rope, &provider, &symbols, &fields, &supertypes),
                None,
            )
            .await;
    }
}

#[cfg(test)]
mod test {
    use pretty_assertions::assert_eq;
    use tower::{Service, ServiceExt};
    use tower_lsp::lsp_types::{
        notification::DidOpenTextDocument, DidOpenTextDocumentParams, TextDocumentItem,
    };

    use crate::test_helpers::helpers::{
        initialize_server, lsp_notification_to_jsonrpc_request, TEST_URI,
    };

    #[tokio::test(flavor = "current_thread")]
    async fn server_did_open_document() {
        // Arrange
        let mut service = initialize_server(&[]).await;
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
        let doc_rope = service.inner().document_map.get(&TEST_URI);
        assert!(doc_rope.is_some());
        let doc_rope = doc_rope.unwrap();
        assert_eq!(doc_rope.to_string(), source);
        let tree = service.inner().cst_map.get(&TEST_URI);
        assert!(tree.is_some());
        let tree = tree.unwrap();
        assert_eq!(
            tree.root_node().utf8_text(source.as_bytes()).unwrap(),
            doc_rope.to_string()
        );
    }
}
