use std::{
    collections::{BTreeSet, HashMap, HashSet},
    fs,
    path::PathBuf,
};

use dashmap::DashMap;
use ropey::Rope;
use tower_lsp::lsp_types::{DidOpenTextDocumentParams, Url};
use tracing::info;
use tree_sitter::{Language, Parser};
use ts_query_ls::Options;

use crate::{
    Backend, DocumentData, ImportedUri, LanguageData, QUERY_LANGUAGE, SymbolInfo,
    util::{get_imported_uris, get_language, get_language_name, push_diagnostics},
};

pub async fn did_open(backend: &Backend, params: DidOpenTextDocumentParams) {
    let uri = params.text_document.uri;
    info!("ts_query_ls did_open: {uri}");
    let contents = params.text_document.text;
    let rope = Rope::from_str(&contents);
    let mut parser = Parser::new();
    parser
        .set_language(&QUERY_LANGUAGE)
        .expect("Error loading Query grammar");
    let tree = parser.parse(&contents, None).unwrap();

    let options = backend.options.read().await;
    let language_name = get_language_name(&uri, &options);
    let workspace_uris = backend.workspace_uris.read().unwrap().clone();
    let imported_uris = get_imported_uris(&workspace_uris, &options, &uri, &rope, &tree);

    populate_import_documents(
        &backend.document_map,
        &workspace_uris,
        &options,
        &imported_uris,
    );

    // Track the document
    let version = params.text_document.version;
    backend.document_map.insert(
        uri.clone(),
        DocumentData {
            rope,
            tree,
            language_name: language_name.clone(),
            version,
            imported_uris,
        },
    );

    fn populate_language_info(backend: &Backend, language_name: Option<String>, options: &Options) {
        let Some(language_name) = language_name else {
            return;
        };
        if backend.language_map.contains_key(&language_name) {
            return;
        }
        let Some(lang) = get_language(&language_name, options) else {
            return;
        };
        let language_data = init_language_data(lang, language_name.clone());
        backend
            .language_map
            .insert(language_name, language_data.into());
    }

    populate_language_info(backend, language_name, &options);

    push_diagnostics(backend, uri).await;
}

pub fn init_language_data(language: Language, name: String) -> LanguageData {
    let mut symbols_vec: Vec<SymbolInfo> = vec![];
    let mut symbols_set: HashSet<SymbolInfo> = HashSet::new();
    let mut fields_vec: Vec<String> = vec![];
    let mut fields_set: HashSet<String> = HashSet::new();
    let mut supertype_map: HashMap<SymbolInfo, BTreeSet<SymbolInfo>> = HashMap::new();

    let error_symbol = SymbolInfo {
        label: "ERROR".to_owned(),
        named: true,
    };
    symbols_set.insert(error_symbol.clone());
    symbols_vec.push(error_symbol);
    for i in 0..language.node_kind_count() as u16 {
        let supertype = language.node_kind_is_supertype(i);
        let named = language.node_kind_is_named(i) || supertype;
        let label = if named {
            language.node_kind_for_id(i).unwrap().to_owned()
        } else {
            language
                .node_kind_for_id(i)
                .unwrap()
                .replace('\\', r"\\")
                .replace('"', r#"\""#)
                .replace('\n', r"\n")
                .replace('\r', r"\r")
                .replace('\t', r"\t")
                .replace('\0', r"\0")
        };
        let symbol_info = SymbolInfo { label, named };
        if supertype {
            supertype_map.insert(
                symbol_info.clone(),
                language
                    .subtypes_for_supertype(i)
                    .iter()
                    .map(|s| SymbolInfo {
                        label: language.node_kind_for_id(*s).unwrap().to_string(),
                        named: language.node_kind_is_named(*s)
                            || language.node_kind_is_supertype(*s),
                    })
                    .collect(),
            );
        }
        if symbols_set.contains(&symbol_info) || !(language.node_kind_is_visible(i) || supertype) {
            continue;
        }
        symbols_set.insert(symbol_info.clone());
        symbols_vec.push(symbol_info);
    }
    // Field IDs go from 1 to nfields inclusive (extra index 0 maps to NULL)
    for i in 1..=language.field_count() as u16 {
        let field_name = language.field_name_for_id(i).unwrap().to_owned();
        if !fields_set.contains(&field_name) {
            fields_set.insert(field_name.clone());
            fields_vec.push(field_name);
        }
    }
    LanguageData {
        name,
        fields_set,
        fields_vec,
        symbols_vec,
        symbols_set,
        supertype_map,
        language,
    }
}

pub fn populate_import_documents(
    document_map: &DashMap<Url, DocumentData>,
    workspace_dirs: &[PathBuf],
    options: &Options,
    imported_uris: &Vec<ImportedUri>,
) {
    for imported_uri in imported_uris {
        if let Some(uri) = &imported_uri.uri
            && !document_map.contains_key(uri)
            && let Ok(contents) = uri
                .to_file_path()
                .and_then(|path| fs::read_to_string(path).map_err(|_| ()))
        {
            let rope = Rope::from_str(&contents);
            let mut parser = Parser::new();
            parser
                .set_language(&QUERY_LANGUAGE)
                .expect("Error loading Query grammar");
            let tree = parser.parse(&contents, None).unwrap();
            let nested_imported_uris =
                get_imported_uris(workspace_dirs, options, uri, &rope, &tree);
            document_map.insert(
                uri.clone(),
                DocumentData {
                    rope,
                    tree,
                    language_name: None,
                    version: -1,
                    imported_uris: nested_imported_uris.clone(),
                },
            );
            populate_import_documents(document_map, workspace_dirs, options, &nested_imported_uris)
        }
    }
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
