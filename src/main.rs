use lazy_static::lazy_static;
use serde::{Deserialize, Serialize};
use std::{
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    sync::{Arc, RwLock},
};

use dashmap::DashMap;
use ropey::Rope;
use tower_lsp::{
    jsonrpc::Result,
    lsp_types::{
        CompletionOptions, CompletionParams, CompletionResponse, DidChangeConfigurationParams,
        DidChangeTextDocumentParams, DidOpenTextDocumentParams, DocumentFormattingParams,
        DocumentHighlight, DocumentHighlightParams, GotoDefinitionParams, GotoDefinitionResponse,
        InitializeParams, InitializeResult, Location, OneOf, ReferenceParams, RenameParams,
        SemanticTokenModifier, SemanticTokenType, SemanticTokensFullOptions, SemanticTokensLegend,
        SemanticTokensOptions, SemanticTokensParams, SemanticTokensResult,
        SemanticTokensServerCapabilities, ServerCapabilities, TextDocumentSyncCapability,
        TextDocumentSyncKind, TextEdit, Url, WorkspaceEdit,
    },
    Client, LanguageServer, LspService, Server,
};
use tree_sitter::{wasmtime::Engine, Language, Tree};

use handlers::*;

mod handlers;
mod test_helpers;
mod util;

lazy_static! {
    static ref SERVER_CAPABILITIES: ServerCapabilities = ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(
            TextDocumentSyncKind::INCREMENTAL,
        )),
        references_provider: Some(OneOf::Left(true)),
        rename_provider: Some(OneOf::Left(true)),
        definition_provider: Some(OneOf::Left(true)),
        document_formatting_provider: Some(OneOf::Left(true)),
        completion_provider: Some(CompletionOptions {
            trigger_characters: Some(["@", "\"", "\\", "(", "/"].map(ToOwned::to_owned).into()),
            ..CompletionOptions::default()
        }),
        document_highlight_provider: Some(OneOf::Left(true)),
        semantic_tokens_provider: Some(SemanticTokensServerCapabilities::SemanticTokensOptions(
            SemanticTokensOptions {
                legend: SemanticTokensLegend {
                    token_types: vec![SemanticTokenType::INTERFACE, SemanticTokenType::VARIABLE],
                    token_modifiers: vec![SemanticTokenModifier::DEFAULT_LIBRARY]
                },
                // TODO: Support range and delta semantic token requests.
                full: Some(SemanticTokensFullOptions::Bool(true)),
                ..Default::default()
            }
        )),
        ..Default::default()
    };
    static ref ENGINE: Engine = Engine::default();
    static ref QUERY_LANGUAGE: Language = tree_sitter_query::LANGUAGE.into();
}

#[derive(PartialEq, Eq, Hash, Clone, PartialOrd, Ord)]
struct SymbolInfo {
    label: String,
    named: bool,
}

struct Backend {
    client: Client,
    document_map: DashMap<Url, Rope>,
    cst_map: DashMap<Url, Tree>,
    symbols_set_map: DashMap<Url, HashSet<SymbolInfo>>,
    symbols_vec_map: DashMap<Url, Vec<SymbolInfo>>,
    fields_set_map: DashMap<Url, HashSet<String>>,
    fields_vec_map: DashMap<Url, Vec<String>>,
    supertype_map_map: DashMap<Url, HashMap<SymbolInfo, BTreeSet<SymbolInfo>>>,
    options: Arc<RwLock<Options>>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Default)]
struct Options {
    parser_install_directories: Option<Vec<String>>,
    parser_aliases: Option<BTreeMap<String, String>>,
    language_retrieval_patterns: Option<Vec<String>>,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        initialize::initialize(self, params).await
    }

    async fn did_change_configuration(&self, params: DidChangeConfigurationParams) {
        did_change_configuration::did_change_configuration(self, params).await
    }

    async fn shutdown(&self) -> Result<()> {
        shutdown::shutdown(self).await
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        goto_definition::goto_definition(self, params).await
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        did_open::did_open(self, params).await
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        did_change::did_change(self, params).await
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        references::references(self, params).await
    }

    async fn document_highlight(
        &self,
        params: DocumentHighlightParams,
    ) -> Result<Option<Vec<DocumentHighlight>>> {
        document_highlight::document_highlight(self, params).await
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        rename::rename(self, params).await
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        completion::completion(self, params).await
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        formatting::formatting(self, params).await
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        semantic_tokens_full::semantic_tokens_full(self, params).await
    }
}

#[tokio::main]
async fn main() {
    tracing_subscriber::fmt()
        .with_writer(std::io::stderr)
        .with_ansi(false)
        .init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let options = Arc::new(RwLock::new(Options {
        parser_install_directories: None,
        parser_aliases: None,
        language_retrieval_patterns: None,
    }));
    let (service, socket) = LspService::build(|client| Backend {
        client,
        document_map: DashMap::new(),
        cst_map: DashMap::new(),
        symbols_set_map: DashMap::new(),
        symbols_vec_map: DashMap::new(),
        fields_set_map: DashMap::new(),
        fields_vec_map: DashMap::new(),
        supertype_map_map: DashMap::new(),
        options,
    })
    .finish();

    Server::new(stdin, stdout, socket).serve(service).await;
}
