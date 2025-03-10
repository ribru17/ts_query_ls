use clap::{Parser, Subcommand};
use core::fmt;
use lazy_static::lazy_static;
use rayon::iter::{IntoParallelRefIterator as _, ParallelIterator as _};
use serde::{Deserialize, Serialize};
use std::{
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    fs,
    path::PathBuf,
    sync::{atomic::AtomicI32, Arc, RwLock},
};
use walkdir::WalkDir;

use dashmap::DashMap;
use ropey::Rope;
use tower_lsp::{
    jsonrpc::Result,
    lsp_types::{
        CompletionOptions, CompletionParams, CompletionResponse, DidChangeConfigurationParams,
        DidChangeTextDocumentParams, DidOpenTextDocumentParams, DocumentFormattingParams,
        DocumentHighlight, DocumentHighlightParams, DocumentSymbolParams, DocumentSymbolResponse,
        GotoDefinitionParams, GotoDefinitionResponse, Hover, HoverParams, HoverProviderCapability,
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
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        document_symbol_provider: Some(OneOf::Left(true)),
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

impl fmt::Display for SymbolInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let delims = if self.named { ('(', ')') } else { ('"', '"') };
        write!(f, "{}{}{}", delims.0, self.label, delims.1)
    }
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

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        hover::hover(self, params).await
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        document_symbol::document_symbol(self, params).await
    }
}

#[derive(Parser)]
#[command(
    name = "ts_query_ls",
    version = env!("CARGO_PKG_VERSION"),
    about = "LSP implementation for Tree-sitter's query files"
)]
struct Arguments {
    #[command(subcommand)]
    format: Option<Commands>,
}

#[derive(clap::ValueEnum, Clone, Debug)]
enum Mode {
    Check,
    Write,
}

#[derive(Subcommand)]
enum Commands {
    /// Format the query files in the given directories
    Format {
        /// List of directories to format
        directories: Vec<PathBuf>,

        /// Operation to perform on files
        #[arg(long, short)]
        mode: Mode,
    },
}

fn get_scm_files(directories: &[PathBuf]) -> Vec<PathBuf> {
    directories
        .iter()
        .flat_map(|directory| {
            WalkDir::new(directory)
                .into_iter()
                .filter_map(|e| e.ok())
                .filter(|e| {
                    e.file_type().is_file() && e.path().extension().is_some_and(|ext| ext == "scm")
                })
                .map(|e| e.path().to_owned())
        })
        .collect()
}

fn format_directories(directories: &[PathBuf], mode: Mode) -> i32 {
    let scm_files = get_scm_files(directories);

    let exit_code = AtomicI32::new(0);
    scm_files.par_iter().for_each(|path| {
        if let Ok(contents) = fs::read_to_string(path) {
            let mut parser = tree_sitter::Parser::new();
            parser
                .set_language(&QUERY_LANGUAGE)
                .expect("Error loading Query grammar");
            let tree = parser.parse(contents.as_str(), None).unwrap();
            let rope = Rope::from(contents.as_str());
            if let Some(formatted) = util::format_document(&rope, &tree) {
                // Add newline at EOF
                let formatted = formatted + "\n";
                match mode {
                    Mode::Check => {
                        let edits = util::diff(&contents, &formatted, &rope);
                        if !edits.is_empty() {
                            exit_code.store(1, std::sync::atomic::Ordering::Relaxed);
                            eprintln!(
                                "Improper formatting detected for {:?}",
                                path.canonicalize().unwrap()
                            );
                        }
                    }
                    Mode::Write => {
                        if fs::write(path, formatted).is_err() {
                            exit_code.store(1, std::sync::atomic::Ordering::Relaxed);
                            eprint!("Failed to write to {:?}", path.canonicalize().unwrap())
                        };
                    }
                }
            }
        } else {
            eprintln!("Failed to read {:?}", path.canonicalize().unwrap());
        }
    });
    exit_code.load(std::sync::atomic::Ordering::Relaxed)
}

#[tokio::main]
async fn main() {
    tracing_subscriber::fmt()
        .with_writer(std::io::stderr)
        .with_ansi(false)
        .init();

    let args = Arguments::parse();
    if let Some(Commands::Format { directories, mode }) = args.format {
        std::process::exit(format_directories(&directories, mode));
    }

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
