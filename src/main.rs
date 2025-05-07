use clap::{Parser, Subcommand};
use cli::{check::check_directories, format::format_directories, lint::lint_directories};
use core::fmt;
use std::{
    collections::{BTreeSet, HashMap, HashSet},
    env,
    fs::{self},
    path::{Path, PathBuf},
    str,
    sync::{Arc, LazyLock, RwLock},
};
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};
use ts_query_ls::Options;

use dashmap::DashMap;
use ropey::Rope;
use tower_lsp::{
    Client, LanguageServer, LspService, Server,
    jsonrpc::Result,
    lsp_types::{
        CodeActionParams, CodeActionProviderCapability, CodeActionResponse, CompletionOptions,
        CompletionParams, CompletionResponse, DiagnosticOptions, DiagnosticServerCapabilities,
        DidChangeConfigurationParams, DidChangeTextDocumentParams, DidOpenTextDocumentParams,
        DocumentDiagnosticParams, DocumentDiagnosticReportResult, DocumentFormattingParams,
        DocumentHighlight, DocumentHighlightParams, DocumentSymbolParams, DocumentSymbolResponse,
        ExecuteCommandOptions, ExecuteCommandParams, GotoDefinitionParams, GotoDefinitionResponse,
        Hover, HoverParams, HoverProviderCapability, InitializeParams, InitializeResult, Location,
        OneOf, ReferenceParams, RenameParams, SemanticTokenModifier, SemanticTokenType,
        SemanticTokensFullOptions, SemanticTokensLegend, SemanticTokensOptions,
        SemanticTokensParams, SemanticTokensResult, SemanticTokensServerCapabilities,
        ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind, TextEdit, Url,
        WorkspaceEdit,
    },
};
use tree_sitter::{Language, Tree, wasmtime::Engine};

use handlers::*;
use logging::LspLogLayer;

mod cli;
mod handlers;
mod logging;
mod test_helpers;
mod util;

static SERVER_CAPABILITIES: LazyLock<ServerCapabilities> = LazyLock::new(|| {
    ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(
            TextDocumentSyncKind::INCREMENTAL,
        )),
        diagnostic_provider: Some(DiagnosticServerCapabilities::Options(DiagnosticOptions {
            identifier: Some(String::from("ts_query_ls")),
            ..Default::default()
        })),
        references_provider: Some(OneOf::Left(true)),
        rename_provider: Some(OneOf::Left(true)),
        definition_provider: Some(OneOf::Left(true)),
        document_formatting_provider: Some(OneOf::Left(true)),
        completion_provider: Some(CompletionOptions {
            trigger_characters: Some(
                ["@", "\"", "\\", "(", "/", ".", "#"]
                    .map(ToOwned::to_owned)
                    .into(),
            ),
            ..CompletionOptions::default()
        }),
        document_highlight_provider: Some(OneOf::Left(true)),
        semantic_tokens_provider: Some(SemanticTokensServerCapabilities::SemanticTokensOptions(
            SemanticTokensOptions {
                legend: SemanticTokensLegend {
                    token_types: vec![SemanticTokenType::INTERFACE, SemanticTokenType::VARIABLE],
                    token_modifiers: vec![SemanticTokenModifier::DEFAULT_LIBRARY],
                },
                // TODO: Support range and delta semantic token requests.
                full: Some(SemanticTokensFullOptions::Bool(true)),
                ..Default::default()
            },
        )),
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        document_symbol_provider: Some(OneOf::Left(true)),
        code_action_provider: Some(CodeActionProviderCapability::Simple(true)),
        execute_command_provider: Some(ExecuteCommandOptions {
            commands: vec!["ts-query-ls.checkImpossiblePatterns".to_string()],
            ..Default::default()
        }),
        ..Default::default()
    }
});
static ENGINE: LazyLock<Engine> = LazyLock::new(Engine::default);
static QUERY_LANGUAGE: LazyLock<Language> = LazyLock::new(|| tree_sitter_query::LANGUAGE.into());

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

#[derive(Clone)]
pub struct LanguageData {
    language: Language,
    // TODO: Once most parsers are upgraded to ABI 15, just get the name from the language object
    // itself
    name: String,
}

struct DocumentData {
    symbols_set: HashSet<SymbolInfo>,
    symbols_vec: Vec<SymbolInfo>,
    fields_set: HashSet<String>,
    fields_vec: Vec<String>,
    supertype_map: HashMap<SymbolInfo, BTreeSet<SymbolInfo>>,
    rope: Rope,
    tree: Tree,
    version: i32,
    language_data: Option<LanguageData>,
}

struct Backend {
    client: Client,
    document_map: DashMap<Url, DocumentData>,
    options: Arc<tokio::sync::RwLock<Options>>,
    workspace_uris: Arc<RwLock<Vec<Url>>>,
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

    async fn code_action(&self, params: CodeActionParams) -> Result<Option<CodeActionResponse>> {
        code_action::code_action(self, params).await
    }

    async fn execute_command(
        &self,
        params: ExecuteCommandParams,
    ) -> Result<Option<serde_json::Value>> {
        execute_command::execute_command(self, params).await
    }

    async fn diagnostic(
        &self,
        params: DocumentDiagnosticParams,
    ) -> Result<DocumentDiagnosticReportResult> {
        diagnostic::diagnostic(self, params).await
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
    commands: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    /// Format the query files in the given directories
    Format {
        /// List of directories to format
        directories: Vec<PathBuf>,

        /// Only check that formatting is valid, do not write
        #[arg(long, short)]
        check: bool,
    },
    /// Check the query files in the given directories for errors. This will only check for query
    /// parsing errors. Pass the `--lint` flag to also check for lint warnings. See the `--lint`
    /// flag description for more details.
    Check {
        /// List of directories to check
        directories: Vec<PathBuf>,

        /// String representing server's JSON configuration
        #[arg(long, short)]
        config: Option<String>,

        /// Check for valid formatting
        #[arg(long, short)]
        format: bool,

        /// Check for lint warnings in addition to query errors. This catches things like invalid
        /// capture names or predicate signatures (as defined by the configuration options).
        #[arg(long, short)]
        lint: bool,
    },
    /// Lint the query files in the given directories for errors. This differs from `check` because
    /// it does not perform a full semantic analysis (e.g. analyzing for impossible patterns), but
    /// it does validate that there are no invalid captures or predicates as specified by the
    /// configuration options. Useful when you don't (yet?) have access to the parser objects.
    Lint {
        /// List of directories to lint
        directories: Vec<PathBuf>,

        /// String representing server's JSON configuration
        #[arg(long, short)]
        config: Option<String>,
    },
}

/// Return the given config string, or read it from a config file if not given. This function can
/// exit the program.
fn get_config_str(config: Option<String>) -> String {
    match config {
        Some(config_str) => config_str,
        None => {
            let config_file_path = Path::new(".tsqueryrc.json");
            fs::read_to_string(config_file_path).unwrap_or_else(|_| {
                eprintln!("No config parameter given, and no .tsqueryrc.json found");
                std::process::exit(1);
            })
        }
    }
}

#[tokio::main]
async fn main() {
    let args = Arguments::parse();
    match args.commands {
        Some(Commands::Format { directories, check }) => {
            std::process::exit(format_directories(&directories, check));
        }
        Some(Commands::Check {
            directories,
            config,
            format,
            lint,
        }) => {
            let config_str = get_config_str(config);
            std::process::exit(check_directories(&directories, config_str, format, lint));
        }
        Some(Commands::Lint {
            directories,
            config,
        }) => {
            let config_str = get_config_str(config);
            std::process::exit(lint_directories(&directories, config_str))
        }
        None => {}
    }

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let options = Arc::new(tokio::sync::RwLock::new(Options::default()));
    let (service, socket) = LspService::build(|client| {
        let lsp_layer = LspLogLayer::new(client.clone());
        tracing_subscriber::registry().with(lsp_layer).init();

        Backend {
            client,
            document_map: Default::default(),
            workspace_uris: Default::default(),
            options,
        }
    })
    .finish();

    Server::new(stdin, stdout, socket).serve(service).await;
}
