use std::{
    env, fs,
    path::{Path, PathBuf},
    sync::{Arc, atomic::AtomicI32},
};

use dashmap::DashMap;
use futures::future::join_all;
use ropey::Rope;
use tower_lsp::lsp_types::{CodeAction, CodeActionOrCommand, DiagnosticSeverity, Url};
use ts_query_ls::Options;

use crate::{
    DocumentData, LanguageData,
    handlers::{
        code_action::diag_to_code_action, diagnostic::get_diagnostics,
        did_open::populate_import_documents,
    },
    util::{edit_rope, get_imported_uris, get_language_name, get_scm_files, parse},
};

#[allow(clippy::too_many_arguments)] // TODO: Refactor this
pub(super) async fn lint_file(
    path: &Path,
    workspace: &Path,
    uri: &Url,
    source: &str,
    options: Arc<tokio::sync::RwLock<Options>>,
    fix: bool,
    ignore_missing_language: bool,
    language_data: Option<Arc<LanguageData>>,
    exit_code: &AtomicI32,
) -> Option<String> {
    let rope = Rope::from(source);
    let tree = parse(&rope, None);
    let options_val = options.clone().read().await.clone();
    let language_name = get_language_name(uri, &options_val);
    let workspace_uris = &[workspace.to_owned()];
    let imported_uris = get_imported_uris(workspace_uris, &options_val, uri, &rope, &tree);
    let document_map = DashMap::new();
    populate_import_documents(&document_map, workspace_uris, &options_val, &imported_uris);
    let doc = DocumentData {
        tree,
        rope,
        language_name,
        version: Default::default(),
        imported_uris,
    };
    let cache = false;
    // The query construction already validates node names, fields, supertypes,
    // etc.
    let diagnostics = get_diagnostics(
        uri,
        &document_map,
        doc.clone(),
        language_data,
        options,
        ignore_missing_language,
        cache,
    )
    .await;
    if diagnostics.is_empty() {
        return None;
    }

    let path_str = path.to_str().expect("Path should be valid UTF-8");
    let mut edits = Vec::new();
    if !fix {
        exit_code.store(1, std::sync::atomic::Ordering::Relaxed);
    }
    let mut unfixed_issues = 0;
    for diagnostic in diagnostics {
        if !fix {
            let kind = match diagnostic.severity {
                Some(DiagnosticSeverity::ERROR) => "Error",
                Some(DiagnosticSeverity::WARNING) => "Warning",
                Some(DiagnosticSeverity::INFORMATION) => "Info",
                Some(DiagnosticSeverity::HINT) => "Hint",
                _ => "Diagnostic",
            };
            eprintln!(
                "{} in \"{}\" on line {}, col {}:\n  {}",
                kind,
                path_str,
                diagnostic.range.start.line + 1,
                diagnostic.range.start.character + 1,
                diagnostic.message
            );
            for related_info in diagnostic.related_information.unwrap_or_default() {
                eprintln!(
                    "    ‣ {}:{}:{}: {}",
                    related_info
                        .location
                        .uri
                        .to_file_path()
                        .expect("Related information URI should be a valid file path")
                        .strip_prefix(workspace)
                        .expect("Related information URI should be within the workspace")
                        .to_string_lossy(),
                    related_info.location.range.start.line + 1,
                    related_info.location.range.start.character + 1,
                    related_info.message
                );
            }
        } else {
            let is_module_diagnostic = diagnostic.related_information.is_some();
            let Some(action) = diag_to_code_action(&doc.tree, &doc.rope, diagnostic, uri) else {
                if !is_module_diagnostic {
                    unfixed_issues += 1;
                }
                continue;
            };
            let CodeActionOrCommand::CodeAction(CodeAction {
                edit: Some(edit), ..
            }) = action
            else {
                continue;
            };
            let Some(mut changes) = edit.changes.and_then(|mut changes| changes.remove(uri)) else {
                continue;
            };
            edits.append(&mut changes);
        }
    }
    if unfixed_issues > 0 {
        let plurality = if unfixed_issues > 1 { "s" } else { "" };
        println!("{path_str}: {unfixed_issues} issue{plurality} could not be fixed automatically");
    }
    if !fix || edits.is_empty() {
        return None;
    }
    edits.sort_unstable_by(|a, b| b.range.start.cmp(&a.range.start));
    let mut rope = doc.rope;
    for edit in edits {
        let range = edit.range;
        let new_text = edit.new_text;
        edit_rope(&mut rope, range, &new_text);
    }
    Some(rope.to_string())
}

/// Lint all the given directories according to the given configuration. Linting covers things like
/// invalid capture names or predicate signatures, but not errors like invalid node names or
/// impossible patterns.
pub async fn lint_directories(
    directories: &[PathBuf],
    config: String,
    workspace: Option<PathBuf>,
    fix: bool,
) -> i32 {
    let Ok(options) = serde_json::from_str::<Options>(&config) else {
        eprintln!("Could not parse the provided configuration");
        return 1;
    };
    let options: Arc<tokio::sync::RwLock<Options>> = Arc::new(options.into());
    let exit_code = Arc::new(AtomicI32::new(0));
    // If directories are not specified, lint all files in the current directory
    let directories = if directories.is_empty() {
        &[env::current_dir().expect("Failed to get current directory")]
    } else {
        directories
    };
    let workspace = workspace
        .unwrap_or(env::current_dir().expect("Failed to get current directory"))
        .canonicalize()
        .expect("Workspace path should be valid");
    let workspace = Arc::new(workspace);
    let scm_files = get_scm_files(directories);
    let tasks = scm_files.into_iter().filter_map(|path| {
        let absolute_path = path.canonicalize().expect("Path should be valid");
        let uri = Url::from_file_path(&absolute_path).expect("Path should be absolute");
        let exit_code = exit_code.clone();
        let options = options.clone();
        if let Ok(source) = fs::read_to_string(&path) {
            let workspace = workspace.clone();
            Some(tokio::spawn(async move {
                if let Some(new_source) = lint_file(
                    &path, &workspace, &uri, &source, options, fix, true, None, &exit_code,
                )
                .await
                    && fs::write(&path, new_source).is_err()
                {
                    eprintln!("Failed to write {absolute_path:?}");
                    exit_code.store(1, std::sync::atomic::Ordering::Relaxed);
                };
            }))
        } else {
            eprintln!("Failed to read {absolute_path:?}");
            exit_code.store(1, std::sync::atomic::Ordering::Relaxed);
            None
        }
    });
    join_all(tasks).await;
    exit_code.load(std::sync::atomic::Ordering::Relaxed)
}
