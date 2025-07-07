use std::{
    fs,
    path::PathBuf,
    sync::{Arc, atomic::AtomicI32},
};

use anstyle::{AnsiColor, Color, Style};
use futures::future::join_all;
use ropey::Rope;

use crate::{QUERY_LANGUAGE, handlers::formatting, util::get_scm_files};

pub async fn format_directories(directories: &[PathBuf], check: bool) -> i32 {
    if directories.is_empty() {
        eprintln!("No directories were specified to be formatted. No work was done.");
        return 1;
    }

    let scm_files = get_scm_files(directories);
    let exit_code = Arc::new(AtomicI32::new(0));
    let use_color = std::env::var("NO_COLOR").map_or(true, |v| v.is_empty());
    let (red, green, blue, purple) = if use_color {
        (
            Some(AnsiColor::Red),
            Some(AnsiColor::Green),
            Some(AnsiColor::Blue),
            Some(AnsiColor::Magenta),
        )
    } else {
        (None, None, None, None)
    };

    let tasks = scm_files.into_iter().map(|path| {
        let exit_code = exit_code.clone();
        tokio::spawn(async move {
            let path_str = path.to_string_lossy();
            let Ok(contents) = fs::read_to_string(&path) else {
                eprintln!("Failed to read {path_str:?}");
                exit_code.store(1, std::sync::atomic::Ordering::Relaxed);
                return;
            };
            let mut parser = tree_sitter::Parser::new();
            parser
                .set_language(&QUERY_LANGUAGE)
                .expect("Error loading Query grammar");
            let tree = parser.parse(contents.as_str(), None).unwrap();
            let rope = Rope::from(contents.as_str());
            let Some(formatted) = formatting::format_document(&rope, &tree.root_node()) else {
                exit_code.store(1, std::sync::atomic::Ordering::Relaxed);
                eprintln!("No formatting performed -- invalid syntax detected at {path_str:?}");
                return;
            };
            if check {
                let edits = formatting::diff(&contents, &formatted, &rope);
                if !edits.is_empty() {
                    exit_code.store(1, std::sync::atomic::Ordering::Relaxed);
                    eprintln!("{}", paint(purple, &format!("{path_str:?}:")));
                    let patch = diffy::create_patch(&contents, &formatted).to_string();
                    for line in patch.lines() {
                        if line.starts_with("@@") {
                            eprintln!("{}", paint(blue, line));
                        } else if line.starts_with("-") {
                            eprintln!("{}", paint(red, line));
                        } else if line.starts_with("+") {
                            eprintln!("{}", paint(green, line));
                        } else {
                            eprintln!("{line}");
                        }
                    }
                    eprintln!();
                }
            } else if fs::write(&path, formatted).is_err() {
                exit_code.store(1, std::sync::atomic::Ordering::Relaxed);
                eprint!("Failed to write to {path_str:?}");
            }
        })
    });
    join_all(tasks).await;
    exit_code.load(std::sync::atomic::Ordering::Relaxed)
}

pub fn paint(color: Option<impl Into<Color>>, text: &str) -> String {
    let style = Style::new().fg_color(color.map(Into::into));
    format!("{style}{text}{style:#}")
}
