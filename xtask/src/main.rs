use std::{fs::File, path::Path};

use clap::{Parser, Subcommand};
use serde_json::to_writer_pretty;
use ts_query_ls::Options;

#[derive(Parser)]
#[command(author, version, about)]
struct Args {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Generate a JSON schema for the config file.
    Schema,
}

fn main() {
    let args = Args::parse();

    match args.command {
        Commands::Schema => {
            let schema = schemars::schema_for!(Options);
            let Ok(xtask_path) = std::env::var("CARGO_MANIFEST_DIR") else {
                panic!("Cannot find CARGO_MANIFEST_DIR");
            };
            let schema_out_dir = Path::new(&xtask_path)
                .parent()
                .unwrap()
                .join("schemas/config.json");
            let Ok(mut json_file) = File::create(&schema_out_dir) else {
                panic!("Cannot create file: {:?}", schema_out_dir);
            };
            to_writer_pretty(&mut json_file, &schema).unwrap();
        }
    }
}
