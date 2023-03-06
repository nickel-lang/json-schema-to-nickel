use std::{error::Error, fs::File, io::stdout, path::PathBuf};

use clap::Parser;
use json_schema_to_nickel::schema_to_types;
use nickel_lang::pretty::{DocBuilder, Pretty};
use pretty::BoxAllocator;
use schemars::schema::Schema;
use terminal_size::{terminal_size, Width};

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Args {
    schema: String,
}

fn main() -> Result<(), Box<dyn Error>> {
    let f = File::open(PathBuf::from(Args::parse().schema))?;
    let schema: Schema = serde_json::from_reader(f)?;

    let size = terminal_size()
        .map(|(Width(w), _)| w as usize)
        .unwrap_or(80);

    let types: DocBuilder<'_, BoxAllocator> =
        dbg!(schema_to_types(dbg!(schema))).pretty(&BoxAllocator);
    types.render(size, &mut stdout())?;

    Ok(())
}
