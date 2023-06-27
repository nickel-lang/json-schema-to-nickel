use std::{error::Error, fs::File, io::stdout, path::PathBuf};

use clap::Parser;
use json_schema_to_nickel::schema_to_contract;
use nickel_lang::pretty::*;
use schemars::schema::{RootSchema, Schema};
use terminal_size::{terminal_size, Width};

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Args {
    schema: String,
}

fn main() -> Result<(), Box<dyn Error>> {
    let f = File::open(PathBuf::from(Args::parse().schema))?;
    let root_schema: RootSchema = serde_json::from_reader(f)?;

    let size = terminal_size()
        .map(|(Width(w), _)| w as usize)
        .unwrap_or(80);

    let types: DocBuilder<'_, _, ()> =
        dbg!(schema_to_contract(Schema::Object(dbg!(root_schema).schema)))
            .pretty(&pretty::BoxAllocator);

    types.render(size, &mut stdout())?;

    Ok(())
}
