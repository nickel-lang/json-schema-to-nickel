use std::{
    error::Error,
    fs::File,
    io::{stdout, Read},
    path::PathBuf,
};

use clap::Parser;
use json_schema_to_nickel::root_schema;
use nickel_lang_core::pretty::*;
use schemars::schema::RootSchema;
use terminal_size::{terminal_size, Width};

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Path to a JSON schema file, or "-" to read the schema file from stdin.
    schema: String,
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();
    let f: Box<dyn Read> = if args.schema == "-" {
        Box::new(std::io::stdin())
    } else {
        Box::new(File::open(PathBuf::from(args.schema))?)
    };
    let schema: RootSchema = serde_json::from_reader(f)?;

    let size = terminal_size()
        .map(|(Width(w), _)| w as usize)
        .unwrap_or(80);

    let types: DocBuilder<'_, _, ()> = root_schema(&schema).pretty(&pretty::BoxAllocator);

    println!("# DO NOT EDIT\n# This file was automatically generated using json-schema-to-nickel");
    types.render(size, &mut stdout())?;

    Ok(())
}
