use std::{
    error::Error,
    ffi::OsString,
    fs::File,
    io::{stdout, Read},
};

use clap::Parser;
use json_schema_to_nickel::{
    inline_lib,
    intermediate::{self, inline_refs, simplify},
};
use nickel_lang_core::pretty::*;
use terminal_size::{terminal_size, Width};

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Path to a JSON schema file. If omitted, the schema file will be read from stdin.
    schema: Option<String>,

    /// Generate a contract that will import the json-schema-to-nickel Nickel library from the
    /// given path. The path must point to the entry point of the library, currently
    /// `predicates.ncl`.
    ///
    /// By default, when no library path is specified, json-schema-to-nickel will inline this
    /// library inside the contract to make it standalone. However, when converting many schemas
    /// and distributing them together, this inlining leads to a lot of code duplication. Using
    /// this argument avoids the duplication, although it makes you responsible of making the
    /// json-schema-to-nickel library available at the right place.
    #[clap(long)]
    library_path: Option<OsString>,
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();
    let f: Box<dyn Read> = if let Some(path) = args.schema {
        Box::new(File::open(path)?)
    } else {
        Box::new(std::io::stdin())
    };

    let val: serde_json::Value = serde_json::from_reader(f)?;
    let schema: intermediate::Schema = (&val).try_into().unwrap();
    let (schema, refs) = intermediate::resolve_references_recursive(&val, schema);
    let refs = refs
        .iter()
        .map(|(k, v)| (k.clone(), simplify(v.clone(), &refs)))
        .collect();
    let schema = inline_refs(schema, &refs);
    let schema = simplify(schema, &refs);

    let lib_term = if let Some(path) = args.library_path {
        nickel_lang_core::term::Term::Import {
            path,
            format: nickel_lang_core::cache::InputFormat::Nickel,
        }
        .into()
    } else {
        inline_lib()
    };

    let size = terminal_size()
        .map(|(Width(w), _)| w as usize)
        .unwrap_or(80);

    let contract = intermediate::to_nickel(schema, &refs, lib_term);
    let pretty_alloc = Allocator::default();
    let types: DocBuilder<'_, _, ()> = contract.pretty(&pretty_alloc);

    println!("# DO NOT EDIT\n# This file was automatically generated using json-schema-to-nickel");
    types.render(size, &mut stdout())?;

    Ok(())
}
