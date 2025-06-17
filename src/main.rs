use std::{
    error::Error,
    ffi::OsString,
    fs::File,
    io::{stdout, Read},
};

use clap::Parser;
use json_schema_to_nickel::{convert, inline_lib};
use nickel_lang_core::{
    bytecode::ast::{AstAlloc, Import, Node},
    pretty::*,
};
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

    /// Generate a contract that will import the json-schema-to-nickel Nickel library from a
    /// package with the given name.
    ///
    /// For example, if you provide `--library-pkg=js2n` then the generated contract will
    /// attempt to import the json-schema-to-nickel library using the syntax `import js2n`.
    /// This argument does not generate a `Nickel-pkg.ncl` package manifest; you will need
    /// to provide one that defines the `js2n` package.
    #[clap(long, conflicts_with = "library_path")]
    library_pkg: Option<String>,
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();
    let f: Box<dyn Read> = if let Some(path) = args.schema {
        Box::new(File::open(path)?)
    } else {
        Box::new(std::io::stdin())
    };
    let alloc = AstAlloc::new();
    let lib_term = if let Some(path) = args.library_path.as_ref() {
        Node::Import(Import::Path {
            path,
            format: nickel_lang_core::cache::InputFormat::Nickel,
        })
        .into()
    } else if let Some(lib) = args.library_pkg {
        Node::Import(Import::Package { id: lib.into() }).into()
    } else {
        inline_lib(&alloc)
    };

    let val: serde_json::Value = serde_json::from_reader(f)?;
    let contract = convert(&val, lib_term, &alloc)?;

    let size = terminal_size()
        .map(|(Width(w), _)| w as usize)
        .unwrap_or(80);
    let pretty_alloc = nickel_lang_core::bytecode::pretty::Allocator::default();
    let types: DocBuilder<'_, _, ()> = contract.pretty(&pretty_alloc);

    println!("# DO NOT EDIT\n# This file was automatically generated using json-schema-to-nickel");
    types.render(size, &mut stdout())?;

    Ok(())
}
