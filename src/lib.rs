//! # Convert JSON schemas to Nickel contracts and predicates
//!
//! JSON schema can be considered as a DSL for writing predicates on JSON
//! documents. That is, a JSON schema encodes a function taking in a JSON
//! document and returning either "success" or an error message. Such predicates
//! can be turned into Nickel contracts.
//!
//! Converting JSON schemas into lazy Nickel contracts is not always possible,
//! because JSON schema keywords like `anyOf` and `oneOf` require eager
//! evaluation. We attempt to create idiomatic lazy Nickel contracts, falling
//! back to eager contracts when we are unable to do so.

pub mod contract;
pub mod extract;
pub mod object;
pub mod references;
pub mod schema;
pub mod transform;
pub mod traverse;
pub mod typ;
pub(crate) mod utils;

use nickel_lang_core::{
    bytecode::ast::{compat::ToMainline, AstAlloc},
    cache::{CacheHub, SourcePath},
    parser::{grammar::TermParser, lexer::Lexer, ErrorTolerantParser},
    term::RichTerm,
};
use references::{resolve_all, AcyclicReferences};
use schema::Schema;

pub fn inline_lib() -> RichTerm {
    let lib_ncl = include_bytes!(concat!(env!("OUT_DIR"), "/main.ncl"));
    let lib_ncl = String::from_utf8_lossy(lib_ncl);

    let mut cache = CacheHub::new();
    let parser = TermParser::new();
    let file_id = cache.sources.add_string(
        SourcePath::Generated("main.ncl".to_owned()),
        lib_ncl.to_string(),
    );
    let lexer = Lexer::new(cache.sources.source(file_id));
    parser
        .parse_strict(&AstAlloc::new(), file_id, lexer)
        .unwrap()
        .to_mainline()
}

/// Create a Nickel contract from a JSON value containing a JSON Schema.
///
/// `lib_import` is a term that imports the json-schema library.
pub fn convert(val: &serde_json::Value, lib_import: RichTerm) -> miette::Result<RichTerm> {
    let schema: Schema = val.try_into()?;
    let all_refs = resolve_all(val, &schema);
    let refs = AcyclicReferences::new(&all_refs);
    let simple_refs = all_refs
        .iter()
        .map(|(k, v)| (k.clone(), transform::simplify(v.clone(), &refs)))
        .collect();
    let refs = AcyclicReferences::new(&simple_refs);
    let schema = transform::inline_refs(schema, &refs);
    let schema = transform::simplify(schema, &refs);

    Ok(transform::schema_to_nickel(&schema, &refs, lib_import))
}
