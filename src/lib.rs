//! # Convert JSON schemas to Nickel contracts and predicates
//!
//! JSON schema can be considered as a DSL for writing predicates on JSON
//! documents. That is, a JSON schema encodes a function taking in a JSON
//! document and returning either "success" or an error message. Such predicates
//! can be turned into Nickel functions and applied as contracts eagerly. This
//! conversion is performed in [`crate::predicates`].
//!
//! Converting JSON schemas into lazy Nickel contracts is more difficult. In
//! fact, the general case is made impossible by JSON schema keywords like
//! `anyOf` and `oneOf`. However, there is still a large fragment of JSON schema
//! that can be translated in principle. The module [`crate::contracts`]
//! implements this conversion for some special cases. Take a look at the
//! documentation there for the current status and the supported kinds of JSON
//! schema.
//!
//! The main entry point of the converter is [`root_schema`]. This functions
//! takes a JSON schema parsed into a [`RootSchema`] and first attempts to
//! convert it into a lazy Nickel record contract. If that fails, it instead
//! generates a predicate. In either case, the result is wrapped with the
//! necessary bindings to the predicate support library and returned.
pub mod intermediate;
pub mod references;
pub mod schema;
pub mod traverse;
pub mod typ;
pub(crate) mod utils;

use nickel_lang_core::{
    cache::{Cache, ErrorTolerance, SourcePath},
    parser::{grammar::TermParser, lexer::Lexer, ErrorTolerantParser},
    term::RichTerm,
};

pub fn inline_lib() -> RichTerm {
    let lib_ncl = include_bytes!(concat!(env!("OUT_DIR"), "/main.ncl"));
    let lib_ncl = String::from_utf8_lossy(lib_ncl);

    let mut cache = Cache::new(ErrorTolerance::Strict);
    let parser = TermParser::new();
    let file_id = cache.add_string(
        SourcePath::Generated("main.ncl".to_owned()),
        lib_ncl.to_string(),
    );
    let lexer = Lexer::new(cache.source(file_id));
    parser.parse_strict(file_id, lexer).unwrap()
}
