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
pub mod contracts;
pub mod definitions;
pub mod predicates;
pub(crate) mod utils;

use contracts::Contract;
use definitions::Environment;
use nickel_lang_core::{
    cache::{Cache, ErrorTolerance, SourcePath},
    parser::{grammar::TermParser, lexer::Lexer, ErrorTolerantParser},
    term::{RichTerm, Term},
};
use predicates::Predicate;
use schemars::schema::RootSchema;

/// The top-level variable storing the json-schema-to-nickel predicate library included by default
/// in any generated contract.
// It would have been nice to do compile-time concatenation instead of inlining MANGLING_PREFIX,
// but Rust's stdlib doesn't support it, and it's not worth pulling a dependency just for that.
pub const PREDICATES_LIBRARY_ID: &str = "_js2n__-prdslib";

/// The top-level variable storing the environment, that is the definitions and the properties
/// referenced in the JSON schema (through the `$ref`) attribute.
pub const ENVIRONMENT_ID: &str = "_js2n__-refsenv";

/// Mangling prefix used to lower the risk of having clash between variables introduced by
/// json-schema-to-nickel and actual JSON schema components. This prefix is used in particular to
/// give (hopefully) unique names to reference pointees appearing in the references environment.
pub const MANGLING_PREFIX: &str = "_js2n__-";

/// Convert a [`RootSchema`] into a Nickel contract. If the JSON schema is
/// representable as a lazy record contract, this conversion is preferred.
/// Otherwise, we fall back to generating a predicate.
pub fn root_schema(root_schema: &RootSchema) -> RichTerm {
    let (contract, refs_usage) = Contract::from_root_schema(root_schema).unwrap_or_else(|| {
        let (predicate, refs_usage) = Predicate::from_root_schema(root_schema);
        (Contract::from(predicate), refs_usage)
    });

    let env = Environment::new(root_schema, refs_usage);
    wrap_contract(env, contract)
}

/// Wrap a Nickel contract making use of the predicates support library and
/// recursive definitions recorded in `env`.
pub fn wrap_contract(env: Environment, contract: Contract) -> RichTerm {
    let lib_ncl = include_bytes!(concat!(env!("OUT_DIR"), "/predicates.ncl"));
    let lib_ncl = String::from_utf8_lossy(lib_ncl);

    let mut cache = Cache::new(ErrorTolerance::Strict);
    let parser = TermParser::new();
    let file_id = cache.add_string(
        SourcePath::Generated("predicates.ncl".to_owned()),
        lib_ncl.to_string(),
    );
    let lexer = Lexer::new(cache.source(file_id));
    let lib_rt = parser.parse_strict(file_id, lexer).unwrap();

    Term::Let(
        PREDICATES_LIBRARY_ID.into(),
        lib_rt,
        env.wrap(contract.into()),
        Default::default(),
    )
    .into()
}
