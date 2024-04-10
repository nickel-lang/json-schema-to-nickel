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
    cache::{Cache, ErrorTolerance},
    parser::{grammar::TermParser, lexer::Lexer, ErrorTolerantParser},
    term::{RichTerm, Term},
};
use predicates::Predicate;
use schemars::schema::RootSchema;

/// The top-level variable storing the json-schema-to-nickel predicate library included by default
/// in any generated contract.
pub const PREDICATES_LIBRARY: &str = "_js2n_nickel_preds_lib";

/// The top-level variable storing the environment, that is the definitions and the properties
/// referenced in the JSON schema (through the `$ref`) attribute. This variable stores
/// [DEFINITIONS_MANGLED] and [PROPS_PREDICATES_MANGLED], each in their own field.
///
/// We put both under the same variable so that definitions and properties are accessible from
/// everywhere, including from other definitions and properties (in fact, we would like to have
/// mutual recursive let definitions for [DEFINITIONS_MANGLED] and [PROPS_PREDICATES_MANGLED], but
/// Nickel doesn't have mutually recursive lets, so we put both in a recursive record instead).
// pub const ENVIRONMENT_MANGLED: &str = "___js2n_nickel_global_env";
pub const ENVIRONMENT_MANGLED: &str = "_js2n_nickel_global_env";

/// The name of the special variable introduced by json-schema-to-nickel in the final contract
/// which holds the predicates and the contracts corresponding to the definitions of the schema.
/// The name is long and specific on purpose as it could clash with existing variable in the
/// schema.
///
/// This Nickel variable is expected to have the type
/// `{_ : {predicate: _, contract: _}}` where field names correspond to the top-level
/// definitions in the schema.
// pub const DEFINITIONS_MANGLED: &str = "___js2n_nickel_defs";
pub const DEFINITIONS_MANGLED: &str = "_js2n_nickel_defs";

/// Same as [DEFINITIONS_MANGLED] but for the predicates corresponding to properties of the schema.
///
/// This Nickel variable is expected to have the type `{_ : Dyn -> Bool}` where predicates are
/// directly stored without further indirection, as opposed to [DEFINITIONS_MANGLED]. Indeed, we
/// don't need the contract part, which can be accessed directly from within the final schema.
///
/// Properties can be nested, so we might need to store both a predicate for `foo` and for
/// `foo.bar.baz`. To make this work, we store the predicates in a flat dictionary, where the keys
/// are complete paths using `/` as a separator (to avoid confusion with Nickel field path).
// pub const PROPS_PREDICATES_MANGLED: &str = "___js2n_nickel_prop_preds";
pub const PROPS_PREDICATES_MANGLED: &str = "_js2n_nickel_prop_preds";

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
    let file_id = cache.add_string("predicates.ncl", lib_ncl.to_string());
    let lexer = Lexer::new(cache.source(file_id));
    let lib_rt = parser.parse_strict(file_id, lexer).unwrap();

    Term::Let(
        PREDICATES_LIBRARY.into(),
        lib_rt,
        env.wrap(contract.into()),
        Default::default(),
    )
    .into()
}
