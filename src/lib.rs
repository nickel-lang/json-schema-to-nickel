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

use contracts::{contract_from_predicate, TryAsContract};
use definitions::Environment;
use nickel_lang_core::term::{RichTerm, Term};
use predicates::schema_object_to_predicate;
use schemars::schema::RootSchema;

/// Convert a [`RootSchema`] into a Nickel contract. If the JSON schema is
/// representable as a lazy record contract, this conversion is preferred.
/// Otherwise, we fall back to generating a predicate.
pub fn root_schema(root: &RootSchema) -> RichTerm {
    let env = Environment::from(&root.definitions);
    if let Some(contract) = root.schema.try_as_contract() {
        wrap_contract(env, contract)
    } else {
        let predicate = schema_object_to_predicate(&root.schema);
        wrap_predicate(env, predicate)
    }
}

/// Wrap a Nickel contract making use of the predicates support library and
/// recursive definitions recorded in `env`.
pub fn wrap_contract(env: Environment, contract: RichTerm) -> RichTerm {
    Term::Let(
        "predicates".into(),
        Term::Import("./lib/predicates.ncl".into()).into(),
        env.wrap(contract),
        Default::default(),
    )
    .into()
}

/// Convert a predicate into a contract and then wrap it using `wrap_contract`.
pub fn wrap_predicate(env: Environment, predicate: RichTerm) -> RichTerm {
    wrap_contract(env, contract_from_predicate(predicate))
}
