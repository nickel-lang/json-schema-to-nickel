pub mod contracts;
pub mod definitions;
pub mod predicates;

use contracts::{contract_from_predicate, schema_object_to_contract};
use definitions::Environment;
use nickel_lang_core::term::{RichTerm, Term};
use predicates::schema_object_to_predicate;
use schemars::schema::RootSchema;

pub fn root_schema(root: &RootSchema) -> RichTerm {
    let env = Environment::from(&root.definitions);
    if let Some(contract) = schema_object_to_contract(env.references(), &root.schema) {
        wrap_contract(env, contract)
    } else {
        let predicate = schema_object_to_predicate(env.references(), &root.schema);
        wrap_predicate(env, predicate)
    }
}

pub fn wrap_contract(env: Environment, contract: RichTerm) -> RichTerm {
    Term::Let(
        "predicates".into(),
        Term::Import("./lib/predicates.ncl".into()).into(),
        env.wrap(contract),
        Default::default(),
    )
    .into()
}

pub fn wrap_predicate(env: Environment, predicate: RichTerm) -> RichTerm {
    wrap_contract(env, contract_from_predicate(predicate))
}
