pub mod contracts;
pub mod definitions;
pub mod predicates;

use std::collections::HashMap;

use contracts::schema_object_to_contract;
use nickel_lang_core::{
    identifier::Ident,
    mk_app,
    term::{
        make,
        record::{Field, RecordData},
        LetAttrs, RichTerm, Term,
    },
};
use predicates::{schema_object_to_predicate, schema_to_predicate};
use schemars::schema::RootSchema;

pub fn root_schema(root: &RootSchema) -> RichTerm {
    let definitions = root.definitions.iter().map(|(name, schema)| {
        (
            Ident::from(name),
            Field::from(schema_to_predicate(&HashMap::new(), schema)),
        )
    });
    if let Some(contract) = schema_object_to_contract(&HashMap::new(), &root.schema) {
        wrap_contract(contract, definitions)
    } else {
        wrap_predicate(
            schema_object_to_predicate(&HashMap::new(), &root.schema),
            definitions,
        )
    }
}

pub fn wrap_contract(
    contract: RichTerm,
    definitions: impl IntoIterator<Item = (Ident, Field)>,
) -> RichTerm {
    Term::Let(
        "predicates".into(),
        Term::Import("./lib/predicates.ncl".into()).into(),
        Term::Let(
            "definitions".into(),
            Term::Record(RecordData {
                fields: definitions.into_iter().collect(),
                attrs: Default::default(),
                sealed_tail: None,
            })
            .into(),
            contract,
            LetAttrs {
                rec: true,
                ..Default::default()
            },
        )
        .into(),
        Default::default(),
    )
    .into()
}

pub fn wrap_predicate(
    predicate: RichTerm,
    definitions: impl IntoIterator<Item = (Ident, Field)>,
) -> RichTerm {
    wrap_contract(
        mk_app!(make::var("predicates.contract_from_predicate"), predicate),
        definitions,
    )
}
