use std::collections::{BTreeMap, HashMap};

use nickel_lang_core::{
    identifier::Ident,
    term::{make, record::RecordData, LetAttrs, RichTerm, Term, UnaryOp},
};
use schemars::schema::Schema;

use crate::{
    contracts::{contract_from_predicate, schema_to_contract},
    predicates::schema_to_predicate,
};

#[derive(Clone)]
pub struct Terms {
    predicate: RichTerm,
    contract: RichTerm,
}

#[derive(Clone)]
pub struct Access {
    pub predicate: RichTerm,
    pub contract: RichTerm,
}

#[derive(Clone, Default)]
pub struct References(HashMap<String, Access>);

#[derive(Clone, Default)]
pub struct Environment {
    accesses: References,
    terms: HashMap<String, Terms>,
}

impl References {
    fn with_fields<'a, F, I>(fields: F) -> Self
    where
        F: 'a + IntoIterator<Item = I>,
        I: 'a + AsRef<str>,
    {
        Self(
            fields
                .into_iter()
                .map(|n| {
                    let id = Ident::from(n.as_ref());
                    (
                        String::from(n.as_ref()),
                        Access {
                            contract: make::op1(
                                UnaryOp::StaticAccess(id),
                                make::op1(
                                    UnaryOp::StaticAccess("contract".into()),
                                    make::var("definitions"),
                                ),
                            ),
                            predicate: make::op1(
                                UnaryOp::StaticAccess(id),
                                make::op1(
                                    UnaryOp::StaticAccess("predicate".into()),
                                    make::var("definitions"),
                                ),
                            ),
                        },
                    )
                })
                .collect(),
        )
    }

    pub fn get_predicate(&self, name: impl AsRef<str>) -> Option<RichTerm> {
        self.0.get(name.as_ref()).map(|e| e.predicate.clone())
    }

    pub fn get_contract(&self, name: impl AsRef<str>) -> Option<RichTerm> {
        self.0.get(name.as_ref()).map(|e| e.contract.clone())
    }

    pub fn access(&self, name: impl AsRef<str>) -> Option<&Access> {
        self.0.get(name.as_ref())
    }

    pub fn reference(&self, reference: &str) -> &Access {
        if let Some(remainder) = reference.strip_prefix("#/definitions/") {
            self.access(remainder).unwrap()
        } else {
            unimplemented!()
        }
    }
}

impl Environment {
    pub fn empty() -> Self {
        Self::default()
    }

    pub fn wrap(self, inner: RichTerm) -> RichTerm {
        let contracts = self
            .terms
            .iter()
            .map(|(k, v)| (Ident::from(k), v.contract.clone()))
            .collect();
        let predicates = self
            .terms
            .into_iter()
            .map(|(k, v)| (Ident::from(k), v.predicate))
            .collect();
        Term::Let(
            "definitions".into(),
            Term::Record(RecordData::with_field_values(
                [
                    (
                        Ident::from("contract"),
                        Term::Record(RecordData::with_field_values(contracts)).into(),
                    ),
                    (
                        Ident::from("predicate"),
                        Term::Record(RecordData::with_field_values(predicates)).into(),
                    ),
                ]
                .into_iter()
                .collect(),
            ))
            .into(),
            inner,
            LetAttrs {
                rec: true,
                ..Default::default()
            },
        )
        .into()
    }

    pub fn references(&self) -> &References {
        &self.accesses
    }
}

impl From<&BTreeMap<String, Schema>> for Environment {
    fn from(defs: &BTreeMap<String, Schema>) -> Self {
        let accesses = References::with_fields(defs.keys());
        let terms = defs
            .iter()
            .map(|(name, schema)| {
                let predicate = schema_to_predicate(&accesses, schema);
                (
                    name.clone(),
                    Terms {
                        contract: schema_to_contract(&accesses, schema).unwrap_or_else(|| {
                            contract_from_predicate(
                                accesses
                                    .get_predicate(name)
                                    .expect("accesses has the correct keys by construction"),
                            )
                        }),
                        predicate,
                    },
                )
            })
            .collect();
        Environment { accesses, terms }
    }
}
