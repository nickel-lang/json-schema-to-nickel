//! # Reference handling for JSON schema
//!
//! JSON schemas can contain a set of definitions at the top level and
//! references to other schemas at essentially arbitrary points. The general
//! format of JSON schema references is quite general. For example, it would be
//! possible to reference fields in a schema hosted at a remote URI. We don't
//! want to support the general case but we need a way of dealing with
//! references to top-level definitions in a schema.
//!
//! This module handles an [`Environment`] data structure that keeps track of
//! top-level definitions in a JSON schema and their mapping to [`RichTerm`]s
//! for accessing them.

use std::collections::{BTreeMap, HashMap};

use nickel_lang_core::{
    identifier::Ident,
    term::{make, record::RecordData, LetAttrs, RichTerm, Term},
};
use schemars::schema::Schema;

use crate::{
    contracts::{contract_from_predicate, schema_to_contract},
    predicates::schema_to_predicate,
};

/// The predicate and contract generated for a schema.
#[derive(Clone)]
pub struct Terms {
    predicate: RichTerm,
    contract: RichTerm,
}

/// The field access for referencing the predicate or contract generated from a
/// schema in other Nickel code.
#[derive(Clone)]
pub struct Access {
    pub predicate: RichTerm,
    pub contract: RichTerm,
}

/// A newtype around [`HashMap`] for keeping track of the Nickel code for
/// referencing top level schema definitions.
#[derive(Clone, Default)]
pub struct References(HashMap<String, Access>);

/// An environment of top level schema definitions and their conversions into
/// Nickel predicates and contracts.
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
                    let id = n.as_ref();
                    (
                        String::from(id),
                        Access {
                            contract: make::static_access_("definitions", ["contract", id]),
                            predicate: make::static_access_("definitions", ["predicate", id]),
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
    /// The empty environment
    pub fn empty() -> Self {
        Self::default()
    }

    /// Wrap a Nickel [`RichTerm`] in a let binding containing the definitions
    /// from the environment. This is necessary for the Nickel access terms
    /// tracked in the environment to actually work.
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

    /// Extract just the Nickel field references
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
