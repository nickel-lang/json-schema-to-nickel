//! # Reference handling for JSON schema
//!
//! JSON schemas can contain a set of definitions at the top level and
//! references to other schemas at essentially arbitrary points. The general
//! format of JSON schema references is quite general. For example, it would be
//! possible to reference fields in a schema hosted at a remote URI. We don't
//! want to support the general case but we need a way of dealing with
//! references to top-level definitions in a schema.
//! This module handles an [`Environment`] data structure that keeps track of
//! top-level definitions in a JSON schema and their translations into Nickel
//! predicates and contracts.

use std::collections::{BTreeMap, HashMap};

use nickel_lang_core::{
    identifier::Ident,
    term::{record::RecordData, LetAttrs, RichTerm, Term},
};
use schemars::schema::Schema;

use crate::{
    contracts::{contract_from_predicate, TryAsContract},
    predicates::AsPredicate,
    utils::static_access,
};

/// The nickel predicate and contract generated for a schema.
#[derive(Clone)]
pub struct ConvertedSchema {
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

/// An environment of top level schema definitions and their conversions into
/// Nickel predicates and contracts.
#[derive(Clone, Default)]
pub struct Environment(HashMap<String, ConvertedSchema>);

pub fn access(name: impl AsRef<str>) -> Access {
    Access {
        contract: static_access("definitions", ["contract", name.as_ref()]),
        predicate: static_access("definitions", ["predicate", name.as_ref()]),
    }
}

pub fn reference(reference: &str) -> Access {
    if let Some(remainder) = reference.strip_prefix("#/definitions/") {
        access(remainder)
    } else {
        unimplemented!()
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
            .0
            .iter()
            .map(|(k, v)| (Ident::from(k), v.contract.clone()))
            .collect();
        let predicates = self
            .0
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
}

/// Convert the `definitions` field of a json schema mapping identifiers to
/// Schemas to an [`Environment`] struct mapping identifiers to Nickel terms
// FIXME: Definitions can have their own definitions. Does this handle that
//        correctly? Does schema.rs even handle it correctly?
impl From<&BTreeMap<String, Schema>> for Environment {
    fn from(defs: &BTreeMap<String, Schema>) -> Self {
        let terms = defs
            .iter()
            .map(|(name, schema)| {
                let predicate = schema.as_predicate();
                (
                    name.clone(),
                    ConvertedSchema {
                        contract: schema
                            .try_as_contract()
                            .unwrap_or_else(|| contract_from_predicate(access(name).predicate)),
                        predicate,
                    },
                )
            })
            .collect();
        Environment(terms)
    }
}
