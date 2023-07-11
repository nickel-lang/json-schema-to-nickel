//! # Nickel contract generation for certain JSON schemas
//!
//! Since generating lazy Nickel contracts for arbitrary JSON schemas is
//! impossible, this module restricts itself to generating record contracts for
//! JSON schemas that are simple enough. A JSON schema can be successfully
//! turned into a record contract if it takes the form
//!
//! ```json
//! {
//!   "type": "object",
//!   "required": ...,
//!   "properties": {
//!     ...
//!   }
//! }
//! ```
//!
//! Schemas specifying a single primitive type are turned into proper contracts
//! as well, for example
//!
//! ```json
//! {
//!   "type": "boolean"
//! }
//! ```
//!
//! is turned into the Nickel type `Bool`.
use std::collections::{BTreeMap, BTreeSet};

use nickel_lang_core::{
    label::Label,
    mk_app,
    term::{
        make,
        record::{Field, FieldMetadata, RecordAttrs, RecordData},
        LabeledType, RichTerm, Term, TypeAnnotation,
    },
    types::{TypeF, Types},
};
use schemars::schema::{InstanceType, ObjectValidation, Schema, SchemaObject, SingleOrVec};

use crate::{definitions::References, predicates::schema_to_predicate, utils::static_access_};

/// Convert an [`InstanceType`] into a Nickel [`RichTerm`]. Currently,
/// `RichTerm` doesn't include a variant for embedding Nickel types into terms.
/// Instead, in Nickel itself, the parser turns types that appear in term
/// position into their contracts immediately. We could do the same thing here,
/// but the resulting `RichTerm` would not be pretty printed in a useful way.
/// Instead, the various internal contracts used in the Nickel interpreter would
/// appear inlined into the term. To address this problem, we use `Term::Var`
/// and rely on the pretty printer not understanding that builtin type names are
/// not valid identifiers.
fn type_to_contract(x: InstanceType) -> RichTerm {
    match x {
        InstanceType::Null => contract_from_predicate(mk_app!(
            static_access_("predicates", ["isType"]),
            Term::Enum("Null".into())
        )),
        InstanceType::Boolean => make::var("Bool"),
        InstanceType::Object => Term::Record(RecordData {
            attrs: RecordAttrs { open: true },
            ..Default::default()
        })
        .into(),
        InstanceType::Array => mk_app!(make::var("Array"), make::var("Dyn")),
        InstanceType::Number => make::var("Number"),
        InstanceType::String => make::var("String"),
        InstanceType::Integer => static_access_("std", ["number", "Integer"]),
    }
}

fn type_to_nickel_type(x: InstanceType) -> LabeledType {
    let types = match x {
        InstanceType::Boolean => TypeF::Bool.into(),
        InstanceType::Array => TypeF::Array(Box::new(Types::from(TypeF::Dyn))).into(),
        InstanceType::Number => TypeF::Number.into(),
        InstanceType::String => TypeF::String.into(),
        InstanceType::Null | InstanceType::Object | InstanceType::Integer => {
            TypeF::Flat(type_to_contract(x)).into()
        }
    };
    LabeledType {
        types,
        label: Label::dummy(),
    }
}

/// Convert a JSON schema object into a Nickel [`LabeledType`], when possible.
pub fn schema_object_to_nickel_type(schema: &SchemaObject) -> Option<LabeledType> {
    match schema {
        SchemaObject {
            metadata: _,
            instance_type: Some(SingleOrVec::Single(instance_type)),
            format: _,
            enum_values: None,
            const_value: None,
            subschemas: None,
            number: None,
            string: None,
            array: None,
            object: None,
            reference: None, /* TODO(vkleen): We should be able to relax this once we properly
                              * track references */
            extensions,
        } if extensions.is_empty() => Some(type_to_nickel_type(**instance_type)),
        _ => None,
    }
}

/// Convert a JSON schema object into a record contract, when possible.
pub fn schema_object_to_contract(env: &References, schema: &SchemaObject) -> Option<RichTerm> {
    let Some(ov) = (match schema {
        SchemaObject {
            metadata: _,
            instance_type: Some(SingleOrVec::Single(instance_type)),
            format: None,
            enum_values: None,
            const_value: None,
            subschemas: None,
            number: None,
            string: None,
            array: None,
            object: ov,
            reference: None,
            extensions,
        } if **instance_type == InstanceType::Object && extensions.is_empty() => ov,
        SchemaObject {
            metadata: _,
            instance_type: None,
            format: None,
            enum_values: None,
            const_value: None,
            subschemas: None,
            number: None,
            string: None,
            array: None,
            object: None,
            reference: Some(reference),
            extensions,
        } if extensions.is_empty() => return Some(env.reference(reference).contract.clone()),
        _ => return None,
    }) else {
        return Some(
            Term::Record(RecordData {
                attrs: RecordAttrs { open: true },
                ..Default::default()
            })
            .into(),
        );
    };

    fn is_open_record(additional: Option<&Schema>) -> bool {
        match additional {
            Some(Schema::Bool(open)) => *open,
            None => true,
            _ => unreachable!("additional_properties must be checked beforehand"),
        }
    }

    match (ov.as_ref(), ov.additional_properties.as_deref()) {
        (
            ObjectValidation {
                max_properties: None,
                min_properties: None,
                required,
                properties,
                pattern_properties,
                additional_properties,
                property_names: None,
            },
            None | Some(Schema::Bool(_)),
        ) if pattern_properties.is_empty() => Some(generate_record_contract(
            env,
            required,
            properties,
            is_open_record(additional_properties.as_deref()),
        )),
        _ => None,
    }
}

/// Convert a JSON schema into a Nickel record contract, when possible.
pub fn schema_to_contract(env: &References, schema: &Schema) -> Option<RichTerm> {
    match schema {
        Schema::Bool(_) => None,
        Schema::Object(obj) => schema_object_to_contract(env, obj),
    }
}

fn generate_record_contract(
    env: &References,
    required: &BTreeSet<String>,
    properties: &BTreeMap<String, Schema>,
    open: bool,
) -> RichTerm {
    let fields = properties.iter().map(|(name, schema)| {
        let contracts = match schema {
            Schema::Bool(false) => vec![LabeledType {
                types: TypeF::Flat(contract_from_predicate(static_access_(
                    "predicates",
                    ["never"],
                )))
                .into(),
                label: Label::dummy(),
            }],
            Schema::Bool(true) => vec![],
            Schema::Object(obj) => {
                if let Some(t) = schema_object_to_nickel_type(obj) {
                    vec![t]
                } else if let Some(term) = schema_object_to_contract(env, obj) {
                    vec![LabeledType {
                        types: TypeF::Flat(term).into(),
                        label: Label::dummy(),
                    }]
                } else {
                    vec![LabeledType {
                        types: TypeF::Flat(contract_from_predicate(schema_to_predicate(
                            env, schema,
                        )))
                        .into(),
                        label: Label::dummy(),
                    }]
                }
            }
        };
        (
            name.into(),
            Field {
                metadata: FieldMetadata {
                    annotation: TypeAnnotation {
                        types: None,
                        contracts,
                    },
                    opt: !required.contains(name),
                    ..Default::default()
                },
                ..Default::default()
            },
        )
    });
    Term::Record(RecordData {
        fields: fields.collect(),
        attrs: RecordAttrs { open },
        ..Default::default()
    })
    .into()
}

/// Convert `predicate` into a contract, suitable for use in a contract
/// assertion `term | Contract`.
pub fn contract_from_predicate(predicate: RichTerm) -> RichTerm {
    mk_app!(
        static_access_("predicates", ["contract_from_predicate"]),
        predicate
    )
}
