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

use crate::{definitions, predicates::schema_to_predicate, utils::static_access};

/// Convert an [`InstanceType`] into a Nickel [`RichTerm`]. We're in a bit of a
/// bind here. When types appear in term position, they are parsed immediately
/// into another form (built-in contracts like `$bool`). These contracts would
/// get pretty-printed as-is (`$bool`), but `$`-identifiers are **only** valid
/// when generated internally in Nickel; the parser does not understand them.
/// In other words, the way types in term position are represented internally
/// cannot be pretty printed and parsed again. So here we use `Term::Var`. If
/// we passed this directly to Nickel as a `RichTerm`, it would be an error,
/// but  the pretty printer not understanding that builtin type names are not
/// valid identifiers.
fn type_to_contract(x: InstanceType) -> RichTerm {
    match x {
        InstanceType::Null => contract_from_predicate(mk_app!(
            static_access("predicates", ["isType"]),
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
        InstanceType::Integer => static_access("std", ["number", "Integer"]),
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
pub fn schema_object_to_contract(schema: &SchemaObject) -> Option<RichTerm> {
    // XXX explain weird pattern match
    // XXX put what these look like in a json schema
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
        } if extensions.is_empty() => return Some(definitions::reference(reference).contract),
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

    // XXX explain weird pattern match
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
            required,
            properties,
            is_open_record(additional_properties.as_deref()),
        )),
        _ => None,
    }
}

/// Convert a JSON schema into a Nickel record contract, when possible.
pub fn schema_to_contract(schema: &Schema) -> Option<RichTerm> {
    match schema {
        Schema::Bool(_) => None,
        Schema::Object(obj) => schema_object_to_contract(obj),
    }
}

fn generate_record_contract(
    required: &BTreeSet<String>,
    properties: &BTreeMap<String, Schema>,
    open: bool,
) -> RichTerm {
    let fields = properties.iter().map(|(name, schema)| {
        // XXX say why we don't just recurse immediately
        let contracts = match schema {
            Schema::Bool(false) => vec![LabeledType {
                types: TypeF::Flat(contract_from_predicate(static_access(
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
                } else if let Some(term) = schema_object_to_contract(obj) {
                    vec![LabeledType {
                        types: TypeF::Flat(term).into(),
                        label: Label::dummy(),
                    }]
                } else {
                    vec![LabeledType {
                        types: TypeF::Flat(contract_from_predicate(schema_to_predicate(schema)))
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
        static_access("predicates", ["contract_from_predicate"]),
        predicate
    )
}
