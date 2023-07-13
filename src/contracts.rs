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

use crate::{definitions, predicates::IntoPredicate, utils::static_access};

/// Convert to a Nickel [`RichtTerm`] representing a contract.
pub trait IntoContract {
    fn into_contract(self) -> RichTerm;
}

/// Convert to a Nickel [`RichtTerm`] representing a contract, if possible.
pub trait TryIntoContract {
    fn try_into_contract(self) -> Option<RichTerm>;
}

/// Convert to a Nickel [`LabeledType`]
pub trait IntoLabeledType {
    fn into_labeled_type(self) -> LabeledType;
}

/// Convert to a Nickel [`LabeledType`], if possible.
pub trait TryIntoLabeledType {
    fn try_into_labeled_type(self) -> Option<LabeledType>;
}

/// Convert an [`InstanceType`] into a Nickel [`RichTerm`]. We're in a bit of a
/// bind here. When types appear in term position, they are parsed immediately
/// into another form (built-in contracts like `$bool`). These contracts would
/// get pretty-printed as-is (`$bool`), but `$`-identifiers are **only** valid
/// when generated internally in Nickel; the parser does not understand them.
/// In other words, the way types in term position are represented internally
/// cannot be pretty printed and parsed again. So here we use `Term::Var`. If
/// we passed this directly to Nickel as a `RichTerm`, it would be an error,
/// but the pretty printer not understanding that builtin type names are not
/// valid identifiers.
impl IntoContract for InstanceType {
    fn into_contract(self) -> RichTerm {
        match self {
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
}

impl IntoLabeledType for InstanceType {
    fn into_labeled_type(self) -> LabeledType {
        let types = match self {
            InstanceType::Boolean => TypeF::Bool.into(),
            InstanceType::Array => TypeF::Array(Box::new(Types::from(TypeF::Dyn))).into(),
            InstanceType::Number => TypeF::Number.into(),
            InstanceType::String => TypeF::String.into(),
            InstanceType::Null | InstanceType::Object | InstanceType::Integer => {
                TypeF::Flat(self.into_contract()).into()
            }
        };
        LabeledType {
            types,
            label: Label::dummy(),
        }
    }
}

impl TryIntoLabeledType for &SchemaObject {
    fn try_into_labeled_type(self) -> Option<LabeledType> {
        match self {
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
            } if extensions.is_empty() => Some(instance_type.into_labeled_type()),
            _ => None,
        }
    }
}

impl TryIntoLabeledType for &Schema {
    fn try_into_labeled_type(self) -> Option<LabeledType> {
        match self {
            Schema::Bool(_) => None,
            Schema::Object(obj) => obj.try_into_labeled_type(),
        }
    }
}

impl TryIntoContract for &ObjectValidation {
    fn try_into_contract(self) -> Option<RichTerm> {
        fn is_open_record(additional: Option<&Schema>) -> bool {
            match additional {
                Some(Schema::Bool(open)) => *open,
                None => true,
                _ => unreachable!("additional_properties must be checked beforehand"),
            }
        }

        // box / deref patterns aren't stabilized, so we have to separate out
        // `additional_properties` as a separate pattern
        // SEE: https://github.com/rust-lang/rust/issues/29641
        // SEE: https://github.com/rust-lang/rust/issues/87121
        match (self, self.additional_properties.as_deref()) {
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
}

impl TryIntoContract for &SchemaObject {
    fn try_into_contract(self) -> Option<RichTerm> {
        match self {
            // a reference to a definition
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
            } if extensions.is_empty() => Some(definitions::reference(reference).contract),
            // a freeform record
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
                object: None,
                reference: None,
                extensions,
            } if **instance_type == InstanceType::Object && extensions.is_empty() => Some(
                Term::Record(RecordData {
                    attrs: RecordAttrs { open: true },
                    ..Default::default()
                })
                .into(),
            ),
            // a record with sub-field types specified
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
                object: Some(ov),
                reference: None,
                extensions,
            } if **instance_type == InstanceType::Object && extensions.is_empty() => {
                ov.as_ref().try_into_contract()
            }
            _ => None,
        }
    }
}

impl TryIntoContract for &Schema {
    fn try_into_contract(self) -> Option<RichTerm> {
        match self {
            Schema::Bool(_) => None,
            Schema::Object(obj) => obj.try_into_contract(),
        }
    }
}

impl IntoLabeledType for RichTerm {
    fn into_labeled_type(self) -> LabeledType {
        LabeledType {
            types: TypeF::Flat(self).into(),
            label: Label::dummy(),
        }
    }
}

fn generate_record_contract(
    required: &BTreeSet<String>,
    properties: &BTreeMap<String, Schema>,
    open: bool,
) -> RichTerm {
    let fields = properties.iter().map(|(name, schema)| {
        let contracts = if let Schema::Bool(true) = schema {
            // record fields where anything is allowed should look like
            // { a, b } not { a | predicates.always, b | predicates.always }
            vec![]
        } else if let Some(t) = schema.try_into_labeled_type() {
            vec![t]
        } else if let Some(term) = schema.try_into_contract() {
            vec![term.into_labeled_type()]
        } else {
            vec![contract_from_predicate(schema.into_predicate()).into_labeled_type()]
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
