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
        array::Array,
        record::{Field, FieldMetadata, RecordAttrs, RecordData},
        LabeledType, RichTerm, Term, TypeAnnotation,
    },
    typ::{EnumRows, EnumRowsF, RecordRows, Type, TypeF},
};
use schemars::schema::{
    ArrayValidation, InstanceType, ObjectValidation, Schema, SchemaObject, SingleOrVec,
};
use serde_json::Value;

use crate::{definitions, predicates::Predicate, utils::static_access};

fn only_ignored_fields<V>(extensions: &BTreeMap<String, V>) -> bool {
    const IGNORED_FIELDS: &[&str] = &["$comment"];
    !extensions
        .keys()
        .any(|x| !IGNORED_FIELDS.contains(&x.as_ref()))
}

/// [`Contract`] represents the set of contracts that would be applied to a
/// value. This can be empty or many as in `a` or `a | Foo | Bar`, but this
/// list can also be converted to a single value using `predicates.always` and
/// std.contract.Sequence
#[derive(Clone)]
pub struct Contract(Vec<RichTerm>);

impl From<RichTerm> for Contract {
    fn from(rt: RichTerm) -> Self {
        Contract(vec![rt])
    }
}

impl From<Contract> for RichTerm {
    fn from(Contract(c): Contract) -> Self {
        match c.as_slice() {
            [] => Predicate::from(&Schema::Bool(true)).into(),
            // TODO: shouldn't need to clone here
            [rt] => rt.clone(),
            _ => {
                let arr = Term::Array(Array::new(c.into_iter().collect()), Default::default());
                mk_app!(static_access("std", ["contract", "Sequence"]), arr)
            }
        }
    }
}

impl From<Term> for Contract {
    fn from(value: Term) -> Self {
        Contract::from(RichTerm::from(value))
    }
}

impl From<TypeF<Box<Type>, RecordRows, EnumRows>> for Contract {
    fn from(value: TypeF<Box<Type>, RecordRows, EnumRows>) -> Self {
        Contract::from(Term::Type(Type::from(value)))
    }
}

impl From<&InstanceType> for Contract {
    fn from(value: &InstanceType) -> Contract {
        match value {
            InstanceType::Null => contract_from_predicate(
                mk_app!(
                    static_access("predicates", ["isType"]),
                    Term::Enum("Null".into())
                )
                .into(),
            ),
            InstanceType::Boolean => Contract::from(TypeF::Bool),
            InstanceType::Object => Contract::from(Term::Record(RecordData {
                attrs: RecordAttrs { open: true },
                ..Default::default()
            })),
            InstanceType::Array => Contract::from(TypeF::Array(Box::new(TypeF::Dyn.into()))),
            InstanceType::Number => Contract::from(TypeF::Number),
            InstanceType::String => Contract::from(TypeF::String),
            InstanceType::Integer => Contract::from(static_access("std", ["number", "Integer"])),
        }
    }
}

impl TryFrom<&ObjectValidation> for Contract {
    type Error = ();

    fn try_from(value: &ObjectValidation) -> Result<Self, Self::Error> {
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
        match (value, value.additional_properties.as_deref()) {
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
            ) if pattern_properties.is_empty() => Ok(Contract::from(generate_record_contract(
                required,
                properties,
                is_open_record(additional_properties.as_deref()),
            ))),
            _ => Err(()),
        }
    }
}

impl TryFrom<&ArrayValidation> for Contract {
    type Error = ();

    fn try_from(val: &ArrayValidation) -> Result<Self, Self::Error> {
        if let ArrayValidation {
            items: Some(SingleOrVec::Single(s)),
            additional_items: None,
            max_items: None,
            min_items: None,
            unique_items: None,
            contains: None,
        } = val
        {
            let elt = Contract::try_from(s.as_ref())
                .unwrap_or_else(|_| contract_from_predicate(Predicate::from(s.as_ref())));
            if let [elt] = elt.0.as_slice() {
                Ok(Contract::from(TypeF::Array(Box::new(
                    TypeF::Flat(elt.clone()).into(),
                ))))
            } else {
                Err(())
            }
        } else {
            Err(())
        }
    }
}

impl TryFrom<&SchemaObject> for Contract {
    type Error = ();

    fn try_from(value: &SchemaObject) -> Result<Self, Self::Error> {
        match value {
            // a raw type
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
                reference: None,
                extensions,
            } if only_ignored_fields(extensions) => {
                // We ultimately produce a Flat type based on a contract with
                // only a type in it. Semantically, this is kind of weird. But
                // the pretty printer doesn't care, and it simplifies our code
                // significantly.
                Ok(Contract::from(instance_type.as_ref()))
            }
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
            } if only_ignored_fields(extensions) => {
                Ok(Contract::from(definitions::reference(reference).contract))
            }
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
            } if **instance_type == InstanceType::Object && only_ignored_fields(extensions) => {
                Ok(Contract::from(Term::Record(RecordData {
                    attrs: RecordAttrs { open: true },
                    ..Default::default()
                })))
            }
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
            } if **instance_type == InstanceType::Object && only_ignored_fields(extensions) => {
                ov.as_ref().try_into()
            }
            // Enum contract with all strings
            // => | std.enum.TagOrString | [| 'foo, 'bar, 'baz |]
            SchemaObject {
                metadata: _,
                instance_type: Some(SingleOrVec::Single(instance_type)),
                format: None,
                enum_values: Some(values),
                const_value: None,
                subschemas: None,
                number: None,
                string: None,
                array: None,
                object: None,
                reference: None,
                extensions: _,
            } if **instance_type == InstanceType::String => {
                let enum_rows: EnumRows =
                    values
                        .iter()
                        .try_fold(EnumRows(EnumRowsF::Empty), |acc, value| {
                            let id = match value {
                                Value::String(s) => s.into(),
                                _ => return Err(()),
                            };
                            Ok(EnumRows(EnumRowsF::Extend {
                                row: id,
                                tail: Box::new(acc),
                            }))
                        })?;
                Ok(Contract(vec![
                    static_access("std", ["enum", "TagOrString"]),
                    Term::Type(TypeF::Enum(enum_rows).into()).into(),
                ]))
            }
            SchemaObject {
                metadata: _,
                instance_type: Some(SingleOrVec::Single(instance_type)),
                format: None,
                enum_values: None,
                const_value: None,
                subschemas: None,
                number: None,
                string: None,
                array: Some(av),
                object: None,
                reference: None,
                extensions: _,
            } if **instance_type == InstanceType::Array => av.as_ref().try_into(),
            _ => Err(()),
        }
    }
}

impl TryFrom<&Schema> for Contract {
    type Error = ();

    fn try_from(value: &Schema) -> Result<Self, Self::Error> {
        match value {
            Schema::Bool(true) => Ok(Contract(vec![])),
            Schema::Bool(false) => Err(()),
            Schema::Object(obj) => obj.try_into(),
        }
    }
}

impl From<Contract> for TypeAnnotation {
    fn from(Contract(value): Contract) -> Self {
        TypeAnnotation {
            typ: None,
            contracts: value
                .into_iter()
                .map(|rt| LabeledType {
                    typ: TypeF::Flat(rt).into(),
                    label: Label::dummy(),
                })
                .collect(),
        }
    }
}

#[derive(Clone)]
pub struct Documentation(String);

impl From<Documentation> for String {
    fn from(value: Documentation) -> Self {
        value.0
    }
}

impl TryFrom<&SchemaObject> for Documentation {
    type Error = ();

    fn try_from(value: &SchemaObject) -> Result<Self, Self::Error> {
        match value {
            SchemaObject {
                metadata: Some(metadata),
                ..
            } => metadata.description.clone().map(Documentation).ok_or(()),
            _ => Err(()),
        }
    }
}

impl TryFrom<&Schema> for Documentation {
    type Error = ();

    fn try_from(value: &Schema) -> Result<Self, Self::Error> {
        match value {
            Schema::Bool(_) => Err(()),
            Schema::Object(obj) => obj.try_into(),
        }
    }
}

fn generate_record_contract(
    required: &BTreeSet<String>,
    properties: &BTreeMap<String, Schema>,
    open: bool,
) -> RichTerm {
    let fields = properties.iter().map(|(name, schema)| {
        // try to convert to a contract, otherwise convert the predicate version
        // to a contract
        let contract = Contract::try_from(schema)
            .unwrap_or_else(|()| contract_from_predicate(Predicate::from(schema)));
        let doc = Documentation::try_from(schema).ok();
        (
            name.into(),
            Field {
                metadata: FieldMetadata {
                    annotation: contract.into(),
                    opt: !required.contains(name),
                    doc: doc.map(String::from),
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
pub fn contract_from_predicate(predicate: Predicate) -> Contract {
    mk_app!(
        static_access("predicates", ["contract_from_predicate"]),
        predicate
    )
    .into()
}
