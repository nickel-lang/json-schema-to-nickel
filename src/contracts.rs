//! # Nickel contract generation for JSON schemas
//!
//! Since generating lazy Nickel contracts for arbitrary JSON schemas is impossible, this module
//! restricts itself to generating record contracts for JSON schemas that are simple enough. A JSON
//! schema can be successfully turned into a record contract if it takes the form
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
use crate::references::RefUsageContext;
use nickel_lang_core::typ::EnumRowF;
use schemars::schema::RootSchema;
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

use crate::{
    predicates::{AsPredicate, Predicate},
    references::{self, RefsUsage},
    utils::static_access,
    PREDICATES_LIBRARY_ID,
};

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

impl Contract {
    /// Convert a root JSON schema to a contract. Returns `None` if the schema couldn't be
    /// converted to a (lazy) contract, and thus requires to go through a predicate.
    /// Upon success, returns the contract and the references used in the schema.
    pub fn from_root_schema(root_schema: &RootSchema) -> Option<(Self, RefsUsage)> {
        let mut refs_usage = RefsUsage::new();

        root_schema
            .schema
            .try_as_contract(&mut refs_usage)
            .map(|ctr| (ctr, refs_usage))
    }

    /// Return the `Dyn` contract, always succeeding.
    pub fn dynamic() -> Self {
        Term::Type(TypeF::Dyn.into()).into()
    }
}

/// [TryAsContract] is essentially like `TryInto<Contract>` but passes additional state around used for
/// effective reference resolution.
pub trait TryAsContract {
    /// Try to convert a JSON schema component `Self` to a contract. Returns `None` if the
    /// component couldn't be converted to a lazy contract, and thus requires to go through a
    /// predicate.
    ///
    /// `try_as_contract` will record the references used during the conversion through the `refs_usage` parameter.
    fn try_as_contract(&self, refs_usage: &mut RefsUsage) -> Option<Contract>;
}

pub trait AsPredicateContract {
    /// Convert a JSON schema to a contract by first converting it to a predicate, and then use
    /// json-schema-to-nickel's `from_predicate` helper. As opposed to [TryAsContract::try_as_contract], this
    /// conversion can't fail. However, it is less desirable (as it throws lazyness out of the
    /// window and is less LSP-friendly for e.g. completion), so we generally try to use
    /// [TryAsContract::try_as_contract] first.
    fn as_predicate_contract(&self, refs_usage: &mut RefsUsage) -> Contract;
}

impl<T> AsPredicateContract for T
where
    T: AsPredicate,
{
    fn as_predicate_contract(&self, refs_usage: &mut RefsUsage) -> Contract {
        Contract::from(self.as_predicate(refs_usage))
    }
}

impl TryAsContract for Schema {
    fn try_as_contract(&self, refs_usage: &mut RefsUsage) -> Option<Contract> {
        match self {
            Schema::Bool(true) => Some(Contract(vec![])),
            Schema::Bool(false) => None,
            Schema::Object(obj) => obj.try_as_contract(refs_usage),
        }
    }
}

impl TryAsContract for SchemaObject {
    fn try_as_contract(&self, refs_usage: &mut RefsUsage) -> Option<Contract> {
        match self {
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
                Some(Contract::from(instance_type.as_ref()))
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
            } if only_ignored_fields(extensions) => Some(Contract::from(references::resolve_ref(
                reference,
                refs_usage,
                RefUsageContext::Contract,
            ))),
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
                Some(Contract::from(Term::Record(RecordData {
                    attrs: RecordAttrs {
                        open: true,
                        ..Default::default()
                    },
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
                ov.try_as_contract(refs_usage)
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
                            let Value::String(id) = value else {
                                return None;
                            };

                            let row = EnumRowF {
                                id: id.into(),
                                typ: None,
                            };

                            Some(EnumRows(EnumRowsF::Extend {
                                row,
                                tail: Box::new(acc),
                            }))
                        })?;
                Some(Contract(vec![
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
            } if **instance_type == InstanceType::Array => av.try_as_contract(refs_usage),
            _ => None,
        }
    }
}

impl TryAsContract for ObjectValidation {
    fn try_as_contract(&self, refs_usage: &mut RefsUsage) -> Option<Contract> {
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
            ) if pattern_properties.is_empty() => Some(Contract::from(generate_record_contract(
                required,
                properties,
                is_open_record(additional_properties.as_deref()),
                refs_usage,
            ))),
            _ => None,
        }
    }
}

impl TryAsContract for ArrayValidation {
    fn try_as_contract(&self, refs_usage: &mut RefsUsage) -> Option<Contract> {
        if let ArrayValidation {
            items: Some(SingleOrVec::Single(s)),
            additional_items: None,
            max_items: None,
            min_items: None,
            unique_items: None,
            contains: None,
        } = self
        {
            let elt = s
                .try_as_contract(refs_usage)
                .unwrap_or_else(|| s.as_predicate_contract(refs_usage));
            if let [elt] = elt.0.as_slice() {
                Some(Contract::from(TypeF::Array(Box::new(
                    TypeF::Flat(elt.clone()).into(),
                ))))
            } else {
                None
            }
        } else {
            None
        }
    }
}

// The following conversions:
//
// 1. Are infallible
// 2. Don't do reference resolution
//
// We implement `From` directly instead of `TryConvert`.

impl From<RichTerm> for Contract {
    fn from(rt: RichTerm) -> Self {
        Contract(vec![rt])
    }
}

impl From<Contract> for RichTerm {
    fn from(Contract(c): Contract) -> Self {
        match c.as_slice() {
            [] => static_access(PREDICATES_LIBRARY_ID, ["always"]),
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
            InstanceType::Null => Contract::from(Predicate::from(mk_app!(
                static_access(PREDICATES_LIBRARY_ID, ["isType"]),
                Term::Enum("Null".into())
            ))),
            InstanceType::Boolean => Contract::from(TypeF::Bool),
            InstanceType::Object => Contract::from(Term::Record(RecordData {
                attrs: RecordAttrs {
                    open: true,
                    ..Default::default()
                },
                ..Default::default()
            })),
            InstanceType::Array => Contract::from(TypeF::Array(Box::new(TypeF::Dyn.into()))),
            InstanceType::Number => Contract::from(TypeF::Number),
            InstanceType::String => Contract::from(TypeF::String),
            InstanceType::Integer => Contract::from(static_access("std", ["number", "Integer"])),
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
    refs_usage: &mut RefsUsage,
) -> RichTerm {
    let fields = properties.iter().map(|(name, schema)| {
        // try to convert to a contract, otherwise convert the predicate version
        // to a contract
        let contract = schema
            .try_as_contract(refs_usage)
            .unwrap_or_else(|| schema.as_predicate_contract(refs_usage));
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
        attrs: RecordAttrs {
            open,
            ..Default::default()
        },
        ..Default::default()
    })
    .into()
}

impl From<Predicate> for Contract {
    // Convert a predicate to a contract by calling a function similar to
    // `std.contract.from_predicate` (but which does a bit more about propagating meaningful error
    // messages)
    fn from(pred: Predicate) -> Self {
        mk_app!(
            static_access(PREDICATES_LIBRARY_ID, ["contract_from_predicate"]),
            pred
        )
        .into()
    }
}
