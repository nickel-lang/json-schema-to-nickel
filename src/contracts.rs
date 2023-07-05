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

use crate::{definitions::Environment, predicates::schema_to_predicate};

fn type_to_contract(x: InstanceType) -> RichTerm {
    match x {
        InstanceType::Null => mk_app!(
            make::var("predicates.contract_from_predicate"),
            mk_app!(make::var("predicates.isType"), Term::Enum("Null".into()))
        ),
        InstanceType::Boolean => make::var("Bool"),
        InstanceType::Object => Term::Record(RecordData {
            attrs: RecordAttrs { open: true },
            ..Default::default()
        })
        .into(),
        InstanceType::Array => mk_app!(make::var("Array"), make::var("Dyn")),
        InstanceType::Number => make::var("Number"),
        InstanceType::String => make::var("String"),
        InstanceType::Integer => make::var("std.number.Integer"),
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
            reference: None, // TODO(vkleen): We should be able to relax this once we properly track references
            extensions,
        } if extensions.is_empty() => Some(type_to_nickel_type(**instance_type)),
        _ => None,
    }
}

pub fn schema_object_to_contract(env: &Environment, schema: &SchemaObject) -> Option<RichTerm> {
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
            reference: None, // TODO(vkleen): We should be able to relax this once we properly track references
            extensions,
        } if **instance_type == InstanceType::Object && extensions.is_empty() => ov,
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

    fn open_record(additional: Option<&Schema>) -> bool {
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
            open_record(additional_properties.as_deref()),
        )),
        _ => None,
    }
}

fn generate_record_contract(
    env: &Environment,
    required: &BTreeSet<String>,
    properties: &BTreeMap<String, Schema>,
    open: bool,
) -> RichTerm {
    let fields = properties.iter().map(|(name, schema)| {
        let contracts = match schema {
            Schema::Bool(false) => vec![LabeledType {
                types: TypeF::Flat(mk_app!(
                    make::var("predicates.contract_from_predicate"),
                    make::var("predicates.never")
                ))
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
                        types: TypeF::Flat(mk_app!(
                            make::var("predicates.contract_from_predicate"),
                            schema_to_predicate(env, schema)
                        ))
                        .into(),
                        label: Label::dummy(),
                    }]
                }
            }
        };
        (
            name.into(),
            Field {
                value: None,
                metadata: FieldMetadata {
                    doc: None,
                    annotation: TypeAnnotation {
                        types: None,
                        contracts,
                    },
                    opt: !required.contains(name),
                    not_exported: false,
                    priority: Default::default(),
                },
                pending_contracts: Vec::new(),
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