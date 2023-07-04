use std::collections::{BTreeMap, BTreeSet};

use nickel_lang_core::{
    identifier::Ident,
    label::Label,
    mk_app,
    term::{
        array::Array,
        make,
        record::{Field, FieldMetadata, RecordAttrs, RecordData},
        LabeledType, LetAttrs, Number, RichTerm, Term, TypeAnnotation,
    },
    types::{TypeF, Types},
};
use schemars::schema::{
    ArrayValidation, InstanceType, NumberValidation, ObjectValidation, RootSchema, Schema,
    SchemaObject, SingleOrVec, StringValidation, SubschemaValidation,
};
use serde_json::Value;

fn or_always(s: Option<&Schema>) -> RichTerm {
    s.map(schema_to_predicate)
        .unwrap_or(make::var("predicates.always"))
}

fn type_to_predicate(x: InstanceType) -> RichTerm {
    let type_tag = match x {
        InstanceType::Null => Term::Enum("Null".into()),
        InstanceType::Boolean => Term::Enum("Bool".into()),
        InstanceType::Object => Term::Enum("Record".into()),
        InstanceType::Array => Term::Enum("Array".into()),
        InstanceType::Number => Term::Enum("Number".into()),
        InstanceType::String => Term::Enum("String".into()),
        InstanceType::Integer => Term::Enum("Integer".into()),
    };
    mk_app!(make::var("predicates.isType"), type_tag)
}

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

fn types_to_predicate(x: &SingleOrVec<InstanceType>) -> RichTerm {
    match x {
        SingleOrVec::Single(t) => type_to_predicate(**t),
        SingleOrVec::Vec(ts) => mk_app!(
            make::var("predicates.anyOf"),
            Term::Array(
                Array::new(ts.iter().map(|t| type_to_predicate(*t)).collect()),
                Default::default()
            )
        ),
    }
}

fn enum_to_predicate(vs: &[Value]) -> RichTerm {
    mk_app!(
        make::var("predicates.enum"),
        Term::Array(
            Array::new(
                vs.iter()
                    .map(|v| serde_json::from_value(v.clone()).unwrap())
                    .collect()
            ),
            Default::default()
        )
    )
}

fn const_to_predicate(v: &Value) -> RichTerm {
    Term::App(
        make::var("predicates.const"),
        serde_json::from_value(v.clone()).unwrap(),
    )
    .into()
}

fn mk_all_of(schemas: impl IntoIterator<Item = RichTerm>) -> RichTerm {
    let schemas: Vec<_> = schemas.into_iter().collect();
    match schemas.as_slice() {
        [] => make::var("predicates.always"),
        [t] => t.clone(),
        _ => mk_app!(
            make::var("predicates.allOf"),
            Term::Array(Array::from_iter(schemas), Default::default())
        ),
    }
}

fn mk_any_of(schemas: impl IntoIterator<Item = RichTerm>) -> RichTerm {
    let schemas: Vec<_> = schemas.into_iter().collect();
    match schemas.as_slice() {
        [] => make::var("predicates.always"),
        [t] => t.clone(),
        _ => mk_app!(
            make::var("predicates.anyOf"),
            Term::Array(Array::from_iter(schemas), Default::default())
        ),
    }
}

fn subschema_predicates(subschemas: &SubschemaValidation) -> impl Iterator<Item = RichTerm> {
    let SubschemaValidation {
        all_of,
        any_of,
        one_of,
        not,
        if_schema,
        then_schema,
        else_schema,
    } = subschemas;

    let all_of = all_of
        .as_deref()
        .map(|schemas| mk_all_of(schemas.iter().map(schema_to_predicate)))
        .into_iter();

    let any_of = any_of
        .as_deref()
        .map(|schemas| mk_any_of(schemas.iter().map(schema_to_predicate)))
        .into_iter();

    let one_of = one_of
        .as_deref()
        .map(|schemas| {
            mk_app!(
                make::var("predicates.oneOf"),
                Term::Array(
                    Array::new(schemas.iter().map(schema_to_predicate).collect()),
                    Default::default()
                )
            )
        })
        .into_iter();

    let not = not
        .as_deref()
        .map(|s| mk_app!(make::var("predicates.not"), schema_to_predicate(s)))
        .into_iter();

    let ite = if_schema
        .as_deref()
        .map(move |if_schema| {
            mk_app!(
                make::var("predicates.ifThenElse"),
                schema_to_predicate(if_schema),
                or_always(then_schema.as_deref()),
                or_always(else_schema.as_deref())
            )
        })
        .into_iter();

    all_of.chain(any_of).chain(one_of).chain(not).chain(ite)
}

fn number_predicates(nv: &NumberValidation) -> impl Iterator<Item = RichTerm> {
    let NumberValidation {
        multiple_of,
        maximum,
        exclusive_maximum,
        minimum,
        exclusive_minimum,
    } = nv;

    fn predicate(s: &str) -> impl '_ + FnOnce(f64) -> RichTerm {
        move |n| {
            mk_app!(
                make::var(format!("predicates.numbers.{s}")),
                Term::Num(Number::try_from_float_simplest(n).unwrap())
            )
        }
    }

    let multiple_of = multiple_of.map(predicate("multipleOf")).into_iter();
    let maximum = maximum.map(predicate("maximum")).into_iter();
    let exclusive_maximum = exclusive_maximum
        .map(predicate("exclusiveMaximum"))
        .into_iter();
    let minimum = minimum.map(predicate("minimum")).into_iter();
    let exclusive_minimum = exclusive_minimum
        .map(predicate("exclusiveMinimum"))
        .into_iter();

    multiple_of
        .chain(maximum)
        .chain(exclusive_maximum)
        .chain(minimum)
        .chain(exclusive_minimum)
}

fn string_predicates(sv: &StringValidation) -> impl Iterator<Item = RichTerm> {
    let StringValidation {
        max_length,
        min_length,
        pattern,
    } = sv;

    let max_length = max_length
        .map(|n| {
            mk_app!(
                make::var("predicates.strings.maxLength"),
                Term::Num(n.into())
            )
        })
        .into_iter();

    let min_length = min_length
        .map(|n| {
            mk_app!(
                make::var("predicates.strings.minLength"),
                Term::Num(n.into())
            )
        })
        .into_iter();

    let pattern = pattern
        .as_deref()
        .map(|s| mk_app!(make::var("predicates.strings.pattern"), Term::Str(s.into())))
        .into_iter();

    max_length.chain(min_length).chain(pattern)
}

fn array_predicates(av: &ArrayValidation) -> impl Iterator<Item = RichTerm> {
    let ArrayValidation {
        items,
        additional_items,
        max_items,
        min_items,
        unique_items,
        contains,
    } = av;

    let items = match items {
        None => vec![],
        Some(SingleOrVec::Single(s)) => vec![mk_app!(
            make::var("predicates.arrays.arrayOf"),
            schema_to_predicate(s)
        )],
        Some(SingleOrVec::Vec(schemas)) => {
            let len = schemas.len();
            [mk_app!(
                make::var("predicates.arrays.items"),
                Term::Array(
                    Array::new(schemas.iter().map(schema_to_predicate).collect()),
                    Default::default()
                )
            )]
            .into_iter()
            .chain(additional_items.as_deref().map(|s| {
                mk_app!(
                    make::var("predicates.arrays.additionalItems"),
                    schema_to_predicate(s),
                    Term::Num(len.into())
                )
            }))
            .collect()
        }
    }
    .into_iter();

    let max_items = max_items
        .map(|n| mk_app!(make::var("predicates.arrays.maxItems"), Term::Num(n.into())))
        .into_iter();

    let min_items = min_items
        .map(|n| mk_app!(make::var("predicates.arrays.minItems"), Term::Num(n.into())))
        .into_iter();

    let unique_items = unique_items
        .and_then(|unique| unique.then_some(make::var("predicates.arrays.uniqueItems")))
        .into_iter();

    let contains = contains
        .as_deref()
        .map(|s| {
            mk_app!(
                make::var("predicates.arrays.contains"),
                schema_to_predicate(s)
            )
        })
        .into_iter();

    items
        .chain(max_items)
        .chain(min_items)
        .chain(unique_items)
        .chain(contains)
}

fn object_predicates(ov: &ObjectValidation) -> impl Iterator<Item = RichTerm> {
    let ObjectValidation {
        max_properties,
        min_properties,
        required,
        properties,
        pattern_properties,
        additional_properties,
        property_names,
    } = ov;

    let max_properties = max_properties
        .map(|n| {
            mk_app!(
                make::var("predicates.records.maxProperties"),
                Term::Num(n.into())
            )
        })
        .into_iter();

    let min_properties = min_properties
        .map(|n| {
            mk_app!(
                make::var("predicates.records.minProperties"),
                Term::Num(n.into())
            )
        })
        .into_iter();

    let property_names = property_names
        .as_deref()
        .map(|s| {
            mk_app!(
                make::var("predicates.records.propertyNames"),
                schema_to_predicate(s)
            )
        })
        .into_iter();

    let required = {
        if required.is_empty() {
            None
        } else {
            Some(mk_app!(
                make::var("predicates.records.required"),
                Term::Array(
                    Array::new(
                        required
                            .iter()
                            .map(|s| Term::Str(s.into()).into())
                            .collect()
                    ),
                    Default::default()
                )
            ))
        }
    }
    .into_iter();

    let record = [mk_app!(
        make::var("predicates.records.record"),
        Term::Record(RecordData::with_field_values(
            properties
                .iter()
                .map(|(k, v)| (k.into(), schema_to_predicate(v)))
                .collect()
        )),
        Term::Record(RecordData::with_field_values(
            pattern_properties
                .iter()
                .map(|(k, v)| (k.into(), schema_to_predicate(v)))
                .collect()
        )),
        Term::Bool(!matches!(
            additional_properties.as_deref(),
            Some(Schema::Bool(false))
        )),
        or_always(additional_properties.as_deref())
    )]
    .into_iter();

    max_properties
        .chain(min_properties)
        .chain(property_names)
        .chain(required)
        .chain(record)
}

fn reference_to_predicate(reference: &str) -> RichTerm {
    if let Some(remainder) = reference.strip_prefix("#/definitions/") {
        let escaped = remainder
            .replace('\\', "\\\\")
            .replace("%{", "\\%{")
            .replace('\"', "\\\"")
            .replace('\n', "\\n");
        make::var(format!("definitions.\"{escaped}\""))
    } else {
        unimplemented!()
    }
}

fn dependencies(extensions: &BTreeMap<String, Value>) -> impl Iterator<Item = RichTerm> {
    extensions
        .get("dependencies")
        .and_then(|v| v.as_object())
        .map(|deps| {
            mk_app!(
                make::var("predicates.records.dependencies"),
                Term::Record(RecordData::with_field_values(
                    deps.into_iter()
                        .map(|(key, value)| (
                            Ident::from(key),
                            if let Some(fields) = value.as_array().and_then(|v| v
                                .iter()
                                .map(|s| s.as_str())
                                .collect::<Option<Vec<_>>>())
                            {
                                Term::Array(
                                    Array::new(
                                        fields
                                            .into_iter()
                                            .map(|s| Term::Str(s.into()).into())
                                            .collect(),
                                    ),
                                    Default::default(),
                                )
                                .into()
                            } else {
                                serde_json::from_value::<Schema>(value.clone())
                                    .map(|s| schema_to_predicate(&s))
                                    .unwrap()
                            }
                        ))
                        .collect()
                ))
            )
        })
        .into_iter()
}

pub fn schema_object_to_predicate(o: &SchemaObject) -> RichTerm {
    let SchemaObject {
        metadata: _,
        instance_type,
        format: _, // TODO(vkleen): deal with string formats
        enum_values,
        const_value,
        subschemas,
        number,
        string,
        array,
        object,
        reference,
        extensions,
    } = o;
    mk_all_of(
        instance_type
            .iter()
            .map(types_to_predicate)
            .chain(enum_values.as_deref().map(enum_to_predicate))
            .chain(const_value.as_ref().map(const_to_predicate))
            .chain(subschemas.iter().flat_map(|s| subschema_predicates(s)))
            .chain(number.iter().flat_map(|nv| number_predicates(nv)))
            .chain(string.iter().flat_map(|sv| string_predicates(sv)))
            .chain(array.iter().flat_map(|av| array_predicates(av)))
            .chain(object.iter().flat_map(|ov| object_predicates(ov)))
            .chain(reference.as_deref().map(reference_to_predicate))
            .chain(dependencies(extensions)),
    )
}

pub fn schema_to_predicate(schema: &Schema) -> RichTerm {
    match schema {
        Schema::Bool(true) => make::var("predicates.always"),
        Schema::Bool(false) => make::var("predicates.never"),
        Schema::Object(o) => schema_object_to_predicate(o),
    }
}

pub fn root_schema(root: &RootSchema) -> RichTerm {
    let definitions = root
        .definitions
        .iter()
        .map(|(name, schema)| (Ident::from(name), Field::from(schema_to_predicate(schema))));
    if let Some(contract) = schema_object_to_contract(&root.schema) {
        wrap_contract(contract, definitions)
    } else {
        wrap_predicate(schema_object_to_predicate(&root.schema), definitions)
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

pub fn schema_object_to_contract(schema: &SchemaObject) -> Option<RichTerm> {
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
            required,
            properties,
            open_record(additional_properties.as_deref()),
        )),
        _ => None,
    }
}

fn generate_record_contract(
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
                } else if let Some(term) = schema_object_to_contract(obj) {
                    vec![LabeledType {
                        types: TypeF::Flat(term).into(),
                        label: Label::dummy(),
                    }]
                } else {
                    vec![LabeledType {
                        types: TypeF::Flat(mk_app!(
                            make::var("predicates.contract_from_predicate"),
                            schema_to_predicate(schema)
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
