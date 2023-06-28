use nickel_lang::{
    identifier::Ident,
    mk_app,
    term::{
        array::Array,
        make,
        record::{Field, RecordData},
        LetAttrs, Number, RichTerm, Term,
    },
};
use schemars::schema::{
    ArrayValidation, InstanceType, NumberValidation, ObjectValidation, RootSchema, Schema,
    SchemaObject, SingleOrVec, StringValidation, SubschemaValidation,
};
use serde_json::Value;

// fn types(t: TypeF<Box<Types>, RecordRows, EnumRows>) -> Types {
//     Types {
//         types: t,
//         pos: TermPos::None,
//     }
// }

// fn instance_to_contract(instance: InstanceType) -> RichTerm {
//     match instance {
//         InstanceType::Number => Term::Var("Number".into()).into(),
//         InstanceType::Boolean => Term::Var("Bool".into()).into(),
//         InstanceType::String => Term::Var("String".into()).into(),
//         InstanceType::Integer => Term::Var("std.number.Integer".into()).into(),
//         InstanceType::Null => mk_fun!(
//             "label",
//             "value",
//             mk_app!(
//                 make::op1(
//                     UnaryOp::Ite(),
//                     make::op2(BinaryOp::Eq(), make::var("value"), Term::Null)
//                 ),
//                 make::var("value"),
//                 mk_app!(
//                     Term::Var("std.contract.blame_with".into()),
//                     make::string("expected null"),
//                     make::var("label")
//                 )
//             )
//         ),
//         InstanceType::Object => Term::Record(RecordData {
//             attrs: RecordAttrs { open: true },
//             ..RecordData::empty()
//         })
//         .into(),
//         InstanceType::Array => mk_app!(Term::Var("Array".into()), Term::Var("Dyn".into())),
//     }
// }

fn or_always(s: Option<Box<Schema>>) -> RichTerm {
    s.map(|s| schema_to_predicate(*s))
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

fn types_to_predicate(x: SingleOrVec<InstanceType>) -> RichTerm {
    match x {
        SingleOrVec::Single(t) => type_to_predicate(*t),
        SingleOrVec::Vec(ts) => mk_app!(
            make::var("predicates.anyOf"),
            Term::Array(
                Array::new(ts.into_iter().map(type_to_predicate).collect()),
                Default::default()
            )
        ),
    }
}

fn enum_to_predicate(vs: Vec<Value>) -> RichTerm {
    mk_app!(
        make::var("predicates.enum"),
        Term::Array(
            Array::new(
                vs.into_iter()
                    .map(|v| serde_json::from_value(v).unwrap())
                    .collect()
            ),
            Default::default()
        )
    )
}

fn const_to_predicate(v: Value) -> RichTerm {
    Term::App(
        make::var("predicates.const"),
        serde_json::from_value(v).unwrap(),
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

fn subschema_predicates(subschemas: SubschemaValidation) -> impl Iterator<Item = RichTerm> {
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
        .map(|schemas| mk_all_of(schemas.into_iter().map(schema_to_predicate)))
        .into_iter();

    let any_of = any_of
        .map(|schemas| mk_any_of(schemas.into_iter().map(schema_to_predicate)))
        .into_iter();

    let one_of = one_of
        .map(|schemas| {
            mk_app!(
                make::var("predicates.oneOf"),
                Term::Array(
                    Array::new(schemas.into_iter().map(schema_to_predicate).collect()),
                    Default::default()
                )
            )
        })
        .into_iter();

    let not = not
        .map(|s| mk_app!(make::var("predicates.not"), schema_to_predicate(*s)))
        .into_iter();

    let ite = if_schema
        .map(move |if_schema| {
            mk_app!(
                make::var("predicates.ifThenElse"),
                schema_to_predicate(*if_schema),
                or_always(then_schema),
                or_always(else_schema)
            )
        })
        .into_iter();

    all_of.chain(any_of).chain(one_of).chain(not).chain(ite)
}

fn number_predicates(nv: NumberValidation) -> impl Iterator<Item = RichTerm> {
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

fn string_predicates(sv: StringValidation) -> impl Iterator<Item = RichTerm> {
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
        .map(|s| mk_app!(make::var("predicates.strings.pattern"), Term::Str(s.into())))
        .into_iter();

    max_length.chain(min_length).chain(pattern)
}

fn array_predicates(av: ArrayValidation) -> impl Iterator<Item = RichTerm> {
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
            schema_to_predicate(*s)
        )],
        Some(SingleOrVec::Vec(schemas)) => {
            let len = schemas.len();
            [mk_app!(
                make::var("predicates.arrays.items"),
                Term::Array(
                    Array::new(schemas.into_iter().map(schema_to_predicate).collect()),
                    Default::default()
                )
            )]
            .into_iter()
            .chain(additional_items.map(|s| {
                mk_app!(
                    make::var("predicates.arrays.additionalItems"),
                    schema_to_predicate(*s),
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
        .map(|s| {
            mk_app!(
                make::var("predicates.arrays.contains"),
                schema_to_predicate(*s)
            )
        })
        .into_iter();

    items
        .chain(max_items)
        .chain(min_items)
        .chain(unique_items)
        .chain(contains)
}

fn object_predicates(ov: ObjectValidation) -> impl Iterator<Item = RichTerm> {
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

    let property_names = property_names.map(|_s| todo!()).into_iter();

    let required = {
        if required.is_empty() {
            None
        } else {
            Some(mk_app!(
                make::var("predicates.records.required"),
                Term::Array(
                    Array::new(
                        required
                            .into_iter()
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
                .into_iter()
                .map(|(k, v)| (k.into(), schema_to_predicate(v)))
                .collect()
        )),
        Term::Record(RecordData::with_field_values(
            pattern_properties
                .into_iter()
                .map(|(k, v)| (k.into(), schema_to_predicate(v)))
                .collect()
        )),
        Term::Bool(!matches!(
            additional_properties.as_deref(),
            Some(Schema::Bool(false))
        )),
        or_always(additional_properties)
    )]
    .into_iter();

    max_properties
        .chain(min_properties)
        .chain(property_names)
        .chain(required)
        .chain(record)
}

fn reference_to_predicate(reference: String) -> RichTerm {
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

pub fn schema_to_predicate(schema: Schema) -> RichTerm {
    let predicates: Vec<_> = match schema {
        Schema::Bool(true) => return make::var("predicates.always"),
        Schema::Bool(false) => return make::var("predicates.never"),
        Schema::Object(SchemaObject {
            metadata: _,
            instance_type,
            format: _, // TODO: deal with string formats
            enum_values,
            const_value,
            subschemas,
            number,
            string,
            array,
            object,
            reference,
            extensions: _,
        }) => instance_type
            .into_iter()
            .map(types_to_predicate)
            .chain(enum_values.map(enum_to_predicate))
            .chain(const_value.map(const_to_predicate))
            .chain(
                subschemas
                    .into_iter()
                    .flat_map(|s| subschema_predicates(*s)),
            )
            .chain(number.into_iter().flat_map(|nv| number_predicates(*nv)))
            .chain(string.into_iter().flat_map(|sv| string_predicates(*sv)))
            .chain(array.into_iter().flat_map(|av| array_predicates(*av)))
            .chain(object.into_iter().flat_map(|ov| object_predicates(*ov)))
            .chain(reference.map(reference_to_predicate)),
    }
    .collect();

    mk_all_of(predicates)
}

pub fn root_schema(root: RootSchema) -> RichTerm {
    let definitions = root
        .definitions
        .into_iter()
        .map(|(name, schema)| (Ident::from(name), Field::from(schema_to_predicate(schema))));

    Term::Let(
        "definitions".into(),
        Term::Record(RecordData {
            fields: definitions.collect(),
            attrs: Default::default(),
            sealed_tail: None,
        })
        .into(),
        schema_to_predicate(Schema::Object(root.schema)),
        LetAttrs {
            rec: true,
            ..Default::default()
        },
    )
    .into()
}
