use std::collections::BTreeMap;

use nickel_lang_core::{
    identifier::Ident,
    mk_app,
    term::{array::Array, make, record::RecordData, Number, RichTerm, Term},
};
use schemars::schema::{
    ArrayValidation, InstanceType, NumberValidation, ObjectValidation, Schema, SchemaObject,
    SingleOrVec, StringValidation, SubschemaValidation,
};
use serde_json::Value;

use crate::{definitions, utils::static_access};

pub trait AsPredicate {
    fn as_predicate(self) -> RichTerm;
}

pub trait AsPredicates {
    // TODO: Turn Vec<_> into Iterator<Item = _> when RPTIT is implemented
    // SEE: https://rust-lang.github.io/impl-trait-initiative/RFCs/rpit-in-traits.html
    fn as_predicates(self) -> Vec<RichTerm>;
}

fn or_always(s: Option<&Schema>) -> RichTerm {
    s.map(|s| s.as_predicate())
        .unwrap_or(static_access("predicates", ["always"]))
}

impl AsPredicate for InstanceType {
    fn as_predicate(self) -> RichTerm {
        let type_tag = match self {
            InstanceType::Null => Term::Enum("Null".into()),
            InstanceType::Boolean => Term::Enum("Bool".into()),
            InstanceType::Object => Term::Enum("Record".into()),
            InstanceType::Array => Term::Enum("Array".into()),
            InstanceType::Number => Term::Enum("Number".into()),
            InstanceType::String => Term::Enum("String".into()),
            InstanceType::Integer => Term::Enum("Integer".into()),
        };
        mk_app!(static_access("predicates", ["isType"]), type_tag)
    }
}

impl AsPredicate for &SingleOrVec<InstanceType> {
    fn as_predicate(self) -> RichTerm {
        match self {
            SingleOrVec::Single(t) => t.as_predicate(),
            SingleOrVec::Vec(ts) => mk_app!(
                static_access("predicates", ["anyOf"]),
                Term::Array(
                    Array::new(ts.iter().map(|t| t.as_predicate()).collect()),
                    Default::default()
                )
            ),
        }
    }
}

/// XXX enum
impl AsPredicate for &[Value] {
    fn as_predicate(self) -> RichTerm {
        mk_app!(
            static_access("predicates", ["enum"]),
            Term::Array(
                Array::new(
                    self.iter()
                        .map(|v| serde_json::from_value(v.clone()).unwrap())
                        .collect()
                ),
                Default::default()
            )
        )
    }
}

impl AsPredicate for &Value {
    fn as_predicate(self) -> RichTerm {
        Term::App(
            static_access("predicates", ["const"]),
            serde_json::from_value(self.clone()).unwrap(),
        )
        .into()
    }
}

fn mk_all_of(schemas: impl IntoIterator<Item = RichTerm>) -> RichTerm {
    let schemas: Vec<_> = schemas.into_iter().collect();
    match schemas.as_slice() {
        [] => static_access("predicates", ["always"]),
        [t] => t.clone(),
        _ => mk_app!(
            static_access("predicates", ["allOf"]),
            Term::Array(Array::from_iter(schemas), Default::default())
        ),
    }
}

fn mk_any_of(schemas: impl IntoIterator<Item = RichTerm>) -> RichTerm {
    let schemas: Vec<_> = schemas.into_iter().collect();
    match schemas.as_slice() {
        [] => static_access("predicates", ["always"]),
        [t] => t.clone(),
        _ => mk_app!(
            static_access("predicates", ["anyOf"]),
            Term::Array(Array::from_iter(schemas), Default::default())
        ),
    }
}

impl AsPredicates for &SubschemaValidation {
    fn as_predicates(self) -> Vec<RichTerm> {
        let SubschemaValidation {
            all_of,
            any_of,
            one_of,
            not,
            if_schema,
            then_schema,
            else_schema,
        } = self;

        let all_of = all_of
            .as_deref()
            .map(|schemas| mk_all_of(schemas.iter().map(|s| s.as_predicate())))
            .into_iter();

        let any_of = any_of
            .as_deref()
            .map(|schemas| mk_any_of(schemas.iter().map(|s| s.as_predicate())))
            .into_iter();

        let one_of = one_of
            .as_deref()
            .map(|schemas| {
                mk_app!(
                    static_access("predicates", ["oneOf"]),
                    Term::Array(
                        Array::new(schemas.iter().map(|schema| schema.as_predicate()).collect()),
                        Default::default()
                    )
                )
            })
            .into_iter();

        let not = not
            .as_deref()
            .map(|s| mk_app!(static_access("predicates", ["not"]), s.as_predicate()))
            .into_iter();

        let ite = if_schema
            .as_deref()
            .map(move |if_schema| {
                mk_app!(
                    static_access("predicates", ["ifThenElse"]),
                    if_schema.as_predicate(),
                    or_always(then_schema.as_deref()),
                    or_always(else_schema.as_deref())
                )
            })
            .into_iter();

        all_of
            .chain(any_of)
            .chain(one_of)
            .chain(not)
            .chain(ite)
            .collect()
    }
}

impl AsPredicates for &NumberValidation {
    fn as_predicates(self) -> Vec<RichTerm> {
        let NumberValidation {
            multiple_of,
            maximum,
            exclusive_maximum,
            minimum,
            exclusive_minimum,
        } = self;

        fn predicate(s: &str) -> impl '_ + FnOnce(f64) -> RichTerm {
            move |n| {
                mk_app!(
                    static_access("predicates", ["numbers", s]),
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
            .collect()
    }
}

impl AsPredicates for &StringValidation {
    fn as_predicates(self) -> Vec<RichTerm> {
        let StringValidation {
            max_length,
            min_length,
            pattern,
        } = self;

        let max_length = max_length
            .map(|n| {
                mk_app!(
                    static_access("predicates", ["strings", "maxLength"]),
                    Term::Num(n.into())
                )
            })
            .into_iter();

        let min_length = min_length
            .map(|n| {
                mk_app!(
                    static_access("predicates", ["strings", "minLength"]),
                    Term::Num(n.into())
                )
            })
            .into_iter();

        let pattern = pattern
            .as_deref()
            .map(|s| {
                mk_app!(
                    static_access("predicates", ["strings", "pattern"]),
                    make::string(s)
                )
            })
            .into_iter();

        max_length.chain(min_length).chain(pattern).collect()
    }
}

impl AsPredicates for &ArrayValidation {
    fn as_predicates(self) -> Vec<RichTerm> {
        let ArrayValidation {
            items,
            additional_items,
            max_items,
            min_items,
            unique_items,
            contains,
        } = self;

        let items = match items {
            None => vec![],
            Some(SingleOrVec::Single(s)) => vec![mk_app!(
                static_access("predicates", ["arrays", "arrayOf"]),
                s.as_predicate()
            )],
            Some(SingleOrVec::Vec(schemas)) => {
                let len = schemas.len();
                [mk_app!(
                    static_access("predicates", ["arrays", "items"]),
                    Term::Array(
                        Array::new(schemas.iter().map(|x| x.as_predicate()).collect()),
                        Default::default()
                    )
                )]
                .into_iter()
                .chain(additional_items.as_deref().map(|s| {
                    mk_app!(
                        static_access("predicates", ["arrays", "additionalItems"]),
                        s.as_predicate(),
                        Term::Num(len.into())
                    )
                }))
                .collect()
            }
        }
        .into_iter();

        let max_items = max_items
            .map(|n| {
                mk_app!(
                    static_access("predicates", ["arrays", "maxItems"]),
                    Term::Num(n.into())
                )
            })
            .into_iter();

        let min_items = min_items
            .map(|n| {
                mk_app!(
                    static_access("predicates", ["arrays", "minItems"]),
                    Term::Num(n.into())
                )
            })
            .into_iter();

        let unique_items = unique_items
            .and_then(|unique| {
                unique.then_some(static_access("predicates", ["arrays", "uniqueItems"]))
            })
            .into_iter();

        let contains = contains
            .as_deref()
            .map(|s| {
                mk_app!(
                    static_access("predicates", ["arrays", "contains"]),
                    s.as_predicate()
                )
            })
            .into_iter();

        items
            .chain(max_items)
            .chain(min_items)
            .chain(unique_items)
            .chain(contains)
            .collect()
    }
}

impl AsPredicates for &ObjectValidation {
    fn as_predicates(self) -> Vec<RichTerm> {
        let ObjectValidation {
            max_properties,
            min_properties,
            required,
            properties,
            pattern_properties,
            additional_properties,
            property_names,
        } = self;

        let max_properties = max_properties
            .map(|n| {
                mk_app!(
                    static_access("predicates", ["records", "maxProperties"]),
                    Term::Num(n.into())
                )
            })
            .into_iter();

        let min_properties = min_properties
            .map(|n| {
                mk_app!(
                    static_access("predicates", ["records", "minProperties"]),
                    Term::Num(n.into())
                )
            })
            .into_iter();

        let property_names = property_names
            .as_deref()
            .map(|s| {
                mk_app!(
                    static_access("predicates", ["records", "propertyNames"]),
                    s.as_predicate()
                )
            })
            .into_iter();

        let required = {
            if required.is_empty() {
                None
            } else {
                Some(mk_app!(
                    static_access("predicates", ["records", "required"]),
                    Term::Array(
                        Array::new(required.iter().map(make::string).collect()),
                        Default::default()
                    )
                ))
            }
        }
        .into_iter();

        let record = [mk_app!(
            static_access("predicates", ["records", "record"]),
            Term::Record(RecordData::with_field_values(
                properties
                    .iter()
                    .map(|(k, v)| (k.into(), v.as_predicate()))
                    .collect()
            )),
            Term::Record(RecordData::with_field_values(
                pattern_properties
                    .iter()
                    .map(|(k, v)| (k.into(), v.as_predicate()))
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
            .collect()
    }
}

fn dependencies(extensions: &BTreeMap<String, Value>) -> impl Iterator<Item = RichTerm> {
    extensions
        .get("dependencies")
        .and_then(|v| v.as_object())
        .map(|deps| {
            mk_app!(
                static_access("predicates", ["records", "dependencies"]),
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
                                    Array::new(fields.into_iter().map(make::string).collect()),
                                    Default::default(),
                                )
                                .into()
                            } else {
                                serde_json::from_value::<Schema>(value.clone())
                                    .map(|s| s.as_predicate())
                                    .unwrap()
                            }
                        ))
                        .collect()
                ))
            )
        })
        .into_iter()
}

impl AsPredicate for &SchemaObject {
    fn as_predicate(self) -> RichTerm {
        // NOTE: You may naively think that, for instance, numbers and strings are
        // mutually exclusive. Not to a json schema! Any number of these fields can
        // be set and the semantics is they get ANDed together, even if that can
        // never pass.
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
        } = self;
        mk_all_of(
            instance_type
                .iter()
                .map(|x| x.as_predicate())
                .chain(enum_values.as_deref().map(|x| x.as_predicate()))
                .chain(const_value.as_ref().map(|x| x.as_predicate()))
                .chain(subschemas.iter().flat_map(|x| x.as_predicates()))
                .chain(number.iter().flat_map(|x| x.as_predicates()))
                .chain(string.iter().flat_map(|x| x.as_predicates()))
                .chain(array.iter().flat_map(|x| x.as_predicates()))
                .chain(object.iter().flat_map(|x| x.as_predicates()))
                .chain(
                    reference
                        .as_deref()
                        .map(|r| definitions::reference(r).predicate),
                )
                // schema.rs parses dependencies incorrectly. It should really be
                // part of object validation (object_predicates()) but it gets put
                // in extensions instead.
                .chain(dependencies(extensions)),
        )
    }
}

impl AsPredicate for &Schema {
    fn as_predicate(self) -> RichTerm {
        match self {
            Schema::Bool(true) => static_access("predicates", ["always"]),
            Schema::Bool(false) => static_access("predicates", ["never"]),
            Schema::Object(o) => o.as_predicate(),
        }
    }
}
