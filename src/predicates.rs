use std::{collections::BTreeMap, iter};

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

#[derive(Clone)]
pub struct Predicate(RichTerm);

impl From<RichTerm> for Predicate {
    fn from(rt: RichTerm) -> Self {
        Predicate(rt)
    }
}

impl From<Term> for Predicate {
    fn from(t: Term) -> Self {
        Predicate(t.into())
    }
}

impl From<Predicate> for RichTerm {
    fn from(Predicate(rt): Predicate) -> Self {
        rt
    }
}

// Orphan rule means we have to wrap in a newtype in order to impl From
struct Predicates(Vec<Predicate>);

impl From<Predicates> for Vec<Predicate> {
    fn from(Predicates(preds): Predicates) -> Self {
        preds
    }
}

impl From<Vec<Predicate>> for Predicates {
    fn from(value: Vec<Predicate>) -> Self {
        Predicates(value)
    }
}

impl IntoIterator for Predicates {
    type Item = <Vec<Predicate> as IntoIterator>::Item;

    type IntoIter = <Vec<Predicate> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

fn or_always(s: Option<&Schema>) -> Predicate {
    s.map(Predicate::from)
        .unwrap_or(Predicate::from(static_access("predicates", ["always"])))
}

impl From<&InstanceType> for Predicate {
    fn from(value: &InstanceType) -> Self {
        let type_tag = match value {
            InstanceType::Null => Term::Enum("Null".into()),
            InstanceType::Boolean => Term::Enum("Bool".into()),
            InstanceType::Object => Term::Enum("Record".into()),
            InstanceType::Array => Term::Enum("Array".into()),
            InstanceType::Number => Term::Enum("Number".into()),
            InstanceType::String => Term::Enum("String".into()),
            InstanceType::Integer => Term::Enum("Integer".into()),
        };
        mk_app!(static_access("predicates", ["isType"]), type_tag).into()
    }
}

impl From<&SingleOrVec<InstanceType>> for Predicate {
    fn from(value: &SingleOrVec<InstanceType>) -> Self {
        match value {
            SingleOrVec::Single(t) => t.as_ref().into(),
            SingleOrVec::Vec(ts) => mk_app!(
                static_access("predicates", ["anyOf"]),
                Term::Array(
                    Array::new(ts.iter().map(|t| Predicate::from(t).into()).collect()),
                    Default::default()
                )
            )
            .into(),
        }
    }
}

/// Convert a json schema Enum to a predicate. Enums are represented by
/// `[Value]`s.
impl From<&[Value]> for Predicate {
    fn from(value: &[Value]) -> Self {
        mk_app!(
            static_access("predicates", ["enum"]),
            Term::Array(
                Array::new(
                    value
                        .iter()
                        .map(|v| serde_json::from_value(v.clone()).unwrap())
                        .collect()
                ),
                Default::default()
            )
        )
        .into()
    }
}

/// Convert a json schema Const to a predicate. Consts are represented by
/// `Value`s
impl From<&Value> for Predicate {
    fn from(value: &Value) -> Self {
        Term::App(
            static_access("predicates", ["const"]),
            serde_json::from_value(value.clone()).unwrap(),
        )
        .into()
    }
}

fn mk_all_of(preds: impl IntoIterator<Item = Predicate>) -> Predicate {
    let mut ps = preds.into_iter();
    match (ps.next(), ps.next()) {
        // []
        (None, _) => static_access("predicates", ["always"]).into(),
        // [p]
        (Some(p), None) => p,
        // ps
        (Some(p1), Some(p2)) => {
            // reconstruct the full iterator
            let ps = iter::once(p1).chain(iter::once(p2)).chain(ps);
            mk_app!(
                static_access("predicates", ["allOf"]),
                Term::Array(Array::from_iter(ps.map(RichTerm::from)), Default::default())
            )
            .into()
        }
    }
}

fn mk_any_of(preds: impl IntoIterator<Item = Predicate>) -> Predicate {
    let mut ps = preds.into_iter();
    match (ps.next(), ps.next()) {
        // []
        (None, _) => static_access("predicates", ["always"]).into(),
        // [p]
        (Some(p), None) => p,
        // ps
        (Some(p1), Some(p2)) => {
            // reconstruct the full iterator
            let ps = iter::once(p1).chain(iter::once(p2)).chain(ps);
            mk_app!(
                static_access("predicates", ["anyOf"]),
                Term::Array(Array::from_iter(ps.map(RichTerm::from)), Default::default())
            )
            .into()
        }
    }
}

impl From<&SubschemaValidation> for Predicates {
    fn from(
        SubschemaValidation {
            all_of,
            any_of,
            one_of,
            not,
            if_schema,
            then_schema,
            else_schema,
        }: &SubschemaValidation,
    ) -> Self {
        let all_of = all_of
            .as_deref()
            .map(|schemas| mk_all_of(schemas.iter().map(Predicate::from)))
            .into_iter();

        let any_of = any_of
            .as_deref()
            .map(|schemas| mk_any_of(schemas.iter().map(Predicate::from)))
            .into_iter();

        let one_of = one_of
            .as_deref()
            .map(|schemas| {
                mk_app!(
                    static_access("predicates", ["oneOf"]),
                    Term::Array(
                        Array::new(schemas.iter().map(|s| Predicate::from(s).into()).collect()),
                        Default::default()
                    )
                )
                .into()
            })
            .into_iter();

        let not = not
            .as_deref()
            .map(|s| mk_app!(static_access("predicates", ["not"]), Predicate::from(s)).into())
            .into_iter();

        let ite = if_schema
            .as_deref()
            .map(move |if_schema| {
                mk_app!(
                    static_access("predicates", ["ifThenElse"]),
                    Predicate::from(if_schema),
                    or_always(then_schema.as_deref()),
                    or_always(else_schema.as_deref())
                )
                .into()
            })
            .into_iter();

        Predicates(
            all_of
                .chain(any_of)
                .chain(one_of)
                .chain(not)
                .chain(ite)
                .collect(),
        )
    }
}

impl From<&NumberValidation> for Predicates {
    fn from(
        NumberValidation {
            multiple_of,
            maximum,
            exclusive_maximum,
            minimum,
            exclusive_minimum,
        }: &NumberValidation,
    ) -> Self {
        fn predicate(s: &str) -> impl '_ + FnOnce(f64) -> Predicate {
            move |n| {
                mk_app!(
                    static_access("predicates", ["numbers", s]),
                    Term::Num(Number::try_from_float_simplest(n).unwrap())
                )
                .into()
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

        Predicates(
            multiple_of
                .chain(maximum)
                .chain(exclusive_maximum)
                .chain(minimum)
                .chain(exclusive_minimum)
                .collect(),
        )
    }
}

impl From<&StringValidation> for Predicates {
    fn from(
        StringValidation {
            max_length,
            min_length,
            pattern,
        }: &StringValidation,
    ) -> Self {
        let max_length = max_length
            .map(|n| {
                mk_app!(
                    static_access("predicates", ["strings", "maxLength"]),
                    Term::Num(n.into())
                )
                .into()
            })
            .into_iter();

        let min_length = min_length
            .map(|n| {
                mk_app!(
                    static_access("predicates", ["strings", "minLength"]),
                    Term::Num(n.into())
                )
                .into()
            })
            .into_iter();

        let pattern = pattern
            .as_deref()
            .map(|s| {
                mk_app!(
                    static_access("predicates", ["strings", "pattern"]),
                    make::string(s)
                )
                .into()
            })
            .into_iter();

        Predicates(max_length.chain(min_length).chain(pattern).collect())
    }
}

impl From<&ArrayValidation> for Predicates {
    fn from(
        ArrayValidation {
            items,
            additional_items,
            max_items,
            min_items,
            unique_items,
            contains,
        }: &ArrayValidation,
    ) -> Self {
        let items = match items {
            None => vec![],
            Some(SingleOrVec::Single(s)) => vec![mk_app!(
                static_access("predicates", ["arrays", "arrayOf"]),
                Predicate::from(s.as_ref())
            )
            .into()],
            Some(SingleOrVec::Vec(schemas)) => {
                let len = schemas.len();
                [mk_app!(
                    static_access("predicates", ["arrays", "items"]),
                    Term::Array(
                        Array::new(schemas.iter().map(|x| Predicate::from(x).into()).collect()),
                        Default::default()
                    )
                )
                .into()]
                .into_iter()
                .chain(additional_items.as_deref().map(|s| {
                    mk_app!(
                        static_access("predicates", ["arrays", "additionalItems"]),
                        Predicate::from(s),
                        Term::Num(len.into())
                    )
                    .into()
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
                .into()
            })
            .into_iter();

        let min_items = min_items
            .map(|n| {
                mk_app!(
                    static_access("predicates", ["arrays", "minItems"]),
                    Term::Num(n.into())
                )
                .into()
            })
            .into_iter();

        let unique_items = unique_items
            .and_then(|unique| {
                unique.then_some(static_access("predicates", ["arrays", "uniqueItems"]).into())
            })
            .into_iter();

        let contains = contains
            .as_deref()
            .map(|s| {
                mk_app!(
                    static_access("predicates", ["arrays", "contains"]),
                    Predicate::from(s)
                )
                .into()
            })
            .into_iter();

        Predicates(
            items
                .chain(max_items)
                .chain(min_items)
                .chain(unique_items)
                .chain(contains)
                .collect(),
        )
    }
}

impl From<&ObjectValidation> for Predicates {
    fn from(
        ObjectValidation {
            max_properties,
            min_properties,
            required,
            properties,
            pattern_properties,
            additional_properties,
            property_names,
        }: &ObjectValidation,
    ) -> Self {
        let max_properties = max_properties
            .map(|n| {
                mk_app!(
                    static_access("predicates", ["records", "maxProperties"]),
                    Term::Num(n.into())
                )
                .into()
            })
            .into_iter();

        let min_properties = min_properties
            .map(|n| {
                mk_app!(
                    static_access("predicates", ["records", "minProperties"]),
                    Term::Num(n.into())
                )
                .into()
            })
            .into_iter();

        let property_names = property_names
            .as_deref()
            .map(|s| {
                mk_app!(
                    static_access("predicates", ["records", "propertyNames"]),
                    Predicate::from(s)
                )
                .into()
            })
            .into_iter();

        let required = {
            if required.is_empty() {
                None
            } else {
                Some(
                    mk_app!(
                        static_access("predicates", ["records", "required"]),
                        Term::Array(
                            Array::new(required.iter().map(make::string).collect()),
                            Default::default()
                        )
                    )
                    .into(),
                )
            }
        }
        .into_iter();

        let record = [mk_app!(
            static_access("predicates", ["records", "record"]),
            Term::Record(RecordData::with_field_values(
                properties
                    .iter()
                    .map(|(k, v)| (k.into(), Predicate::from(v).into()))
                    .collect()
            )),
            Term::Record(RecordData::with_field_values(
                pattern_properties
                    .iter()
                    .map(|(k, v)| (k.into(), Predicate::from(v).into()))
                    .collect()
            )),
            Term::Bool(!matches!(
                additional_properties.as_deref(),
                Some(Schema::Bool(false))
            )),
            or_always(additional_properties.as_deref())
        )
        .into()]
        .into_iter();

        Predicates(
            max_properties
                .chain(min_properties)
                .chain(property_names)
                .chain(required)
                .chain(record)
                .collect(),
        )
    }
}

fn dependencies(extensions: &BTreeMap<String, Value>) -> impl IntoIterator<Item = Predicate> {
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
                                    .map(|s| Predicate::from(&s).into())
                                    .unwrap()
                            }
                        ))
                        .collect()
                ))
            )
            .into()
        })
        .into_iter()
}

impl From<&SchemaObject> for Predicate {
    fn from(
        SchemaObject {
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
        }: &SchemaObject,
    ) -> Self {
        mk_all_of(
            instance_type
                .iter()
                .map(Predicate::from)
                .chain(enum_values.as_deref().map(Predicate::from))
                .chain(const_value.as_ref().map(Predicate::from))
                .chain(subschemas.iter().flat_map(|x| Predicates::from(x.as_ref())))
                .chain(number.iter().flat_map(|x| Predicates::from(x.as_ref())))
                .chain(string.iter().flat_map(|x| Predicates::from(x.as_ref())))
                .chain(array.iter().flat_map(|x| Predicates::from(x.as_ref())))
                .chain(object.iter().flat_map(|x| Predicates::from(x.as_ref())))
                .chain(
                    reference
                        .as_deref()
                        .map(|r| Predicate::from(definitions::reference(r).predicate)),
                )
                // schema.rs parses dependencies incorrectly. It should really be
                // part of object validation (object_predicates()) but it gets put
                // in extensions instead.
                .chain(dependencies(extensions)),
        )
    }
}

impl From<&Schema> for Predicate {
    fn from(value: &Schema) -> Self {
        match value {
            Schema::Bool(true) => static_access("predicates", ["always"]).into(),
            Schema::Bool(false) => static_access("predicates", ["never"]).into(),
            Schema::Object(o) => o.into(),
        }
    }
}
