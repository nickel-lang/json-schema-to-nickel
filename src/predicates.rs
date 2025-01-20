//! # Nickel eager contract generation for JSON schemas
//!
//! [crate::contracts] implements a translation from JSON schemas to Nickel which tries to preserve
//! lazyness. This isn't always possible. This module provides a fallback converion that is based
//! on boolean predicates instead, and which can handle general JSON schemas.
//!
//! The drawback is that the resulting Nickel contracts are eager (they don't preserve lazyness)
//! and are less LSP-friendly.
use std::{collections::BTreeMap, iter};

use nickel_lang_core::{
    identifier::LocIdent,
    mk_app,
    term::{array::Array, make, record::RecordData, Number, RichTerm, Term},
};
use schemars::schema::{
    ArrayValidation, InstanceType, NumberValidation, ObjectValidation, RootSchema, Schema,
    SchemaObject, SingleOrVec, StringValidation, SubschemaValidation,
};
use serde_json::Value;

use crate::{
    references::{self, RefUsageContext, RefsUsage},
    utils::static_access,
    PREDICATES_LIBRARY_ID,
};

#[derive(Clone)]
pub struct Predicate(RichTerm);

impl Predicate {
    /// Convert a full JSON schema to a predicate. Returns the predicate and the `$refs` that were
    /// referenced during the conversion.
    pub fn from_root_schema(root: &RootSchema) -> (Self, RefsUsage) {
        let mut refs_usage = RefsUsage::default();
        let predicate = root.schema.as_predicate(&mut refs_usage);

        (predicate, refs_usage)
    }

    /// Return an always succeeding predicate.
    pub fn always() -> Self {
        static_access(PREDICATES_LIBRARY_ID, ["always"]).into()
    }
}

/// [AsPredicate] is essentially like `Into<Predicate>` but passes additional state around used for
/// effective reference resolution. Similar to [crate::contracts::TryAsContract] for [Predicate], but
/// infallible.
pub trait AsPredicate {
    /// Convert a JSON schema component `Self` to a predicate. [Self::as_predicate] carries
    /// additional state related to reference resolution.
    fn as_predicate(&self, refs_usage: &mut RefsUsage) -> Predicate;
}

/// [AsPredicates] is a variant of [AsPredicate] returning a sequence of predicates.
pub trait AsPredicates {
    /// Convert a JSON schema component `Self` to a list of predicates. [Self::as_predicates]
    /// carries additional state related to reference resolution.
    fn as_predicates(&self, refs_usage: &mut RefsUsage) -> Predicates;
}

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
pub struct Predicates(Vec<Predicate>);

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

fn or_always(s: Option<&Schema>, refs_usage: &mut RefsUsage) -> Predicate {
    s.map(|s| s.as_predicate(refs_usage))
        .unwrap_or(Predicate::always())
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
        mk_app!(static_access(PREDICATES_LIBRARY_ID, ["isType"]), type_tag).into()
    }
}

impl From<&SingleOrVec<InstanceType>> for Predicate {
    fn from(value: &SingleOrVec<InstanceType>) -> Self {
        match value {
            SingleOrVec::Single(t) => t.as_ref().into(),
            SingleOrVec::Vec(ts) => mk_app!(
                static_access(PREDICATES_LIBRARY_ID, ["anyOf"]),
                Term::Array(
                    ts.iter().map(|t| Predicate::from(t).into()).collect(),
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
            static_access(PREDICATES_LIBRARY_ID, ["enum"]),
            Term::Array(
                value
                    .iter()
                    .map(|v| serde_json::from_value(v.clone()).unwrap())
                    .collect(),
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
            static_access(PREDICATES_LIBRARY_ID, ["const"]),
            serde_json::from_value(value.clone()).unwrap(),
        )
        .into()
    }
}

fn mk_all_of(preds: impl IntoIterator<Item = Predicate>) -> Predicate {
    let mut ps = preds.into_iter();
    match (ps.next(), ps.next()) {
        // []
        (None, _) => Predicate::always(),
        // [p]
        (Some(p), None) => p,
        // ps
        (Some(p1), Some(p2)) => {
            // reconstruct the full iterator
            let ps = iter::once(p1).chain(iter::once(p2)).chain(ps);
            mk_app!(
                static_access(PREDICATES_LIBRARY_ID, ["allOf"]),
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
        (None, _) => Predicate::always(),
        // [p]
        (Some(p), None) => p,
        // ps
        (Some(p1), Some(p2)) => {
            // reconstruct the full iterator
            let ps = iter::once(p1).chain(iter::once(p2)).chain(ps);
            mk_app!(
                static_access(PREDICATES_LIBRARY_ID, ["anyOf"]),
                Term::Array(Array::from_iter(ps.map(RichTerm::from)), Default::default())
            )
            .into()
        }
    }
}

impl AsPredicates for SubschemaValidation {
    fn as_predicates(&self, refs_usage: &mut RefsUsage) -> Predicates {
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
            .map(|schemas| mk_all_of(schemas.iter().map(|res| res.as_predicate(refs_usage))))
            .into_iter();

        let any_of = any_of
            .as_deref()
            .map(|schemas| mk_any_of(schemas.iter().map(|res| res.as_predicate(refs_usage))))
            .into_iter();

        let one_of = one_of
            .as_deref()
            .map(|schemas| {
                mk_app!(
                    static_access(PREDICATES_LIBRARY_ID, ["oneOf"]),
                    Term::Array(
                        schemas
                            .iter()
                            .map(|s| s.as_predicate(refs_usage).into())
                            .collect(),
                        Default::default()
                    )
                )
                .into()
            })
            .into_iter();

        let not = not
            .as_deref()
            .map(|s| {
                mk_app!(
                    static_access(PREDICATES_LIBRARY_ID, ["not"]),
                    s.as_predicate(refs_usage)
                )
                .into()
            })
            .into_iter();

        let ite = if_schema
            .as_deref()
            .map(move |if_schema| {
                mk_app!(
                    static_access(PREDICATES_LIBRARY_ID, ["ifThenElse"]),
                    if_schema.as_predicate(refs_usage),
                    or_always(then_schema.as_deref(), refs_usage),
                    or_always(else_schema.as_deref(), refs_usage)
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
                    static_access(PREDICATES_LIBRARY_ID, ["numbers", s]),
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
    ) -> Predicates {
        let max_length = max_length
            .map(|n| {
                mk_app!(
                    static_access(PREDICATES_LIBRARY_ID, ["strings", "maxLength"]),
                    Term::Num(n.into())
                )
                .into()
            })
            .into_iter();

        let min_length = min_length
            .map(|n| {
                mk_app!(
                    static_access(PREDICATES_LIBRARY_ID, ["strings", "minLength"]),
                    Term::Num(n.into())
                )
                .into()
            })
            .into_iter();

        let pattern = pattern
            .as_deref()
            .map(|s| {
                mk_app!(
                    static_access(PREDICATES_LIBRARY_ID, ["strings", "pattern"]),
                    make::string(s)
                )
                .into()
            })
            .into_iter();

        Predicates(max_length.chain(min_length).chain(pattern).collect())
    }
}

impl AsPredicates for ArrayValidation {
    fn as_predicates(&self, refs_usage: &mut RefsUsage) -> Predicates {
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
                static_access(PREDICATES_LIBRARY_ID, ["arrays", "arrayOf"]),
                s.as_predicate(refs_usage)
            )
            .into()],
            Some(SingleOrVec::Vec(schemas)) => {
                let len = schemas.len();
                [mk_app!(
                    static_access(PREDICATES_LIBRARY_ID, ["arrays", "items"]),
                    Term::Array(
                        schemas
                            .iter()
                            .map(|x| x.as_predicate(refs_usage).into())
                            .collect(),
                        Default::default()
                    )
                )
                .into()]
                .into_iter()
                .chain(additional_items.as_deref().map(|s| {
                    mk_app!(
                        static_access(PREDICATES_LIBRARY_ID, ["arrays", "additionalItems"]),
                        s.as_predicate(refs_usage),
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
                    static_access(PREDICATES_LIBRARY_ID, ["arrays", "maxItems"]),
                    Term::Num(n.into())
                )
                .into()
            })
            .into_iter();

        let min_items = min_items
            .map(|n| {
                mk_app!(
                    static_access(PREDICATES_LIBRARY_ID, ["arrays", "minItems"]),
                    Term::Num(n.into())
                )
                .into()
            })
            .into_iter();

        let unique_items = unique_items
            .and_then(|unique| {
                unique.then_some(
                    static_access(PREDICATES_LIBRARY_ID, ["arrays", "uniqueItems"]).into(),
                )
            })
            .into_iter();

        let contains = contains
            .as_deref()
            .map(|s| {
                mk_app!(
                    static_access(PREDICATES_LIBRARY_ID, ["arrays", "contains"]),
                    s.as_predicate(refs_usage)
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

impl AsPredicates for ObjectValidation {
    fn as_predicates(&self, refs_usage: &mut RefsUsage) -> Predicates {
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
                    static_access(PREDICATES_LIBRARY_ID, ["records", "maxProperties"]),
                    Term::Num(n.into())
                )
                .into()
            })
            .into_iter();

        let min_properties = min_properties
            .map(|n| {
                mk_app!(
                    static_access(PREDICATES_LIBRARY_ID, ["records", "minProperties"]),
                    Term::Num(n.into())
                )
                .into()
            })
            .into_iter();

        let property_names = property_names
            .as_deref()
            .map(|s| {
                mk_app!(
                    static_access(PREDICATES_LIBRARY_ID, ["records", "propertyNames"]),
                    s.as_predicate(refs_usage)
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
                        static_access(PREDICATES_LIBRARY_ID, ["records", "required"]),
                        Term::Array(
                            required.iter().map(make::string).collect(),
                            Default::default()
                        )
                    )
                    .into(),
                )
            }
        }
        .into_iter();

        let record = [mk_app!(
            static_access(PREDICATES_LIBRARY_ID, ["records", "record"]),
            Term::Record(RecordData::with_field_values(
                properties
                    .iter()
                    .map(|(k, v)| (k.into(), v.as_predicate(refs_usage).into()))
            )),
            Term::Record(RecordData::with_field_values(
                pattern_properties
                    .iter()
                    .map(|(k, v)| (k.into(), v.as_predicate(refs_usage).into()))
            )),
            Term::Bool(!matches!(
                additional_properties.as_deref(),
                Some(Schema::Bool(false))
            )),
            or_always(additional_properties.as_deref(), refs_usage)
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

fn dependencies(
    extensions: &BTreeMap<String, Value>,
    refs_usage: &mut RefsUsage,
) -> impl IntoIterator<Item = Predicate> {
    extensions
        .get("dependencies")
        .and_then(|v| v.as_object())
        .map(|deps| {
            mk_app!(
                static_access(PREDICATES_LIBRARY_ID, ["records", "dependencies"]),
                Term::Record(RecordData::with_field_values(deps.into_iter().map(
                    |(key, value)| (
                        LocIdent::from(key),
                        if let Some(fields) = value.as_array().and_then(|v| {
                            v.iter().map(|s| s.as_str()).collect::<Option<Vec<_>>>()
                        }) {
                            Term::Array(
                                fields.into_iter().map(make::string).collect(),
                                Default::default(),
                            )
                            .into()
                        } else {
                            serde_json::from_value::<Schema>(value.clone())
                                .map(|s| s.as_predicate(refs_usage).into())
                                .unwrap()
                        }
                    )
                )))
            )
            .into()
        })
        .into_iter()
}

impl AsPredicate for SchemaObject {
    fn as_predicate(&self, refs_usage: &mut RefsUsage) -> Predicate {
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

        // Because we can't share the mutable reference to refs_usage, we need to build the
        // arguments to `mk_all_of` in a separate vector pieces by pieces instead of chaining
        // directly everything in one big iterator, so that we never share `ref_usages` between two
        // live closures.
        let mut args: Vec<_> = instance_type
            .iter()
            .map(Predicate::from)
            .chain(enum_values.as_deref().map(Predicate::from))
            .chain(const_value.as_ref().map(Predicate::from))
            .chain(subschemas.iter().flat_map(|x| x.as_predicates(refs_usage)))
            .chain(
                number
                    .iter()
                    .flat_map(|x| Predicates::from(x.as_ref()))
                    .chain(string.iter().flat_map(|x| Predicates::from(x.as_ref()))),
            )
            .collect();

        args.extend(array.iter().flat_map(|x| x.as_predicates(refs_usage)));

        args.extend(
            object
                .iter()
                .flat_map(|x| x.as_ref().as_predicates(refs_usage)),
        );

        args.extend(reference.as_deref().map(|r| {
            Predicate::from(references::resolve_ref(
                r,
                refs_usage,
                RefUsageContext::Predicate,
            ))
        }));

        args.extend(
            // schema.rs parses dependencies incorrectly. It should really be
            // part of object validation (object_predicates()) but it gets put
            // in extensions instead.
            dependencies(extensions, refs_usage),
        );

        mk_all_of(args)
    }
}

impl AsPredicate for Schema {
    fn as_predicate(&self, refs_usage: &mut RefsUsage) -> Predicate {
        match self {
            Schema::Bool(true) => Predicate::always(),
            Schema::Bool(false) => static_access(PREDICATES_LIBRARY_ID, ["never"]).into(),
            Schema::Object(o) => o.as_predicate(refs_usage),
        }
    }
}
