//! Our internal representation of schemas.
//!
//! This representation is quite close to JSON Schema, but a little lower-level.
//! For example, JSON Schema has (with a few exceptions, like "properties" and
//! "additionalProperties') an implicit "and" between all fields of a schema; rather
//! than do that, our representation always makes boolean operations explicit.

use std::collections::{BTreeMap, BTreeSet};

use ordered_float::NotNan;
use serde::Serialize;
use serde_json::Value;

use crate::{
    references::References,
    typ::{InstanceType, InstanceTypeSet},
};

#[derive(Clone, Debug, Serialize, Hash, PartialEq, Eq)]
pub enum Schema {
    Always,
    Never,
    Null,
    Boolean,
    Const(Value),
    Enum(Vec<Value>),
    Object(Obj),
    String(Str),
    Number(Num),
    Array(Arr),
    Ref(String),
    AnyOf(Vec<Schema>),
    OneOf(Vec<Schema>),
    AllOf(Vec<Schema>),
    Ite {
        iph: Box<Schema>,
        then: Box<Schema>,
        els: Box<Schema>,
    },
    Not(Box<Schema>),
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq, Hash)]
pub enum Str {
    Any,
    MaxLength(u64),
    MinLength(u64),
    Pattern(String),
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq, Hash)]
pub enum Num {
    Any,
    // The json-schema reference doesn't say this has to be an integer (and
    // if it isn't an integer it doesn't specify how rounding is supposed to
    // be handled).
    MultipleOf(NotNan<f64>),
    Maximum(NotNan<f64>),
    Minimum(NotNan<f64>),
    ExclusiveMinimum(NotNan<f64>),
    ExclusiveMaximum(NotNan<f64>),
    Integer,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq, Hash)]
pub enum Arr {
    Any,
    AllItems(Box<Schema>),
    PerItem {
        initial: Vec<Schema>,
        rest: Box<Schema>,
    },
    MaxItems(u64),
    MinItems(u64),
    UniqueItems,
    Contains(Box<Schema>),
}

fn apparent_type(value: &Value) -> InstanceType {
    match value {
        Value::Null => InstanceType::Null,
        Value::Bool(_) => InstanceType::Boolean,
        Value::Number(_) => InstanceType::Number,
        Value::String(_) => InstanceType::String,
        Value::Array(_) => InstanceType::Array,
        Value::Object(_) => InstanceType::Object,
    }
}

fn apparent_types<'a>(values: impl IntoIterator<Item = &'a Value>) -> InstanceTypeSet {
    values.into_iter().map(apparent_type).collect()
}

impl Schema {
    pub fn simple_type(&self, refs: &References) -> Option<InstanceType> {
        fn simple_type_rec<'s>(slf: &'s Schema, refs: &'s References) -> Option<InstanceType> {
            match slf {
                Schema::Always => None,
                Schema::Never => None,
                Schema::Null => Some(InstanceType::Null),
                Schema::Boolean => Some(InstanceType::Boolean),
                Schema::Const(v) => Some(apparent_type(v)),
                Schema::Enum(vs) => {
                    if let Some(ty) = vs.first().map(apparent_type) {
                        vs.iter().map(apparent_type).all(|u| u == ty).then_some(ty)
                    } else {
                        None
                    }
                }
                Schema::Object(_) => Some(InstanceType::Object),
                Schema::String(_) => Some(InstanceType::String),
                Schema::Number(_) => Some(InstanceType::Number),
                Schema::Array(_) => Some(InstanceType::Array),
                Schema::Ref(r) => refs.get(r).and_then(|s| simple_type_rec(&s, refs)),
                Schema::AnyOf(_) => None,
                Schema::OneOf(_) => None,
                Schema::AllOf(schemas) => {
                    let mut intersection = InstanceTypeSet::FULL;
                    for s in schemas {
                        intersection = intersection.intersect(s.allowed_types_shallow(refs));
                    }
                    intersection.to_singleton()
                }
                Schema::Ite { els, .. } => els.simple_type(refs),
                Schema::Not(_) => None,
            }
        }

        simple_type_rec(self, refs)
    }

    pub fn just_type(&self) -> Option<InstanceType> {
        match self {
            Schema::Null => Some(InstanceType::Null),
            Schema::Boolean => Some(InstanceType::Boolean),
            Schema::Object(Obj::Any) => Some(InstanceType::Object),
            Schema::Array(Arr::Any) => Some(InstanceType::Array),
            Schema::String(Str::Any) => Some(InstanceType::String),
            Schema::Number(Num::Any) => Some(InstanceType::Number),
            _ => None,
        }
    }

    pub fn just_type_set(&self) -> Option<InstanceTypeSet> {
        if let Some(ty) = self.just_type() {
            Some(InstanceTypeSet::singleton(ty))
        } else if let Schema::AnyOf(schemas) = self {
            schemas.iter().map(|s| s.just_type()).collect()
        } else {
            None
        }
    }

    pub fn allowed_types_shallow(&self, refs: &References) -> InstanceTypeSet {
        match self {
            Schema::Always => InstanceTypeSet::FULL,
            Schema::Never => InstanceTypeSet::EMPTY,
            Schema::Null => InstanceTypeSet::singleton(InstanceType::Null),
            Schema::Boolean => InstanceTypeSet::singleton(InstanceType::Boolean),
            Schema::Const(v) => InstanceTypeSet::singleton(apparent_type(v)),
            Schema::Enum(vs) => apparent_types(vs),
            Schema::Object(_) => InstanceTypeSet::singleton(InstanceType::Object),
            Schema::String(_) => InstanceTypeSet::singleton(InstanceType::String),
            Schema::Number(_) => InstanceTypeSet::singleton(InstanceType::Number),
            Schema::Array(_) => InstanceTypeSet::singleton(InstanceType::Array),
            Schema::Ref(name) => refs
                .get(name)
                .map(|s| s.allowed_types_shallow(refs))
                .unwrap_or(InstanceTypeSet::FULL),
            Schema::AnyOf(vec) => vec
                .iter()
                .map(|s| s.simple_type(refs))
                .collect::<Option<InstanceTypeSet>>()
                .unwrap_or(InstanceTypeSet::FULL),
            Schema::OneOf(_) => InstanceTypeSet::FULL,
            Schema::AllOf(_) => InstanceTypeSet::FULL,
            Schema::Ite { .. } => InstanceTypeSet::FULL,
            Schema::Not(_) => InstanceTypeSet::FULL,
        }
    }

    // TODO: may be worth memoizing something
    pub fn allowed_types(&self, refs: &References) -> InstanceTypeSet {
        match self {
            Schema::Always => InstanceTypeSet::FULL,
            Schema::Never => InstanceTypeSet::EMPTY,
            Schema::Null => InstanceTypeSet::singleton(InstanceType::Null),
            Schema::Boolean => InstanceTypeSet::singleton(InstanceType::Boolean),
            Schema::Const(v) => InstanceTypeSet::singleton(apparent_type(v)),
            Schema::Enum(vs) => apparent_types(vs),
            Schema::Object(_) => InstanceTypeSet::singleton(InstanceType::Object),
            Schema::String(_) => InstanceTypeSet::singleton(InstanceType::String),
            Schema::Number(_) => InstanceTypeSet::singleton(InstanceType::Number),
            Schema::Array(_) => InstanceTypeSet::singleton(InstanceType::Array),
            Schema::Ref(name) => refs
                .get(name)
                .map(|s| s.allowed_types(refs))
                .unwrap_or(InstanceTypeSet::FULL),
            Schema::AnyOf(vec) => vec
                .iter()
                .map(|s| s.simple_type(refs))
                .collect::<Option<InstanceTypeSet>>()
                .unwrap_or(InstanceTypeSet::FULL),
            Schema::OneOf(_) => InstanceTypeSet::FULL,
            Schema::AllOf(vec) => {
                let mut intersection = InstanceTypeSet::FULL;
                for s in vec {
                    intersection = intersection.intersect(s.allowed_types(refs));
                }
                intersection
            }
            Schema::Ite { .. } => InstanceTypeSet::FULL,
            Schema::Not(_) => InstanceTypeSet::FULL,
        }
    }

    pub fn is_always_eager(&self, refs: &References) -> bool {
        match self {
            Schema::Always
            | Schema::Never
            | Schema::Null
            | Schema::Boolean
            | Schema::Const(_)
            | Schema::Enum(_)
            | Schema::String(_)
            | Schema::Number(_) => true,
            Schema::Ref(r) => refs.get(r).is_none_or(|s| s.is_always_eager(refs)),
            Schema::Object(obj) => match obj {
                Obj::Any
                | Obj::MaxProperties(_)
                | Obj::MinProperties(_)
                | Obj::Required(_)
                | Obj::PropertyNames(_)
                | Obj::DependentFields(_) => true,
                Obj::Properties(props) => {
                    props
                        .properties
                        .values()
                        .all(|p| p.schema.is_always_eager(refs))
                        && props
                            .pattern_properties
                            .values()
                            .all(|s| s.is_always_eager(refs))
                        && props
                            .additional_properties
                            .as_ref()
                            .is_none_or(|s| s.is_always_eager(refs))
                }
                Obj::DependentSchemas(deps) => deps.values().all(|s| s.is_always_eager(refs)),
            },
            Schema::Array(arr) => match arr {
                Arr::Any
                | Arr::MaxItems(_)
                | Arr::MinItems(_)
                | Arr::UniqueItems
                | Arr::Contains(_) => true,
                Arr::AllItems(schema) => schema.is_always_eager(refs),
                Arr::PerItem { initial, rest } => {
                    initial.iter().all(|s| s.is_always_eager(refs)) && rest.is_always_eager(refs)
                }
            },
            Schema::AnyOf(vec) | Schema::OneOf(vec) | Schema::AllOf(vec) => {
                vec.iter().all(|s| s.is_always_eager(refs))
            }
            Schema::Ite { iph, then, els } => {
                iph.is_always_eager(refs) && then.is_always_eager(refs) && els.is_always_eager(refs)
            }
            Schema::Not(schema) => schema.is_always_eager(refs),
        }
    }
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq, Hash)]
pub enum Obj {
    Any,
    Properties(ObjectProperties),
    MaxProperties(u64),
    MinProperties(u64),
    Required(BTreeSet<String>),
    // This could be simplified maybe, because we're guaranteed that this will only need to validate strings.
    PropertyNames(Box<Schema>),
    DependentFields(BTreeMap<String, Vec<String>>),
    DependentSchemas(BTreeMap<String, Schema>),
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq, Hash)]
pub struct Property {
    pub doc: Option<String>,
    pub schema: Schema,
    pub optional: bool,
}

impl From<Schema> for Property {
    fn from(s: Schema) -> Self {
        Property {
            doc: None,
            schema: s,
            optional: true,
        }
    }
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq, Hash)]
pub struct ObjectProperties {
    pub properties: BTreeMap<String, Property>,
    pub pattern_properties: BTreeMap<String, Schema>,
    pub additional_properties: Option<Box<Schema>>,
}
