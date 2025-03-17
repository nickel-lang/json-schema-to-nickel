//! Our internal representation of schemas.
//!
//! This representation is quite close to JSON Schema, but a little lower-level.
//! For example, JSON Schema has (with a few exceptions, like "properties" and
//! "additionalProperties') an implicit "and" between all fields of a schema; rather
//! than do that, our representation always makes boolean operations explicit.

use nickel_lang_core::{
    identifier::Ident,
    mk_app,
    term::{array::ArrayAttrs, RichTerm, Term},
    typ::{EnumRowF, EnumRows, EnumRowsF, TypeF},
};
use ordered_float::NotNan;
use serde::Serialize;
use serde_json::Value;

use crate::{
    contract::ContractContext,
    object::Obj,
    references::References,
    typ::{InstanceType, InstanceTypeSet},
    utils::{num, sequence, type_contract},
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

    pub fn to_contract(&self, ctx: ContractContext) -> Vec<RichTerm> {
        match self {
            Schema::Always => vec![ctx.js2n("Always")],
            Schema::Never => vec![ctx.js2n("Never")],
            Schema::Null => vec![ctx.js2n("Null")],
            Schema::Boolean => vec![type_contract(TypeF::Bool)],
            Schema::Const(val) => {
                let nickel_val: RichTerm = serde_json::from_value(val.clone()).unwrap();
                if ctx.eager {
                    vec![mk_app!(ctx.js2n("Const"), nickel_val)]
                } else {
                    vec![mk_app!(ctx.std("contract.Equal"), nickel_val)]
                }
            }
            Schema::Enum(vec) => {
                if let Some(strings) = vec.iter().map(|v| v.as_str()).collect::<Option<Vec<_>>>() {
                    let enum_rows: EnumRows =
                        strings.iter().fold(EnumRows(EnumRowsF::Empty), |acc, s| {
                            let row = EnumRowF {
                                id: Ident::new(s).into(),
                                typ: None,
                            };

                            EnumRows(EnumRowsF::Extend {
                                row,
                                tail: Box::new(acc),
                            })
                        });

                    vec![
                        ctx.std("enum.TagOrString"),
                        type_contract(TypeF::Enum(enum_rows)),
                    ]
                } else {
                    vec![mk_app!(
                        ctx.js2n("enum"),
                        Term::Array(
                            vec.iter()
                                .map(|val| serde_json::from_value(val.clone()).unwrap())
                                .collect(),
                            ArrayAttrs::default()
                        )
                    )]
                }
            }
            Schema::Object(obj) => obj.to_contract(ctx),
            Schema::String(s) => vec![s.to_contract(ctx)],
            Schema::Number(num) => vec![num.to_contract(ctx)],
            Schema::Array(arr) => vec![arr.to_contract(ctx)],
            Schema::Ref(s) => vec![ctx.ref_term(s)],
            Schema::AnyOf(vec) => {
                let eager = ctx.eager || !eagerly_disjoint(vec.iter(), ctx.refs);
                let ctx = if eager { ctx.eager() } else { ctx.lazy() };
                let contracts = vec.iter().map(|s| sequence(s.to_contract(ctx))).collect();
                vec![mk_app!(
                    ctx.std("contract.any_of"),
                    Term::Array(contracts, ArrayAttrs::default())
                )]
            }
            Schema::OneOf(vec) => {
                let contracts = vec
                    .iter()
                    .map(|s| sequence(s.to_contract(ctx.eager())))
                    .collect();
                vec![mk_app!(
                    ctx.js2n("one_of"),
                    Term::Array(contracts, ArrayAttrs::default())
                )]
            }
            Schema::AllOf(vec) => vec.iter().flat_map(|s| s.to_contract(ctx)).collect(),
            Schema::Ite { iph, then, els } => {
                // The "if" contract always needs to be checked eagerly.
                let iph = sequence(iph.to_contract(ctx.eager()));
                let then = sequence(then.to_contract(ctx));
                let els = sequence(els.to_contract(ctx));
                vec![mk_app!(ctx.js2n("if_then_else"), iph, then, els)]
            }
            Schema::Not(schema) => {
                vec![mk_app!(
                    ctx.std("contract.not"),
                    sequence(schema.to_contract(ctx.eager()))
                )]
            }
        }
    }
}

impl Str {
    pub fn to_contract(&self, ctx: ContractContext) -> RichTerm {
        match self {
            Str::Any => type_contract(TypeF::String),
            Str::MaxLength(n) => {
                mk_app!(ctx.js2n("string.MaxLength"), Term::Num((*n).into()))
            }
            Str::MinLength(n) => {
                mk_app!(ctx.js2n("string.MinLength"), Term::Num((*n).into()))
            }
            Str::Pattern(s) => mk_app!(ctx.js2n("string.Matches"), Term::Str(s.to_owned().into())),
        }
    }
}

impl Num {
    pub fn to_contract(&self, ctx: ContractContext) -> RichTerm {
        match self {
            Num::Any => type_contract(TypeF::Number),
            Num::MultipleOf(x) => mk_app!(ctx.js2n("number.MultipleOf"), num(*x)),
            Num::Maximum(x) => mk_app!(ctx.js2n("number.Maximum"), num(*x)),
            Num::Minimum(x) => mk_app!(ctx.js2n("number.Minimum"), num(*x)),
            Num::ExclusiveMinimum(x) => {
                mk_app!(ctx.js2n("number.ExclusiveMinimum"), num(*x))
            }
            Num::ExclusiveMaximum(x) => {
                mk_app!(ctx.js2n("number.ExclusiveMaximum"), num(*x))
            }
            Num::Integer => ctx.std("number.Integer"),
        }
    }
}

impl Arr {
    pub fn to_contract(&self, ctx: ContractContext) -> RichTerm {
        match self {
            Arr::Any => type_contract(TypeF::Array(Box::new(TypeF::Dyn.into()))),
            Arr::AllItems(schema) => {
                if ctx.eager {
                    mk_app!(ctx.js2n("array.ArrayOf"), sequence(schema.to_contract(ctx)))
                } else {
                    type_contract(TypeF::Array(Box::new(
                        TypeF::Contract(sequence(schema.to_contract(ctx))).into(),
                    )))
                }
            }
            Arr::PerItem { initial, rest } => {
                let initial = initial.iter().map(|s| sequence(s.to_contract(ctx)));
                let rest = sequence(rest.to_contract(ctx));
                mk_app!(
                    ctx.js2n("array.Items"),
                    Term::Array(initial.collect(), ArrayAttrs::default()),
                    rest
                )
            }
            Arr::MaxItems(n) => {
                mk_app!(ctx.js2n("array.MaxItems"), Term::Num((*n).into()))
            }
            Arr::MinItems(n) => {
                mk_app!(ctx.js2n("array.MinItems"), Term::Num((*n).into()))
            }
            Arr::UniqueItems => ctx.js2n("array.UniqueItems"),
            Arr::Contains(schema) => {
                let contract = sequence(schema.to_contract(ctx.eager()));
                mk_app!(ctx.js2n("array.Contains"), contract)
            }
        }
    }
}

fn eagerly_disjoint<'a>(schemas: impl Iterator<Item = &'a Schema>, refs: &References) -> bool {
    let mut seen_types = InstanceTypeSet::EMPTY;

    for s in schemas {
        let Some(ty) = s.simple_type(refs) else {
            return false;
        };

        if seen_types.contains(ty) {
            return false;
        }

        seen_types.insert(ty);
    }
    true
}
