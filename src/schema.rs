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
use serde::Serialize;
use serde_json::Value;

use crate::{
    contract::ContractContext,
    object::Obj,
    references::AcyclicReferences,
    typ::{InstanceType, InstanceTypeSet},
    utils::{num, sequence, type_contract},
};

use nickel_lang_core::term::Number;

/// A schema.
#[derive(Clone, Debug, Serialize, Hash, PartialEq, Eq)]
pub enum Schema {
    /// Always succeeds.
    Always,
    /// Always fails.
    Never,
    /// Asserts that the value equals `null`.
    Null,
    /// Asserts that the value is a boolean.
    Boolean,
    /// Asserts that the value is equal to a specific constant.
    Const(Value),
    /// Asserts that the value is equal to one of a number of constants.
    Enum(Vec<Value>),
    /// Asserts that the value is an object, possibly with some additional constraints.
    Object(Obj),
    /// Asserts that the value is a string, possibly with some additional constraints.
    String(Str),
    /// Asserts that the value is a number, possibly with some additional constraints.
    Number(Num),
    /// Asserts that the value is an array, possibly with some additional constraints.
    Array(Array),
    /// A reference to another schema.
    ///
    /// The string is a JSON-schema path, like "#/definitions/foo" or "#/properties/bar".
    Ref(String),
    /// Asserts that at least one of the contained schemas succeeds.
    AnyOf(Vec<Schema>),
    /// Asserts that exactly one of the contained schemas succeeds.
    OneOf(Vec<Schema>),
    /// Asserts that all of the contained schemas succeed.
    AllOf(Vec<Schema>),
    /// If `iph` succeeds, asserts that `then` does also. Otherwise, asserts that `els` succeeds.
    IfThenElse {
        iph: Box<Schema>,
        then: Box<Schema>,
        els: Box<Schema>,
    },
    /// Asserts that the contained schema fails.
    Not(Box<Schema>),
}

/// Schemas that apply to strings.
#[derive(Clone, Debug, Serialize, PartialEq, Eq, Hash)]
pub enum Str {
    /// Any string is ok.
    Any,
    /// Asserts that a string isn't too long.
    MaxLength(Number),
    /// Asserts that a string isn't too short.
    MinLength(Number),
    /// Asserts that a string matches a regular expression.
    Pattern(String),
}

/// Schemas that apply to numbers.
#[derive(Clone, Debug, Serialize, PartialEq, Eq, Hash)]
pub enum Num {
    /// Any number is ok.
    Any,
    /// Asserts that a number is an integer multiple of something.
    ///
    /// We use exact arithmetic when the "something" is a non-integer; JSON-schema
    /// isn't precise about the requirements in that case.
    MultipleOf(Number),
    /// Asserts that a number is at most some value.
    Maximum(Number),
    /// Asserts that a number is at least some value.
    Minimum(Number),
    /// Asserts that a number is strictly less than some value.
    ExclusiveMinimum(Number),
    /// Asserts that a number is strictly greater than some value.
    ExclusiveMaximum(Number),
    /// Asserts that a number is an integer.
    Integer,
}

/// Schemas that apply to arrays.
#[derive(Clone, Debug, Serialize, PartialEq, Eq, Hash)]
pub enum Array {
    /// Any array is ok.
    Any,
    /// Asserts that all items in this array satisfy a schema.
    AllItems(Box<Schema>),
    /// Asserts that the first several items in this array satisfy individual
    /// schemas, and the remaining items satisfy the `rest` schema.
    PerItem {
        initial: Vec<Schema>,
        rest: Box<Schema>,
    },
    /// Asserts that this array has at most a certain number of elements.
    MaxItems(Number),
    /// Asserts that this array has at least.
    MinItems(Number),
    /// Asserts that all items in this array are distinct.
    UniqueItems,
    /// Asserts that this array contains at least one value satisfying the given schema.
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
    /// Does this schema match just a single type of object? If so, return that type.
    ///
    /// This is best-effort: it may return `None` even when the schema does in fact
    /// match a single type. Unlike [`Schema::just_type`], this method doesn't insist
    /// that the schema is *only* a type-check. For example, if the schema is an
    /// object schema with some properties then we will return `Some(InstanceType::Object)`.
    pub fn simple_type(&self, refs: &AcyclicReferences) -> Option<InstanceType> {
        fn simple_type_rec<'s>(
            slf: &'s Schema,
            refs: &'s AcyclicReferences,
        ) -> Option<InstanceType> {
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
                Schema::IfThenElse { .. } => None,
                Schema::Not(_) => None,
            }
        }

        simple_type_rec(self, refs)
    }

    /// Is this schema just a single type with no other constraints?
    ///
    /// If so, return that type.
    pub fn just_type(&self) -> Option<InstanceType> {
        match self {
            Schema::Null => Some(InstanceType::Null),
            Schema::Boolean => Some(InstanceType::Boolean),
            Schema::Object(Obj::Any) => Some(InstanceType::Object),
            Schema::Array(Array::Any) => Some(InstanceType::Array),
            Schema::String(Str::Any) => Some(InstanceType::String),
            Schema::Number(Num::Any) => Some(InstanceType::Number),
            _ => None,
        }
    }

    /// Is this schema just a set of types with no other constraints?
    ///
    /// If so, return that set.
    pub fn just_type_set(&self) -> Option<InstanceTypeSet> {
        if let Some(ty) = self.just_type() {
            Some(InstanceTypeSet::singleton(ty))
        } else if let Schema::AnyOf(schemas) = self {
            schemas.iter().map(|s| s.just_type()).collect()
        } else {
            None
        }
    }

    /// Returns a set of types that might be accepted by this schema.
    ///
    /// The returned set is conservative, in the sense that it might be larger
    /// than necessary. Unlike `Self::allowed_types`, we don't recurse into
    /// sub-schemas. Instead, we're just extra-conservative.
    pub fn allowed_types_shallow(&self, refs: &AcyclicReferences) -> InstanceTypeSet {
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
            Schema::IfThenElse { .. } => InstanceTypeSet::FULL,
            Schema::Not(_) => InstanceTypeSet::FULL,
        }
    }

    /// Returns a set of types that might be accepted by this schema.
    ///
    /// The returned set is conservative, in the sense that it might be larger
    /// than necessary. Unlike `Self::allowed_types_shallow`, we recurse into
    /// subschemas to try and gain some extra precision. As a result, this could
    /// be slow (linear in the size of `self`).
    ///
    /// If this becomes a performance issue, it should be possible to memoize
    /// the allowed types of subschemas.
    pub fn allowed_types(&self, refs: &AcyclicReferences) -> InstanceTypeSet {
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
            Schema::IfThenElse { .. } => InstanceTypeSet::FULL,
            Schema::Not(_) => InstanceTypeSet::FULL,
        }
    }

    /// Is the most idiomatic Nickel contract for this schema an eager contract?
    ///
    /// For example, if this is an object schema with some properties then the
    /// most idiomatic Nickel contract would be a record contract (which is not
    /// eager). On the other hand, shallower schemas (like number schemas) are
    /// typically eager.
    pub fn is_always_eager(&self, refs: &AcyclicReferences) -> bool {
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
                Array::Any
                | Array::MaxItems(_)
                | Array::MinItems(_)
                | Array::UniqueItems
                | Array::Contains(_) => true,
                Array::AllItems(schema) => schema.is_always_eager(refs),
                Array::PerItem { initial, rest } => {
                    initial.iter().all(|s| s.is_always_eager(refs)) && rest.is_always_eager(refs)
                }
            },
            Schema::AnyOf(vec) | Schema::OneOf(vec) | Schema::AllOf(vec) => {
                vec.iter().all(|s| s.is_always_eager(refs))
            }
            Schema::IfThenElse { iph, then, els } => {
                iph.is_always_eager(refs) && then.is_always_eager(refs) && els.is_always_eager(refs)
            }
            Schema::Not(schema) => schema.is_always_eager(refs),
        }
    }

    /// Converts this schema into a collection of Nickel contracts.
    ///
    /// This returns a collection of Nickel contracts, and you need to apply them all
    /// (or combine them using `std.contract.Sequence`). The reason we return a
    /// collection instead of combining them for you is so that if you're applying
    /// these contracts to a record field then you can apply them individually.
    pub fn to_contract(&self, ctx: ContractContext) -> Vec<RichTerm> {
        match self {
            Schema::Always => vec![ctx.js2n("Always")],
            Schema::Never => vec![ctx.js2n("Never")],
            Schema::Null => vec![ctx.js2n("Null")],
            Schema::Boolean => vec![type_contract(TypeF::Bool)],
            Schema::Const(val) => {
                let nickel_val: RichTerm = serde_json::from_value(val.clone()).unwrap();
                if ctx.is_eager() {
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
                        ctx.js2n("Enum"),
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
            Schema::AnyOf(vec) => match vec.as_slice() {
                [Schema::Null, other] | [other, Schema::Null] => {
                    vec![mk_app!(
                        ctx.js2n("Nullable"),
                        sequence(other.to_contract(ctx))
                    )]
                }
                _ => {
                    let eager = ctx.is_eager() || !eagerly_disjoint(vec.iter(), ctx.refs());
                    let ctx = if eager { ctx.eager() } else { ctx.lazy() };
                    let contracts = vec.iter().map(|s| sequence(s.to_contract(ctx))).collect();
                    vec![mk_app!(
                        ctx.std("contract.any_of"),
                        Term::Array(contracts, ArrayAttrs::default())
                    )]
                }
            },
            Schema::OneOf(vec) => {
                let contracts = vec
                    .iter()
                    .map(|s| sequence(s.to_contract(ctx.eager())))
                    .collect();
                vec![mk_app!(
                    ctx.js2n("OneOf"),
                    Term::Array(contracts, ArrayAttrs::default())
                )]
            }
            Schema::AllOf(vec) => vec.iter().flat_map(|s| s.to_contract(ctx)).collect(),
            Schema::IfThenElse { iph, then, els } => {
                // The "if" contract always needs to be checked eagerly.
                let iph = sequence(iph.to_contract(ctx.eager()));
                let then = sequence(then.to_contract(ctx));
                let els = sequence(els.to_contract(ctx));
                vec![mk_app!(ctx.js2n("IfThenElse"), iph, then, els)]
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
                mk_app!(ctx.js2n("string.MaxLength"), num(n))
            }
            Str::MinLength(n) => {
                mk_app!(ctx.js2n("string.MinLength"), num(n))
            }
            Str::Pattern(s) => mk_app!(ctx.js2n("string.Matches"), Term::Str(s.to_owned().into())),
        }
    }
}

impl Num {
    pub fn to_contract(&self, ctx: ContractContext) -> RichTerm {
        match self {
            Num::Any => type_contract(TypeF::Number),
            Num::MultipleOf(x) => mk_app!(ctx.js2n("number.MultipleOf"), num(x)),
            Num::Maximum(x) => mk_app!(ctx.js2n("number.Maximum"), num(x)),
            Num::Minimum(x) => mk_app!(ctx.js2n("number.Minimum"), num(x)),
            Num::ExclusiveMinimum(x) => {
                mk_app!(ctx.js2n("number.ExclusiveMinimum"), num(x))
            }
            Num::ExclusiveMaximum(x) => {
                mk_app!(ctx.js2n("number.ExclusiveMaximum"), num(x))
            }
            Num::Integer => ctx.std("number.Integer"),
        }
    }
}

impl Array {
    pub fn to_contract(&self, ctx: ContractContext) -> RichTerm {
        match self {
            Array::Any => type_contract(TypeF::Array(Box::new(TypeF::Dyn.into()))),
            Array::AllItems(schema) => {
                if ctx.is_eager() {
                    mk_app!(ctx.js2n("array.ArrayOf"), sequence(schema.to_contract(ctx)))
                } else {
                    type_contract(TypeF::Array(Box::new(
                        TypeF::Contract(sequence(schema.to_contract(ctx))).into(),
                    )))
                }
            }
            Array::PerItem { initial, rest } => {
                let initial = initial.iter().map(|s| sequence(s.to_contract(ctx)));
                let rest = sequence(rest.to_contract(ctx));
                mk_app!(
                    ctx.js2n("array.Items"),
                    Term::Array(initial.collect(), ArrayAttrs::default()),
                    rest
                )
            }
            Array::MaxItems(n) => {
                mk_app!(ctx.js2n("array.MaxItems"), num(n))
            }
            Array::MinItems(n) => {
                mk_app!(ctx.js2n("array.MinItems"), num(n))
            }
            Array::UniqueItems => ctx.js2n("array.UniqueItems"),
            Array::Contains(schema) => {
                let contract = sequence(schema.to_contract(ctx.eager()));
                mk_app!(ctx.js2n("array.Contains"), contract)
            }
        }
    }
}

fn eagerly_disjoint<'a>(
    schemas: impl Iterator<Item = &'a Schema>,
    refs: &AcyclicReferences,
) -> bool {
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
