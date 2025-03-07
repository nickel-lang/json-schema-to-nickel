use std::{
    collections::{BTreeMap, BTreeSet, HashSet},
    convert::Infallible,
    str::FromStr,
};

use miette::miette;
use nickel_lang_core::{
    identifier::Ident,
    label::Label,
    mk_app,
    term::{
        array::ArrayAttrs,
        record::{Field, FieldMetadata, RecordAttrs, RecordData},
        LabeledType, Rational, RichTerm, Term, TypeAnnotation,
    },
    typ::{EnumRowF, EnumRows, EnumRowsF, RecordRows, Type, TypeF},
};
use ordered_float::NotNan;
use serde::Serialize;
use serde_json::{Map, Value};

use crate::{
    references::{SchemaPointer, SchemaPointerElt},
    utils::{distinct, lib_access, static_access},
};

// See https://github.com/orgs/json-schema-org/discussions/526#discussioncomment-7559030
// regarding the semantics of $ref in an object with other fields. Basically, we should interpret
//
// {
//   "$ref": "#foo",
//   "properties": ...
// }
//
// as
//
// {
//   "allOf": [
//     { "$ref": "#foo" },
//     { "properties": ... }
//   ]
// }

fn type_contract(ty: TypeF<Box<Type>, RecordRows, EnumRows, RichTerm>) -> RichTerm {
    Term::Type {
        typ: ty.into(),
        // We don't actually care about the contract -- it's a runtime thing.
        contract: Term::Null.into(),
    }
    .into()
}

fn num(x: NotNan<f64>) -> RichTerm {
    // unwrap: TODO json doesn't have infinity so this should be fine. But maybe there's a
    // better type than NotNan?
    Term::Num(Rational::try_from(x.into_inner()).unwrap()).into()
}

fn sequence(mut contracts: Vec<RichTerm>) -> RichTerm {
    if contracts.len() == 1 {
        contracts.pop().unwrap()
    } else {
        mk_app!(
            static_access("std", ["contract", "Sequence"]),
            Term::Array(contracts.into_iter().collect(), ArrayAttrs::default())
        )
    }
}

fn eagerly_disjoint<'a>(
    schemas: impl Iterator<Item = &'a Schema>,
    refs: &BTreeMap<String, Schema>,
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
    pub fn simple_type(&self, refs: &BTreeMap<String, Schema>) -> Option<InstanceType> {
        fn simple_type_rec<'s>(
            slf: &'s Schema,
            refs: &'s BTreeMap<String, Schema>,
            // Keeps track of the references we've followed to get to the current schema,
            // as protection against infinite recursion.
            followed: &mut HashSet<&'s str>,
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
                Schema::Ref(r) => {
                    if followed.insert(r) {
                        let ret = refs.get(r).and_then(|s| simple_type_rec(s, refs, followed));
                        followed.remove(r.as_str());
                        ret
                    } else {
                        None
                    }
                }
                Schema::AnyOf(_) => None,
                Schema::OneOf(_) => None,
                Schema::AllOf(_) => None,
                Schema::Ite { els, .. } => els.simple_type(refs),
                Schema::Not(_) => None,
            }
        }

        let mut followed = HashSet::new();
        simple_type_rec(self, refs, &mut followed)
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

    pub fn allowed_types_shallow(&self) -> InstanceTypeSet {
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
            Schema::Ref(_) => InstanceTypeSet::FULL,
            Schema::AnyOf(vec) => {
                let refs = BTreeMap::new();
                vec.iter()
                    .map(|s| s.simple_type(&refs))
                    .collect::<Option<InstanceTypeSet>>()
                    .unwrap_or(InstanceTypeSet::FULL)
            }
            Schema::OneOf(_) => InstanceTypeSet::FULL,
            Schema::AllOf(_) => InstanceTypeSet::FULL,
            Schema::Ite { .. } => InstanceTypeSet::FULL,
            Schema::Not(_) => InstanceTypeSet::FULL,
        }
    }

    pub fn to_contract(&self, eager: bool, refs: &BTreeMap<String, Schema>) -> Vec<RichTerm> {
        match self {
            Schema::Always => vec![lib_access(["Always"])],
            Schema::Never => vec![lib_access(["Never"])],
            Schema::Null => vec![lib_access(["Null"])],
            Schema::Boolean => vec![type_contract(TypeF::Bool)],
            Schema::Const(val) => {
                let nickel_val: RichTerm = serde_json::from_value(val.clone()).unwrap();
                if eager && (val.is_object() || val.is_array()) {
                    vec![mk_app!(lib_access(["eager", "const"]), nickel_val)]
                } else {
                    vec![mk_app!(
                        static_access("std", ["contract", "Equal"]),
                        nickel_val
                    )]
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
                        static_access("std", ["enum", "TagOrString"]),
                        type_contract(TypeF::Enum(enum_rows)),
                    ]
                } else {
                    Schema::AnyOf(vec.iter().map(|v| Schema::Const(v.clone())).collect())
                        .to_contract(eager, refs)
                }
            }
            Schema::Object(obj) => obj.to_contract(eager, refs),
            Schema::String(s) => vec![s.to_contract()],
            Schema::Number(num) => vec![num.to_contract()],
            Schema::Array(arr) => vec![arr.to_contract(eager, refs)],
            Schema::Ref(s) => vec![static_access("definitions", [s.as_str()])],
            Schema::AnyOf(vec) => {
                vec![if eager || !eagerly_disjoint(vec.iter(), refs) {
                    let contracts = vec
                        .iter()
                        .map(|s| sequence(s.to_contract(true, refs)))
                        .collect();
                    mk_app!(
                        lib_access(["any_of"]),
                        Term::Array(contracts, ArrayAttrs::default())
                    )
                } else {
                    let contracts = vec
                        .iter()
                        .map(|s| sequence(s.to_contract(false, refs)))
                        .collect();
                    mk_app!(
                        static_access("std", ["contract", "any_of"]),
                        Term::Array(contracts, ArrayAttrs::default())
                    )
                }]
            }
            Schema::OneOf(vec) => {
                let contracts = vec
                    .iter()
                    .map(|s| sequence(s.to_contract(true, refs)))
                    .collect();
                vec![mk_app!(
                    lib_access(["one_of"]),
                    Term::Array(contracts, ArrayAttrs::default())
                )]
            }
            Schema::AllOf(vec) => vec
                .iter()
                .flat_map(|s| s.to_contract(eager, refs))
                .collect(),
            Schema::Ite { iph, then, els } => todo!(),
            Schema::Not(schema) => {
                vec![mk_app!(
                    static_access("std", ["contract", "not"]),
                    sequence(schema.to_contract(eager, refs))
                )]
            }
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
    Dependencies(BTreeMap<String, Dependency>),
}

impl Obj {
    pub fn to_contract(&self, eager: bool, refs: &BTreeMap<String, Schema>) -> Vec<RichTerm> {
        match self {
            Obj::Any => vec![type_contract(TypeF::Dict {
                type_fields: Box::new(TypeF::Dyn.into()),
                flavour: nickel_lang_core::typ::DictTypeFlavour::Contract,
            })],
            Obj::Properties(op) => {
                vec![op.to_special_contract(eager, refs).unwrap_or_else(|| {
                    let f = if eager {
                        lib_access(["records", "eager", "record"])
                    } else {
                        lib_access(["records", "record"])
                    };
                    let additional_allowed =
                        !matches!(op.additional_properties.as_deref(), Some(Schema::Never));
                    let additional_contract = match op.additional_properties.as_deref() {
                        Some(s) => sequence(s.to_contract(eager, refs)),
                        None => type_contract(TypeF::Dyn),
                    };
                    mk_app!(
                        f,
                        Term::Record(RecordData::with_field_values(op.properties.iter().map(
                            |(k, v)| (k.into(), sequence(v.schema.to_contract(eager, refs)))
                        ))),
                        Term::Record(RecordData::with_field_values(
                            op.pattern_properties
                                .iter()
                                .map(|(k, v)| (k.into(), sequence(v.to_contract(eager, refs))))
                        )),
                        Term::Bool(additional_allowed),
                        additional_contract
                    )
                })]
            }
            Obj::MaxProperties(n) => {
                vec![mk_app!(
                    lib_access(["object", "max_properties"]),
                    Term::Num((*n).into())
                )]
            }
            Obj::MinProperties(n) => {
                vec![mk_app!(
                    lib_access(["object", "min_properties"]),
                    Term::Num((*n).into())
                )]
            }
            Obj::Required(names) => {
                vec![mk_app!(
                    lib_access(["object", "required"]),
                    Term::Array(
                        names.iter().map(|s| Term::Str(s.into()).into()).collect(),
                        ArrayAttrs::default()
                    )
                )]
            }
            Obj::PropertyNames(schema) => {
                vec![mk_app!(
                    lib_access(["object", "property_names"]),
                    sequence(schema.to_contract(eager, refs))
                )]
            }
            Obj::Dependencies(btree_map) => todo!(),
        }
    }
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq, Hash)]
pub struct Property {
    doc: Option<String>,
    schema: Schema,
    optional: bool,
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

impl ObjectProperties {
    pub fn to_special_contract(
        &self,
        eager: bool,
        refs: &BTreeMap<String, Schema>,
    ) -> Option<RichTerm> {
        let trivial_additional = matches!(
            self.additional_properties.as_deref(),
            None | Some(Schema::Always) | Some(Schema::Never)
        );
        let trivial_properties = self.properties.values().all(|p| p.schema == Schema::Always);
        if self.pattern_properties.is_empty()
            && trivial_additional
            && (!eager || trivial_properties)
        {
            let open = matches!(
                self.additional_properties.as_deref(),
                None | Some(Schema::Always)
            );

            let fields = self.properties.iter().map(|(name, prop)| {
                (
                    name.into(),
                    Field {
                        metadata: FieldMetadata {
                            annotation: TypeAnnotation {
                                typ: None,
                                contracts: prop
                                    .schema
                                    .to_contract(eager, refs)
                                    .into_iter()
                                    .map(|c| LabeledType {
                                        typ: TypeF::Contract(c).into(),
                                        label: Label::dummy(),
                                    })
                                    .collect(),
                            },
                            opt: prop.optional,
                            doc: prop.doc.clone(), // TODO: fill out the doc
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                )
            });

            Some(
                Term::Record(RecordData {
                    fields: fields.collect(),
                    attrs: RecordAttrs {
                        open,
                        ..Default::default()
                    },
                    ..Default::default()
                })
                .into(),
            )
        } else {
            None
        }
    }
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq, Hash)]
pub enum Dependency {
    Array(Vec<String>),
    Schema(Schema),
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq, Hash)]
pub enum Str {
    Any,
    MaxLength(u64),
    MinLength(u64),
    Pattern(String),
}

impl Str {
    pub fn to_contract(&self) -> RichTerm {
        match self {
            Str::Any => todo!(),
            Str::MaxLength(_) => todo!(),
            Str::MinLength(_) => todo!(),
            Str::Pattern(_) => todo!(),
        }
    }
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq, Hash)]
pub enum Num {
    Any,
    // The json-schema reference doesn't say this has to be an integer (and
    // if it isn't an integer it doesn't specify how rounding is supposed to
    // be handled).
    MultipleOf(i64),
    Maximum(NotNan<f64>),
    Minimum(NotNan<f64>),
    ExclusiveMinimum(NotNan<f64>),
    ExclusiveMaximum(NotNan<f64>),
    Integer,
}

impl Num {
    pub fn to_contract(&self) -> RichTerm {
        match self {
            Num::Any => type_contract(TypeF::Number),
            Num::MultipleOf(n) => {
                mk_app!(lib_access(["number", "multipleOf"]), Term::Num((*n).into()))
            }
            Num::Maximum(x) => mk_app!(lib_access(["number", "maximum"]), num(*x)),
            Num::Minimum(x) => mk_app!(lib_access(["number", "minimum"]), num(*x)),
            Num::ExclusiveMinimum(x) => {
                mk_app!(lib_access(["number", "exclusiveMinimum"]), num(*x))
            }
            Num::ExclusiveMaximum(x) => {
                mk_app!(lib_access(["number", "exclusiveMaximum"]), num(*x))
            }
            Num::Integer => static_access("std", ["number", "Integer"]),
        }
    }
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

impl Arr {
    pub fn to_contract(&self, eager: bool, refs: &BTreeMap<String, Schema>) -> RichTerm {
        todo!()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum InstanceType {
    Null = 0,
    Boolean,
    Object,
    Array,
    Number,
    String,
}

impl InstanceType {
    pub fn all() -> [InstanceType; 6] {
        use InstanceType::*;
        [Null, Boolean, Object, Array, Number, String]
    }

    pub fn to_schema(self) -> Schema {
        match self {
            InstanceType::Null => Schema::Null,
            InstanceType::Boolean => Schema::Boolean,
            InstanceType::Object => Schema::Object(Obj::Any),
            InstanceType::Array => Schema::Array(Arr::Any),
            InstanceType::Number => Schema::Number(Num::Any),
            InstanceType::String => Schema::String(Str::Any),
        }
    }
}

impl FromStr for InstanceType {
    type Err = miette::Report;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let ty = match s {
            "null" => InstanceType::Null,
            "boolean" => InstanceType::Boolean,
            "object" => InstanceType::Object,
            "array" => InstanceType::Array,
            "integer" => InstanceType::Number,
            "number" => InstanceType::Number,
            "string" => InstanceType::String,
            s => miette::bail!("unknown instance type `{s}`"),
        };
        Ok(ty)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InstanceTypeSet {
    inner: u8,
}

impl InstanceTypeSet {
    const FULL: InstanceTypeSet = InstanceTypeSet { inner: 0b0011_1111 };
    const EMPTY: InstanceTypeSet = InstanceTypeSet { inner: 0b0000_0000 };

    pub fn singleton(ty: InstanceType) -> Self {
        Self {
            inner: 1 << ty as u8,
        }
    }

    pub fn insert(&mut self, ty: InstanceType) {
        self.inner |= 1 << ty as u8;
    }

    pub fn contains(self, ty: InstanceType) -> bool {
        self.inner & (1 << ty as u8) != 0
    }

    pub fn intersect(self, other: InstanceTypeSet) -> InstanceTypeSet {
        InstanceTypeSet {
            inner: self.inner & other.inner,
        }
    }

    pub fn to_schema(self) -> Schema {
        let types: Vec<_> = InstanceType::all()
            .into_iter()
            .filter(|ty| self.contains(*ty))
            .collect();

        match types.as_slice() {
            [] => Schema::Never,
            [ty] => ty.to_schema(),
            _ => Schema::AnyOf(types.into_iter().map(InstanceType::to_schema).collect()),
        }
    }
}

impl FromIterator<InstanceType> for InstanceTypeSet {
    fn from_iter<T: IntoIterator<Item = InstanceType>>(iter: T) -> Self {
        let mut ret = InstanceTypeSet::EMPTY;
        for ty in iter.into_iter() {
            ret.insert(ty);
        }
        ret
    }
}

impl<'a> TryFrom<&'a serde_json::Value> for Schema {
    type Error = miette::Report;

    fn try_from(value: &'a serde_json::Value) -> Result<Self, Self::Error> {
        if let Some(b) = value.as_bool() {
            return if b {
                Ok(Schema::Always)
            } else {
                Ok(Schema::Never)
            };
        }

        let obj = value
            .as_object()
            .ok_or_else(|| miette!("schema must be an object"))?;

        let mut num_schemas = Vec::new();
        let types = match obj.get("type") {
            Some(tys) => {
                if let Some(s) = tys.as_str() {
                    if s == "integer" {
                        num_schemas.push(Schema::Number(Num::Integer));
                    }
                    InstanceTypeSet::singleton(s.parse()?)
                } else if let Some(a) = tys.as_array() {
                    if a.iter().any(|v| v.as_str() == Some("integer")) {
                        num_schemas.push(Schema::Number(Num::Integer));
                    }

                    a.iter()
                        .map(|value| {
                            value
                                .as_str()
                                .ok_or_else(|| miette!("instance type element must be a string"))?
                                .parse()
                        })
                        .collect::<miette::Result<InstanceTypeSet>>()?
                } else {
                    miette::bail!("instance type must be a string or an array");
                }
            }
            None => InstanceTypeSet::FULL,
        };

        let mut and_schemas = Vec::new();
        let mut or_schemas = Vec::new();

        if types.contains(InstanceType::Null) {
            or_schemas.push(Schema::Null);
        }

        if types.contains(InstanceType::Boolean) {
            or_schemas.push(Schema::Boolean);
        }

        if types.contains(InstanceType::Number) {
            num_schemas.extend(extract_number_schemas(obj)?);
            if !num_schemas.iter().any(|s| matches!(s, Schema::Number(_))) {
                num_schemas.push(Schema::Number(Num::Any));
            }

            or_schemas.push(Schema::AllOf(num_schemas));
        }

        if types.contains(InstanceType::String) {
            let mut str_schemas = extract_string_schemas(obj)?;
            if str_schemas.is_empty() {
                str_schemas.push(Schema::String(Str::Any));
            }

            or_schemas.push(Schema::AllOf(str_schemas));
        }

        if types.contains(InstanceType::Object) {
            let mut obj_schemas = extract_object_schemas(obj)?;
            if obj_schemas.is_empty() {
                obj_schemas.push(Schema::Object(Obj::Any));
            }

            or_schemas.push(Schema::AllOf(obj_schemas));
        }

        if types.contains(InstanceType::Array) {
            let mut arr_schemas = extract_array_schemas(obj)?;
            if arr_schemas.is_empty() {
                arr_schemas.push(Schema::Array(Arr::Any));
            }

            or_schemas.push(Schema::AllOf(arr_schemas));
        }

        if !or_schemas.is_empty() {
            and_schemas.push(Schema::AnyOf(or_schemas));
        }

        if let Some(any_of) = get_non_empty_array(obj, "anyOf")? {
            let mut any_of_schemas = Vec::new();
            for value in any_of {
                any_of_schemas.push(value.try_into()?);
            }
            and_schemas.push(Schema::AnyOf(any_of_schemas));
        }

        if let Some(one_of) = get_non_empty_array(obj, "oneOf")? {
            let mut one_of_schemas = Vec::new();
            for value in one_of {
                one_of_schemas.push(value.try_into()?);
            }

            and_schemas.push(Schema::OneOf(one_of_schemas));
        }

        if let Some(all_of) = get_non_empty_array(obj, "allOf")? {
            for value in all_of {
                and_schemas.push(value.try_into()?);
            }
        }

        if let Some(iph) = obj.get("if") {
            let iph = Schema::try_from(iph)?;
            let then = match obj.get("then") {
                Some(then_obj) => then_obj.try_into()?,
                None => Schema::Always,
            };
            let els = match obj.get("else") {
                Some(els_obj) => els_obj.try_into()?,
                None => Schema::Always,
            };

            and_schemas.push(Schema::Ite {
                iph: Box::new(iph),
                then: Box::new(then),
                els: Box::new(els),
            });
        }

        if let Some(not) = obj.get("not") {
            and_schemas.push(Schema::Not(Box::new(not.try_into()?)));
        }

        if let Some(konst) = obj.get("const") {
            and_schemas.push(Schema::Const(konst.clone()));
        }

        // It SHOULD be a non-empty array, and that's easier to handle for now.
        if let Some(inum) = get_non_empty_array(obj, "enum")? {
            and_schemas.push(Schema::Enum(inum.clone()));
        }

        if let Some(reff) = get_string(obj, "$ref")? {
            and_schemas.push(Schema::Ref(reff.to_owned()));
        }

        Ok(Schema::AllOf(and_schemas))
    }
}

fn get_unsigned(obj: &Map<String, Value>, field: &str) -> miette::Result<Option<u64>> {
    if let Some(val) = obj.get(field) {
        match val.as_number() {
            Some(n) => Ok(Some(
                n.as_u64()
                    // TODO: better number handling
                    .ok_or_else(|| miette!("{field} was a weird number"))?,
            )),
            None => miette::bail!("{field} must be a non-negative integer"),
        }
    } else {
        Ok(None)
    }
}

fn get_integer(obj: &Map<String, Value>, field: &str) -> miette::Result<Option<i64>> {
    if let Some(val) = obj.get(field) {
        match val.as_number() {
            Some(n) => Ok(Some(
                n.as_i64()
                    // TODO: better number handling
                    .ok_or_else(|| miette!("{field} was a weird number"))?,
            )),
            None => miette::bail!("{field} must be an integer"),
        }
    } else {
        Ok(None)
    }
}

fn get_number(obj: &Map<String, Value>, field: &str) -> miette::Result<Option<f64>> {
    if let Some(val) = obj.get(field) {
        match val.as_number() {
            Some(n) => Ok(Some(
                n.as_f64()
                    // TODO: better number handling
                    .ok_or_else(|| miette!("{field} was a weird number"))?,
            )),
            None => miette::bail!("{field} must be a number"),
        }
    } else {
        Ok(None)
    }
}

fn get_string(obj: &Map<String, Value>, field: &str) -> miette::Result<Option<String>> {
    if let Some(val) = obj.get(field) {
        match val.as_str() {
            Some(s) => Ok(Some(s.to_owned())),
            None => miette::bail!("{field} must be a string"),
        }
    } else {
        Ok(None)
    }
}

fn get_non_empty_array<'a>(
    obj: &'a Map<String, Value>,
    field: &str,
) -> miette::Result<Option<&'a Vec<Value>>> {
    if let Some(arr) = get_array(obj, field)? {
        if arr.is_empty() {
            miette::bail!("{field} van't be empty")
        } else {
            Ok(Some(arr))
        }
    } else {
        Ok(None)
    }
}

fn get_array<'a>(
    obj: &'a Map<String, Value>,
    field: &str,
) -> miette::Result<Option<&'a Vec<Value>>> {
    if let Some(val) = obj.get(field) {
        match val.as_array() {
            Some(arr) => Ok(Some(arr)),
            None => miette::bail!("{field} must be an array"),
        }
    } else {
        Ok(None)
    }
}

fn get_object<'a>(
    obj: &'a Map<String, Value>,
    field: &str,
) -> miette::Result<Option<&'a Map<String, Value>>> {
    if let Some(val) = obj.get(field) {
        match val.as_object() {
            Some(obj) => Ok(Some(obj)),
            None => miette::bail!("{field} must be an object"),
        }
    } else {
        Ok(None)
    }
}

fn extract_string_schemas(obj: &Map<String, Value>) -> miette::Result<Vec<Schema>> {
    let mut ret = Vec::new();

    if let Some(max) = get_unsigned(obj, "maxLength")? {
        ret.push(Schema::String(Str::MaxLength(max)));
    }

    if let Some(min) = get_unsigned(obj, "minLength")? {
        ret.push(Schema::String(Str::MinLength(min)));
    }

    if let Some(pattern) = get_string(obj, "pattern")? {
        ret.push(Schema::String(Str::Pattern(pattern)));
    }

    Ok(ret)
}

fn extract_number_schemas(obj: &Map<String, Value>) -> miette::Result<Vec<Schema>> {
    let mut ret = Vec::new();

    if let Some(mult) = get_integer(obj, "multipleOf")? {
        ret.push(Schema::Number(Num::MultipleOf(mult)));
    }
    if let Some(max) = get_number(obj, "maximum")? {
        ret.push(Schema::Number(Num::Maximum(NotNan::try_from(max).unwrap())));
    }
    if let Some(min) = get_number(obj, "minimum")? {
        ret.push(Schema::Number(Num::Minimum(NotNan::try_from(min).unwrap())));
    }
    if let Some(ex_max) = get_number(obj, "exclusiveMaximum")? {
        ret.push(Schema::Number(Num::ExclusiveMaximum(
            NotNan::try_from(ex_max).unwrap(),
        )));
    }
    if let Some(ex_min) = get_number(obj, "exclusiveMinimum")? {
        ret.push(Schema::Number(Num::ExclusiveMinimum(
            NotNan::try_from(ex_min).unwrap(),
        )));
    }

    Ok(ret)
}

fn extract_object_schemas(obj: &Map<String, Value>) -> miette::Result<Vec<Schema>> {
    // Properties(ObjectProperties),
    //
    let mut ret = Vec::new();

    if let Some(max) = get_unsigned(obj, "maxProperties")? {
        ret.push(Schema::Object(Obj::MaxProperties(max)));
    }
    if let Some(min) = get_unsigned(obj, "minProperties")? {
        ret.push(Schema::Object(Obj::MinProperties(min)));
    }
    if let Some(req) = get_array(obj, "required")? {
        if !req.is_empty() {
            let req: BTreeSet<String> = req
                .iter()
                .map(|name| {
                    name.as_str()
                        .ok_or_else(|| miette!("elements of \"required\" must be strings"))
                        .map(str::to_owned)
                })
                .collect::<miette::Result<_>>()?;
            ret.push(Schema::Object(Obj::Required(req)));
        }
    }
    if let Some(names) = obj.get("propertyNames") {
        ret.push(Schema::Object(Obj::PropertyNames(Box::new(
            names.try_into()?,
        ))));
    }
    if let Some(deps) = get_object(obj, "dependencies")? {
        let mut deps_map = BTreeMap::new();
        for (field, val) in deps {
            if let Some(arr) = val.as_array() {
                let names = arr
                    .iter()
                    .map(|v| {
                        v.as_str()
                            .ok_or_else(|| miette!("elements of dependency array must be strings"))
                            .map(str::to_owned)
                    })
                    .collect::<miette::Result<_>>()?;
                deps_map.insert(field.to_owned(), Dependency::Array(names));
            } else {
                deps_map.insert(field.to_owned(), Dependency::Schema(val.try_into()?));
            }
        }
    }

    let mut properties = ObjectProperties {
        properties: BTreeMap::new(),
        pattern_properties: BTreeMap::new(),
        additional_properties: None,
    };
    if let Some(props) = get_object(obj, "properties")? {
        properties.properties = props
            .iter()
            .map(|(k, v)| Ok((k.to_owned(), Schema::try_from(v)?.into())))
            .collect::<miette::Result<_>>()?;
    };
    if let Some(pats) = get_object(obj, "patternProperties")? {
        properties.pattern_properties = pats
            .iter()
            .map(|(k, v)| Ok((k.to_owned(), v.try_into()?)))
            .collect::<miette::Result<_>>()?;
    }
    if let Some(adds) = obj.get("additionalProperties") {
        properties.additional_properties = Some(Box::new(adds.try_into()?));
    }

    if !properties.properties.is_empty()
        || !properties.pattern_properties.is_empty()
        || properties.additional_properties.is_some()
    {
        ret.push(Schema::Object(Obj::Properties(properties)));
    }

    Ok(ret)
}

fn extract_array_schemas(obj: &Map<String, Value>) -> miette::Result<Vec<Schema>> {
    let mut ret = Vec::new();

    if let Some(max) = get_unsigned(obj, "maxItems")? {
        ret.push(Schema::Array(Arr::MaxItems(max)));
    }
    if let Some(min) = get_unsigned(obj, "minItems")? {
        ret.push(Schema::Array(Arr::MinItems(min)));
    }
    if let Some(unique) = obj.get("uniqueItems") {
        if let Some(unique) = unique.as_bool() {
            if unique {
                ret.push(Schema::Array(Arr::UniqueItems));
            }
        } else {
            miette::bail!("\"uniqueItems\" must be a boolean");
        }
    }
    if let Some(contains) = obj.get("contains") {
        ret.push(Schema::Array(Arr::Contains(Box::new(contains.try_into()?))));
    }

    if let Some(items) = obj.get("items") {
        if let Some(arr) = items.as_array() {
            let initial: Vec<_> = arr
                .iter()
                .map(Schema::try_from)
                .collect::<miette::Result<_>>()?;

            let rest = match obj.get("additionalItems") {
                Some(add) => add.try_into()?,
                None => Schema::Always,
            };
            ret.push(Schema::Array(Arr::PerItem {
                initial,
                rest: Box::new(rest),
            }));
        } else {
            // If items is a single item, we ignore additionalItems.
            ret.push(Schema::Array(Arr::AllItems(Box::new(items.try_into()?))));
        }
    }

    Ok(ret)
}

#[derive(Copy, Clone, PartialEq)]
enum TraverseOrder {
    TopDown,
    BottomUp,
}

trait Traverse<T>: Sized {
    fn traverse<F, E>(self, f: &mut F, order: TraverseOrder) -> Result<Self, E>
    where
        F: FnMut(T) -> Result<T, E>;
}

impl Traverse<Schema> for Schema {
    fn traverse<F, E>(self, f: &mut F, order: TraverseOrder) -> Result<Self, E>
    where
        F: FnMut(Schema) -> Result<Schema, E>,
    {
        let val = match order {
            TraverseOrder::TopDown => f(self)?,
            TraverseOrder::BottomUp => self,
        };

        let val = match val {
            Schema::Always
            | Schema::Never
            | Schema::Null
            | Schema::Boolean
            | Schema::Const(_)
            | Schema::Enum(_)
            | Schema::Number(_)
            | Schema::String(_)
            | Schema::Ref(_) => val,
            Schema::Object(obj) => Schema::Object(obj.traverse(f, order)?),
            Schema::Array(arr) => Schema::Array(arr.traverse(f, order)?),
            Schema::AnyOf(vec) => Schema::AnyOf(vec.traverse(f, order)?),
            Schema::OneOf(vec) => Schema::OneOf(vec.traverse(f, order)?),
            Schema::AllOf(vec) => Schema::AllOf(vec.traverse(f, order)?),
            Schema::Ite { iph, then, els } => Schema::Ite {
                iph: iph.traverse(f, order)?,
                then: then.traverse(f, order)?,
                els: els.traverse(f, order)?,
            },
            Schema::Not(schema) => Schema::Not(schema.traverse(f, order)?),
        };

        match order {
            TraverseOrder::TopDown => Ok(val),
            TraverseOrder::BottomUp => f(val),
        }
    }
}

impl<S, T: Traverse<S>> Traverse<S> for Box<T> {
    fn traverse<F, E>(self, f: &mut F, order: TraverseOrder) -> Result<Self, E>
    where
        F: FnMut(S) -> Result<S, E>,
    {
        (*self).traverse(f, order).map(Box::new)
    }
}

impl<S, T: Traverse<S>> Traverse<S> for Option<T> {
    fn traverse<F, E>(self, f: &mut F, order: TraverseOrder) -> Result<Self, E>
    where
        F: FnMut(S) -> Result<S, E>,
    {
        self.map(|v| v.traverse(f, order)).transpose()
    }
}

impl<S, T: Traverse<S>> Traverse<S> for Vec<T> {
    fn traverse<F, E>(self, f: &mut F, order: TraverseOrder) -> Result<Self, E>
    where
        F: FnMut(S) -> Result<S, E>,
    {
        self.into_iter().map(|x| x.traverse(f, order)).collect()
    }
}

impl<K: Ord + Eq + std::fmt::Debug, S, T: Traverse<S>> Traverse<S> for BTreeMap<K, T> {
    fn traverse<F, E>(self, f: &mut F, order: TraverseOrder) -> Result<Self, E>
    where
        F: FnMut(S) -> Result<S, E>,
    {
        self.into_iter()
            .map(|(k, v)| Ok((k, v.traverse(f, order)?)))
            .collect()
    }
}

impl Traverse<Schema> for Property {
    fn traverse<F, E>(self, f: &mut F, order: TraverseOrder) -> Result<Self, E>
    where
        F: FnMut(Schema) -> Result<Schema, E>,
    {
        Ok(Property {
            doc: self.doc,
            optional: self.optional,
            schema: self.schema.traverse(f, order)?,
        })
    }
}

impl Traverse<Schema> for Obj {
    fn traverse<F, E>(self, f: &mut F, order: TraverseOrder) -> Result<Self, E>
    where
        F: FnMut(Schema) -> Result<Schema, E>,
    {
        let val = match self {
            Obj::Any | Obj::MaxProperties(_) | Obj::MinProperties(_) | Obj::Required(_) => self,
            Obj::PropertyNames(schema) => Obj::PropertyNames(schema.traverse(f, order)?),
            Obj::Dependencies(hash_map) => Obj::Dependencies(hash_map.traverse(f, order)?),
            Obj::Properties(props) => Obj::Properties(ObjectProperties {
                properties: props.properties.traverse(f, order)?,
                pattern_properties: props.pattern_properties.traverse(f, order)?,
                additional_properties: props.additional_properties.traverse(f, order)?,
            }),
        };
        Ok(val)
    }
}

impl Traverse<Schema> for Dependency {
    fn traverse<F, E>(self, f: &mut F, order: TraverseOrder) -> Result<Self, E>
    where
        F: FnMut(Schema) -> Result<Schema, E>,
    {
        let val = match self {
            Dependency::Array(_) => self,
            Dependency::Schema(schema) => Dependency::Schema(schema.traverse(f, order)?),
        };
        Ok(val)
    }
}

impl Traverse<Schema> for Arr {
    fn traverse<F, E>(self, f: &mut F, order: TraverseOrder) -> Result<Self, E>
    where
        F: FnMut(Schema) -> Result<Schema, E>,
    {
        let val = match self {
            Arr::Any | Arr::MaxItems(_) | Arr::MinItems(_) | Arr::UniqueItems => self,
            Arr::AllItems(schema) => Arr::AllItems(schema.traverse(f, order)?),
            Arr::PerItem { initial, rest } => Arr::PerItem {
                initial: initial.traverse(f, order)?,
                rest: rest.traverse(f, order)?,
            },
            Arr::Contains(schema) => Arr::Contains(schema.traverse(f, order)?),
        };
        Ok(val)
    }
}

pub fn resolve_references(value: &Value, schema: Schema) -> (Schema, BTreeMap<String, Schema>) {
    let mut refs = BTreeMap::new();
    let mut record_ref = |schema: Schema| -> Result<Schema, Infallible> {
        if let Schema::Ref(s) = schema {
            let Some(stripped) = s.strip_prefix("#/") else {
                eprintln!("skipping unsupported pointer \"{s}\"");
                return Ok(Schema::Ref(s));
            };
            if !refs.contains_key(&s) {
                match SchemaPointer::parse(stripped) {
                    Err(e) => {
                        eprintln!("skipping unparseable pointer \"{s}\": {e}");
                    }
                    Ok(ptr) => match resolve_ptr(&ptr, value) {
                        Ok(val) => match Schema::try_from(val) {
                            Ok(v) => {
                                refs.insert(s.clone(), v);
                            }
                            Err(e) => {
                                eprintln!("skipping pointer \"{s}\" because we failed to convert the pointee: {e}");
                            }
                        },
                        Err(e) => {
                            eprintln!("skipping pointer \"{s}\" because it failed to resolve: {e}");
                        }
                    },
                }
            }
            Ok(Schema::Ref(s))
        } else {
            Ok(schema)
        }
    };

    let schema = schema
        .traverse(&mut record_ref, TraverseOrder::BottomUp)
        .unwrap();
    (schema, refs)
}

impl SchemaPointerElt {
    fn name(&self) -> &str {
        match self {
            SchemaPointerElt::Definitions(_) => "definitions",
            SchemaPointerElt::Properties(_) => "properties",
            SchemaPointerElt::AdditionalProperties => "additionalProperties",
            SchemaPointerElt::ItemsIndexed(_) => "items",
            SchemaPointerElt::ItemsSingle => "items",
            SchemaPointerElt::Contains => "contains",
            SchemaPointerElt::AllOf(_) => "allOf",
            SchemaPointerElt::AnyOf(_) => "anyOf",
            SchemaPointerElt::OneOf(_) => "oneOf",
            SchemaPointerElt::Not => "not",
            SchemaPointerElt::Then => "then",
            SchemaPointerElt::Else => "else",
        }
    }
}

fn resolve_ptr<'a>(ptr: &SchemaPointer, root: &'a Value) -> miette::Result<&'a Value> {
    let mut val = root;
    let Some(root) = root.as_object() else {
        miette::bail!("root must be an object");
    };
    for elt in ptr.path.iter() {
        match elt {
            SchemaPointerElt::Definitions(name) => {
                val = get_object(root, "definitions")?
                    .ok_or_else(|| miette!("no definitions in the root"))?
                    .get(name)
                    .ok_or_else(|| miette!("missing {name}"))?;
            }
            SchemaPointerElt::Properties(name) => {
                let Some(obj) = val.as_object() else {
                    miette::bail!("cannot look up {} in a non-object", elt.name());
                };
                let Some(props) = get_object(obj, "properties")? else {
                    miette::bail!("no \"properties\" field");
                };
                val = props
                    .get(name)
                    .ok_or_else(|| miette!("field {name} not found"))?;
            }
            SchemaPointerElt::ItemsIndexed(i)
            | SchemaPointerElt::AllOf(i)
            | SchemaPointerElt::AnyOf(i)
            | SchemaPointerElt::OneOf(i) => {
                let Some(obj) = val.as_object() else {
                    miette::bail!("cannot look up {} in a non-object", elt.name());
                };

                let field = get_array(obj, elt.name())?
                    .ok_or_else(|| miette!("field {} not found", elt.name()))?;

                val = field
                    .get(*i)
                    .ok_or_else(|| miette!("array has no index {i}"))?;
            }
            SchemaPointerElt::ItemsSingle
            | SchemaPointerElt::Contains
            | SchemaPointerElt::AdditionalProperties
            | SchemaPointerElt::Not
            | SchemaPointerElt::Then
            | SchemaPointerElt::Else => {
                let Some(obj) = val.as_object() else {
                    miette::bail!("cannot look up {} in a non-object", elt.name());
                };

                val = obj
                    .get(elt.name())
                    .ok_or(miette!("field {} not found", elt.name()))?;
            }
        };
    }
    Ok(val)
}

pub fn flatten_logical_ops(schema: Schema) -> Schema {
    fn flatten_one(schema: Schema) -> Result<Schema, Infallible> {
        let ret = match schema {
            Schema::AnyOf(vec) | Schema::OneOf(vec) if vec.is_empty() => Schema::Never,
            Schema::AllOf(vec) if vec.is_empty() => Schema::Always,

            Schema::AnyOf(mut vec) | Schema::AllOf(mut vec) | Schema::OneOf(mut vec)
                if vec.len() == 1 =>
            {
                vec.pop().unwrap()
            }

            Schema::AllOf(vec) => {
                // This is a perfect application of Vec::extract_if, if it were stable.
                let mut new_vec = Vec::new();
                for elt in vec {
                    match elt {
                        Schema::AllOf(e) => new_vec.extend(e),
                        Schema::Always => {}
                        Schema::Never => {
                            return Ok(Schema::Never);
                        }
                        e => new_vec.push(e),
                    }
                }
                Schema::AllOf(new_vec)
            }

            Schema::AnyOf(vec) => {
                // This is a perfect application of Vec::extract_if, if it were stable.
                let mut new_vec = Vec::new();
                for elt in vec {
                    match elt {
                        Schema::AnyOf(e) => new_vec.extend(e),
                        Schema::Always => {
                            return Ok(Schema::Always);
                        }
                        Schema::Never => {}
                        e => new_vec.push(e),
                    }
                }
                Schema::AnyOf(new_vec)
            }
            s => s,
        };
        Ok(ret)
    }

    schema
        .traverse(&mut flatten_one, TraverseOrder::BottomUp)
        .unwrap()
}

pub fn one_to_any(schema: Schema, refs: &BTreeMap<String, Schema>) -> Schema {
    fn distinct_types(s: &[Schema], refs: &BTreeMap<String, Schema>) -> bool {
        let simple_types = s
            .iter()
            .map(|s| s.simple_type(refs))
            .collect::<Option<Vec<_>>>();

        matches!(simple_types, Some(tys) if distinct(tys.iter()))
    }

    let mut one_to_any_one = |schema: Schema| -> Result<Schema, Infallible> {
        let ret = match schema {
            Schema::OneOf(vec) if distinct_types(&vec, refs) => Schema::AnyOf(vec),
            s => s,
        };
        Ok(ret)
    };

    schema
        .traverse(&mut one_to_any_one, TraverseOrder::BottomUp)
        .unwrap()
}

pub fn simplify(mut schema: Schema, refs: &BTreeMap<String, Schema>) -> Schema {
    loop {
        let prev = schema.clone();
        schema = flatten_logical_ops(schema);
        schema = one_to_any(schema, refs);
        schema = intersect_types(schema, refs);
        schema = merge_required_properties(schema);

        if schema == prev {
            return schema;
        }
    }
}

pub fn inline_refs(schema: Schema, refs: &BTreeMap<String, Schema>) -> Schema {
    let mut inline_one = |schema: Schema| -> Result<Schema, Infallible> {
        let ret = match schema {
            Schema::Ref(s) => match refs.get(&s) {
                // The ref-inlining heuristic could use some work. We probably
                // want to avoid inlining large schemas, and we probably want to
                // prioritize inlining things that can lead to further simplication.
                Some(resolved @ Schema::AnyOf(tys))
                    if tys.iter().all(|ty| ty.just_type().is_some()) =>
                {
                    resolved.clone()
                }
                Some(_) => Schema::Ref(s),
                None => Schema::Always,
            },
            s => s,
        };
        Ok(ret)
    };

    schema
        .traverse(&mut inline_one, TraverseOrder::BottomUp)
        .unwrap()
}

pub fn merge_required_properties(schema: Schema) -> Schema {
    let mut merge_one = |schema: Schema| -> Result<Schema, Infallible> {
        let ret = match schema {
            Schema::AllOf(vec) => {
                let mut required = BTreeSet::new();
                let mut props = Vec::new();
                let mut new_vec = Vec::new();

                for s in vec {
                    match s {
                        Schema::Object(Obj::Required(strings)) => {
                            required.extend(strings.into_iter())
                        }
                        Schema::Object(Obj::Properties(p)) => props.push(p),
                        s => new_vec.push(s),
                    }
                }

                let mut unused_required = required.clone();

                for mut p in props {
                    for (name, prop) in p.properties.iter_mut() {
                        if required.contains(name) {
                            unused_required.remove(name);
                            prop.optional = false;
                        }
                    }
                    new_vec.push(Schema::Object(Obj::Properties(p)));
                }

                if !unused_required.is_empty() {
                    new_vec.push(Schema::Object(Obj::Required(unused_required)))
                }
                Schema::AllOf(new_vec)
            }
            s => s,
        };
        Ok(ret)
    };

    schema
        .traverse(&mut merge_one, TraverseOrder::BottomUp)
        .unwrap()
}

pub fn intersect_types(schema: Schema, refs: &BTreeMap<String, Schema>) -> Schema {
    let mut intersect_one = |schema: Schema| -> Result<Schema, Infallible> {
        let ret = match schema {
            Schema::AllOf(mut vec) => {
                // The set of allowed types is the intersection, over all elements of `vec`,
                // of that schema's set of allowed types.
                let mut allowed_types = InstanceTypeSet::FULL;

                for s in &vec {
                    allowed_types = allowed_types.intersect(s.allowed_types_shallow());
                }

                vec.retain_mut(|s| {
                    if s.just_type_set().is_some() {
                        // We filter out all the elements that are just type restrictions, since
                        // there could be repeats. Then we'll add back in one if necessary.
                        false
                    } else if let Schema::AnyOf(schemas) = s {
                        schemas.retain(|s| {
                            s.simple_type(refs)
                                .is_none_or(|ty| allowed_types.contains(ty))
                        });
                        true
                    } else {
                        s.simple_type(refs)
                            .is_none_or(|ty| allowed_types.contains(ty))
                    }
                });

                if !vec
                    .iter()
                    .any(|s| s.allowed_types_shallow() == allowed_types)
                {
                    vec.push(allowed_types.to_schema());
                }

                Schema::AllOf(vec)
            }
            s => s,
        };

        Ok(ret)
    };

    schema
        .traverse(&mut intersect_one, TraverseOrder::BottomUp)
        .unwrap()
}

#[cfg(test)]
mod tests {
    use std::io::stdout;

    use nickel_lang_core::pretty::*;

    use super::*;

    #[test]
    fn hack() {
        // let data =
        //     std::fs::read_to_string("examples/github-workflow/github-workflow.json").unwrap();
        let data = std::fs::read_to_string("examples/simple-schema/test.schema.json").unwrap();
        let val: serde_json::Value = serde_json::from_str(&data).unwrap();
        let schema: super::Schema = (&val).try_into().unwrap();
        dbg!(&schema);
        let (schema, refs) = resolve_references(&val, schema);
        dbg!(&refs);

        let refs = refs
            .iter()
            .map(|(k, v)| (k.clone(), simplify(v.clone(), &refs)))
            .collect();
        dbg!(&refs);
        let schema = inline_refs(schema, &refs);
        let schema = simplify(schema, &refs);
        dbg!(&schema);

        let rt = dbg!(schema.to_contract(false, &refs));
        let pretty_alloc = Allocator::default();
        let out = sequence(rt).pretty(&pretty_alloc);
        out.render(80, &mut stdout()).unwrap();
    }
}
