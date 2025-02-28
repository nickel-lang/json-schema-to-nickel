use std::{
    collections::{HashMap, HashSet},
    convert::Infallible,
    str::FromStr,
};

use miette::miette;
use serde_json::{Map, Value};

use crate::references::{SchemaPointer, SchemaPointerElt};

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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub enum Obj {
    Any,
    Properties(ObjectProperties),
    MaxProperties(u64),
    MinProperties(u64),
    Required(HashSet<String>),
    // This could be simplified maybe, because we're guaranteed that this will only need to validate strings.
    PropertyNames(Box<Schema>),
    Dependencies(HashMap<String, Dependency>),
}

#[derive(Clone, Debug)]
pub struct ObjectProperties {
    pub properties: HashMap<String, Schema>,
    pub pattern_properties: HashMap<String, Schema>,
    pub additional_properties: Option<Box<Schema>>,
}

#[derive(Clone, Debug)]
pub enum Dependency {
    Array(Vec<String>),
    Schema(Schema),
}

#[derive(Clone, Debug)]
pub enum Str {
    Any,
    MaxLength(u64),
    MinLength(u64),
    Pattern(String),
}

#[derive(Clone, Debug)]
pub enum Num {
    Any,
    // The json-schema reference doesn't say this has to be an integer (and
    // if it isn't an integer it doesn't specify how rounding is supposed to
    // be handled).
    MultipleOf(i64),
    Maximum(f64),
    Minimum(f64),
    ExclusiveMinimum(f64),
    ExclusiveMaximum(f64),
    Integer,
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum InstanceType {
    Null,
    Boolean,
    Object,
    Array,
    Number,
    String,
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
                    vec![s.parse()?]
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
                        .collect::<miette::Result<Vec<_>>>()?
                } else {
                    miette::bail!("instance type must be a string or an array");
                }
            }
            None => vec![
                InstanceType::Null,
                InstanceType::Boolean,
                InstanceType::Object,
                InstanceType::Array,
                InstanceType::Number,
                InstanceType::String,
            ],
        };

        let mut and_schemas = Vec::new();
        let mut or_schemas = Vec::new();

        if types.contains(&InstanceType::Null) {
            or_schemas.push(Schema::Null);
        }

        if types.contains(&InstanceType::Boolean) {
            or_schemas.push(Schema::Boolean);
        }

        if types.contains(&InstanceType::Number) {
            num_schemas.extend(extract_number_schemas(obj)?);
            if !num_schemas.iter().any(|s| matches!(s, Schema::Number(_))) {
                num_schemas.push(Schema::Number(Num::Any));
            }

            or_schemas.push(Schema::AllOf(num_schemas));
        }

        if types.contains(&InstanceType::String) {
            let mut str_schemas = extract_string_schemas(obj)?;
            if str_schemas.is_empty() {
                str_schemas.push(Schema::String(Str::Any));
            }

            or_schemas.push(Schema::AllOf(str_schemas));
        }

        if types.contains(&InstanceType::Object) {
            let mut obj_schemas = extract_object_schemas(obj)?;
            if obj_schemas.is_empty() {
                obj_schemas.push(Schema::Object(Obj::Any));
            }

            or_schemas.push(Schema::AllOf(obj_schemas));
        }

        if types.contains(&InstanceType::Array) {
            let mut arr_schemas = extract_array_schemas(obj)?;
            if arr_schemas.is_empty() {
                arr_schemas.push(Schema::Array(Arr::Any));
            }

            or_schemas.push(Schema::AllOf(arr_schemas));
        }

        if let Some(any_of) = get_non_empty_array(obj, "anyOf")? {
            for value in any_of {
                or_schemas.push(value.try_into()?);
            }
        }

        if !or_schemas.is_empty() {
            and_schemas.push(Schema::AnyOf(or_schemas));
        }

        let mut one_of_schemas = Vec::new();
        if let Some(one_of) = get_non_empty_array(obj, "oneOf")? {
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
        ret.push(Schema::Number(Num::Maximum(max)));
    }
    if let Some(min) = get_number(obj, "minimum")? {
        ret.push(Schema::Number(Num::Minimum(min)));
    }
    if let Some(ex_max) = get_number(obj, "exclusiveMaximum")? {
        ret.push(Schema::Number(Num::ExclusiveMaximum(ex_max)));
    }
    if let Some(ex_min) = get_number(obj, "exclusiveMinimum")? {
        ret.push(Schema::Number(Num::ExclusiveMinimum(ex_min)));
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
            let req: HashSet<String> = req
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
        let mut deps_map = HashMap::new();
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
        properties: HashMap::new(),
        pattern_properties: HashMap::new(),
        additional_properties: None,
    };
    if let Some(props) = get_object(obj, "properties")? {
        properties.properties = props
            .iter()
            .map(|(k, v)| Ok((k.to_owned(), v.try_into()?)))
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

impl<K: std::hash::Hash + Eq + std::fmt::Debug, S, T: Traverse<S>> Traverse<S> for HashMap<K, T> {
    fn traverse<F, E>(self, f: &mut F, order: TraverseOrder) -> Result<Self, E>
    where
        F: FnMut(S) -> Result<S, E>,
    {
        self.into_iter()
            .map(|(k, v)| Ok((dbg!(k), v.traverse(f, order)?)))
            .collect()
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

pub fn resolve_references(value: &Value, schema: Schema) -> (Schema, HashMap<String, Schema>) {
    let mut refs = HashMap::new();
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

#[cfg(test)]
mod tests {
    use crate::intermediate::resolve_references;

    #[test]
    fn hack() {
        // let data =
        //     std::fs::read_to_string("examples/github-workflow/github-workflow.json").unwrap();
        let data = std::fs::read_to_string("examples/simple-schema/test.schema.json").unwrap();
        let val: serde_json::Value = serde_json::from_str(&data).unwrap();
        let schema: super::Schema = (&val).try_into().unwrap();
        dbg!(&schema);
        let (_, refs) = resolve_references(&val, schema);
        dbg!(refs);
    }
}
