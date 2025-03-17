// TODO;
// - in the github example, why doesn't the {_ | Dyn} in services get removed?
// - simplify types in if/then expressions without an else

use std::{
    cell::RefCell,
    collections::{BTreeMap, BTreeSet, HashSet},
    ops::DerefMut,
};

use miette::miette;
use nickel_lang_core::term::{make, record::RecordData, RichTerm, Term};
use ordered_float::NotNan;
use serde_json::{Map, Value};

use crate::{
    contract::ContractContext,
    object::{Obj, ObjectProperties, Property},
    references::{self, References, SchemaPointer, SchemaPointerElt},
    schema::{Arr, Num, Schema, Str},
    traverse::Traverse,
    typ::{InstanceType, InstanceTypeSet},
    utils::{distinct, sequence},
};

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

        if obj.is_empty() {
            return Ok(Schema::Always);
        }

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

    if let Some(mult) = get_number(obj, "multipleOf")? {
        ret.push(Schema::Number(Num::MultipleOf(
            NotNan::try_from(mult).unwrap(),
        )));
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
        let mut dep_fields = BTreeMap::new();
        let mut dep_schemas = BTreeMap::new();
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
                dep_fields.insert(field.to_owned(), names);
            } else {
                dep_schemas.insert(field.to_owned(), val.try_into()?);
            }
        }

        if !dep_fields.is_empty() {
            ret.push(Schema::Object(Obj::DependentFields(dep_fields)));
        }
        if !dep_schemas.is_empty() {
            ret.push(Schema::Object(Obj::DependentSchemas(dep_schemas)));
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
            .map(|(k, v)| {
                let schema = Schema::try_from(v)?;
                let mut prop = Property::from(schema);

                if let Some(doc) = v
                    .as_object()
                    .and_then(|obj| obj.get("description"))
                    .and_then(|desc| desc.as_str())
                {
                    prop.doc = Some(doc.to_owned());
                }
                Ok((k.to_owned(), prop))
            })
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

pub fn resolve_references(value: &Value, schema: Schema) -> (Schema, BTreeMap<String, Schema>) {
    let mut refs = BTreeMap::new();
    let mut record_ref = |schema: Schema| -> Schema {
        if let Schema::Ref(s) = schema {
            if !refs.contains_key(&s) {
                match references::resolve_ptr(&s) {
                    None => {
                        eprintln!("skipping unparseable pointer \"{s}\"");
                    }
                    Some(ptr) => match resolve_ptr(&ptr, value) {
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
            Schema::Ref(s)
        } else {
            schema
        }
    };

    let schema = schema.traverse(&mut record_ref);
    (schema, refs)
}

pub fn resolve_references_recursive(
    value: &Value,
    schema: Schema,
) -> (Schema, BTreeMap<String, Schema>) {
    let (schema, mut refs) = resolve_references(value, schema);
    let mut seen_refs: HashSet<_> = refs.keys().cloned().collect();
    let mut unfollowed_refs = seen_refs.clone();

    while !unfollowed_refs.is_empty() {
        let mut next_refs = HashSet::new();
        for name in unfollowed_refs {
            // unwrap: refs is always a superset of unfollowed_refs
            let (_schema, new_refs) = resolve_references(value, refs[&name].clone());

            next_refs.extend(new_refs.keys().cloned());
            refs.extend(new_refs);
        }
        unfollowed_refs = next_refs.difference(&seen_refs).cloned().collect();
        seen_refs.extend(next_refs);
    }

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
    for elt in ptr.path.iter() {
        match elt {
            SchemaPointerElt::Definitions(name) => {
                let Some(obj) = val.as_object() else {
                    miette::bail!("cannot look up definitions in a non-object");
                };
                val = get_object(obj, "definitions")?
                    .ok_or_else(|| miette!("no definitions"))?
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
    fn flatten_one(schema: Schema) -> Schema {
        match schema {
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
                            return Schema::Never;
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
                            return Schema::Always;
                        }
                        Schema::Never => {}
                        e => new_vec.push(e),
                    }
                }
                Schema::AnyOf(new_vec)
            }
            s => s,
        }
    }

    schema.traverse(&mut flatten_one)
}

pub fn one_to_any(schema: Schema, refs: &References) -> Schema {
    fn distinct_types(s: &[Schema], refs: &References) -> bool {
        let simple_types = s
            .iter()
            .map(|s| s.simple_type(refs))
            .collect::<Option<Vec<_>>>();

        matches!(simple_types, Some(tys) if distinct(tys.iter()))
    }

    let mut one_to_any_one = |schema: Schema| -> Schema {
        match schema {
            Schema::OneOf(vec) if distinct_types(&vec, refs) => Schema::AnyOf(vec),
            s => s,
        }
    };

    schema.traverse(&mut one_to_any_one)
}

pub fn simplify(mut schema: Schema, refs: &References) -> Schema {
    loop {
        let prev = schema.clone();
        schema = flatten_logical_ops(schema);
        schema = one_to_any(schema, refs);
        schema = intersect_types(schema, refs);
        schema = merge_required_properties(schema);
        schema = enumerate_regex_properties(schema, 8);

        if schema == prev {
            return schema;
        }
    }
}

pub fn inline_refs(schema: Schema, refs: &References) -> Schema {
    let mut inline_one = |schema: Schema| -> Schema {
        match schema {
            Schema::Ref(s) => match refs.get(&s).as_deref() {
                // The ref-inlining heuristic could use some work. We probably
                // want to avoid inlining large schemas, and we probably want to
                // prioritize inlining things that can lead to further simplication.
                Some(resolved @ Schema::AnyOf(tys))
                    if tys.iter().all(|ty| ty.just_type().is_some()) =>
                {
                    resolved.clone()
                }
                Some(_) => Schema::Ref(s.clone()),
                None => Schema::Always,
            },
            s => s,
        }
    };

    schema.traverse(&mut inline_one)
}

pub fn merge_required_properties(schema: Schema) -> Schema {
    let mut merge_one = |schema: Schema| -> Schema {
        match schema {
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
        }
    };

    schema.traverse(&mut merge_one)
}

pub fn intersect_types(schema: Schema, refs: &References) -> Schema {
    let mut intersect_one = |schema: Schema| -> Schema {
        match schema {
            Schema::AllOf(mut vec) => {
                // The set of allowed types is the intersection, over all elements of `vec`,
                // of that schema's set of allowed types.
                let mut allowed_types = InstanceTypeSet::FULL;

                for s in &vec {
                    allowed_types = allowed_types.intersect(s.allowed_types(refs));
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
                    .any(|s| s.allowed_types_shallow(refs) == allowed_types)
                {
                    vec.push(allowed_types.to_schema());
                }

                Schema::AllOf(vec)
            }
            s => s,
        }
    };

    schema.traverse(&mut intersect_one)
}

fn enumerate_regex(s: &str, max_expansion: usize) -> Option<Vec<String>> {
    use regex_syntax::hir::{Hir, HirKind, Look};
    // TODO: maybe we should signal an error (probably while constructing the schema) if
    // there's a regex we can't parse?
    let hir = regex_syntax::parse(s).ok()?.into_kind();

    // We're only interested in anchored regexes (starting with ^, ending with $), so
    // check that that's the case. Then strip the anchors in preparation for recursion.
    let HirKind::Concat(mut elems) = hir else {
        return None;
    };
    let Some(HirKind::Look(Look::Start)) = elems.first().map(|h| h.kind()) else {
        return None;
    };

    let Some(HirKind::Look(Look::End)) = elems.last().map(|h| h.kind()) else {
        return None;
    };
    elems.remove(0);
    elems.pop();
    let hir = Hir::concat(elems);

    fn enumerate_rec(hir: regex_syntax::hir::Hir, max_expansion: usize) -> Option<Vec<String>> {
        match hir.into_kind() {
            HirKind::Empty => Some(vec![String::new()]),
            HirKind::Literal(literal) => Some(vec![String::from_utf8(literal.0.to_vec()).ok()?]),
            HirKind::Class(_) | HirKind::Look(_) => None,
            HirKind::Repetition(repetition) => {
                let max = repetition.max? as usize;
                let min = repetition.min as usize;
                let inner = enumerate_rec(*repetition.sub, max_expansion)?;
                if inner.len().checked_mul(max - min)? <= max_expansion {
                    let mut ret = Vec::new();
                    for i in min..=max {
                        for s in &inner {
                            ret.push(s.repeat(i));
                        }
                    }
                    Some(ret)
                } else {
                    None
                }
            }
            HirKind::Capture(capture) => enumerate_rec(*capture.sub, max_expansion),
            HirKind::Concat(vec) => {
                let mut options = vec![String::new()];
                for sub in vec {
                    let sub_options = enumerate_rec(sub, max_expansion)?;
                    if sub_options.len() * options.len() > max_expansion {
                        return None;
                    }

                    options = options
                        .iter()
                        .flat_map(|prev| {
                            sub_options.iter().map(|next| {
                                let mut new = prev.to_owned();
                                new.push_str(next);
                                new
                            })
                        })
                        .collect();
                }

                Some(options)
            }
            HirKind::Alternation(vec) => {
                let mut options = Vec::new();
                for sub in vec {
                    options.extend(enumerate_rec(sub, max_expansion - options.len())?);
                    if options.len() > max_expansion {
                        return None;
                    }
                }
                Some(options)
            }
        }
    }

    let mut ret = enumerate_rec(hir, max_expansion)?;
    ret.sort();
    Some(ret)
}

pub fn enumerate_regex_properties(schema: Schema, max_expansion: usize) -> Schema {
    fn try_expand_props(
        props: &ObjectProperties,
        max_expansion: usize,
    ) -> Option<ObjectProperties> {
        let mut new_props = ObjectProperties {
            properties: props.properties.clone(),
            pattern_properties: BTreeMap::new(),
            additional_properties: props.additional_properties.clone(),
        };

        for (s, schema) in &props.pattern_properties {
            let expanded = enumerate_regex(s, max_expansion)?;
            for name in expanded {
                if new_props
                    .properties
                    .insert(name, schema.clone().into())
                    .is_some()
                {
                    // Abort if there's any overlap between properties (either between
                    // existing properties and regex properties, or between multiple
                    // regex properties). In principle, I think it's also ok to combine
                    // overlaps using allOf.
                    return None;
                }
            }
        }

        Some(new_props)
    }

    let mut enumerate_one = |schema: Schema| -> Schema {
        match schema {
            Schema::Object(Obj::Properties(props)) => Schema::Object(Obj::Properties(
                try_expand_props(&props, max_expansion).unwrap_or(props),
            )),
            s => s,
        }
    };

    schema.traverse(&mut enumerate_one)
}

fn all_shadowed_names(s: Schema) -> (Schema, HashSet<String>) {
    let mut ret = HashSet::new();
    let mut shadowed = |s: Schema| -> Schema {
        if let Schema::Object(Obj::Properties(props)) = &s {
            ret.extend(props.properties.keys().cloned());
        }
        s
    };

    let s = s.traverse(&mut shadowed);
    (s, ret)
}

fn no_collisions_name(taken_names: &HashSet<String>, prefix: &str) -> String {
    let mut ret = prefix.to_owned();
    while taken_names.contains(&ret) {
        ret.push('_');
    }
    ret
}

fn all_ref_names(s: Schema) -> (Schema, HashSet<String>) {
    let mut ret = HashSet::new();
    let mut ref_name = |s: Schema| -> Schema {
        if let Schema::Ref(s) = &s {
            ret.insert(s.clone());
        }
        s
    };

    let s = s.traverse(&mut ref_name);
    (s, ret)
}

pub fn to_nickel(s: Schema, refs: &References, import_term: RichTerm) -> RichTerm {
    let (s, mut all_refs) = all_ref_names(s);

    let mut unfollowed_refs = all_refs.clone();
    let (s, mut shadowed_names) = all_shadowed_names(s);

    while !unfollowed_refs.is_empty() {
        let mut next_refs = HashSet::new();
        for name in unfollowed_refs {
            if let Some(schema) = refs.get(&name) {
                let (schema, new_refs) = all_ref_names(schema.clone());
                let (_, new_shadowed_names) = all_shadowed_names(schema);
                next_refs.extend(new_refs);
                shadowed_names.extend(new_shadowed_names);
            }
        }

        unfollowed_refs = next_refs.difference(&all_refs).cloned().collect();
        all_refs.extend(next_refs);
    }

    let refs_name = no_collisions_name(&shadowed_names, "refs");
    let lib_name = no_collisions_name(&shadowed_names, "js2n");

    let always_eager = refs
        .iter()
        .map(|(name, schema)| (name, schema.is_always_eager(refs)))
        .collect();
    let accessed_refs = RefCell::new(BTreeSet::new());

    let ctx = ContractContext {
        refs,
        lib_name: &lib_name,
        refs_name: &refs_name,
        eager: false,
        always_eager_refs: &always_eager,
        accessed_refs: &accessed_refs,
    };
    let main_contract = s.to_contract(ctx);

    let mut accessed = std::mem::take(ctx.accessed_refs.borrow_mut().deref_mut());
    let mut refs_env = BTreeMap::new();
    let mut unfollowed_refs = accessed.clone();
    while !unfollowed_refs.is_empty() {
        for (name, eager) in unfollowed_refs {
            // FIXME: once we convert to the new ast, make this an actual nested access
            let names: Vec<_> = ctx.ref_name(&name, eager).collect();
            // unwrap: the context shouldn't collect missing references
            refs_env.insert(
                names.join("."),
                sequence(refs.get(&name).unwrap().to_contract(ctx)),
            );
        }

        let newly_accessed = std::mem::take(ctx.accessed_refs.borrow_mut().deref_mut());
        unfollowed_refs = newly_accessed.difference(&accessed).cloned().collect();
        accessed.extend(newly_accessed);
    }

    let refs_dict = Term::Record(RecordData {
        fields: refs_env
            .into_iter()
            .map(|(name, value)| (name.into(), value.into()))
            .collect(),
        ..Default::default()
    });

    make::let_one_in(
        ctx.lib_name,
        import_term,
        make::let_one_rec_in(ctx.refs_name, refs_dict, sequence(main_contract)),
    )
}

pub fn convert(val: &serde_json::Value, lib_import: RichTerm) -> miette::Result<RichTerm> {
    let schema: Schema = val.try_into()?;
    let (schema, all_refs) = resolve_references_recursive(val, schema);
    let refs = References::new(&all_refs);
    let simple_refs = all_refs
        .iter()
        .map(|(k, v)| (k.clone(), simplify(v.clone(), &refs)))
        .collect();
    let refs = References::new(&simple_refs);
    let schema = inline_refs(schema, &refs);
    let schema = simplify(schema, &refs);

    Ok(to_nickel(schema, &refs, lib_import))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn regex_expansion() {
        assert_eq!(
            enumerate_regex("^(foo|bar)$", 2),
            Some(vec!["bar".to_owned(), "foo".to_owned()])
        );

        assert_eq!(enumerate_regex("^(foo|bar)$", 1), None);

        assert_eq!(
            enumerate_regex("^(foo|bar)s?$", 4),
            Some(vec![
                "bar".to_owned(),
                "bars".to_owned(),
                "foo".to_owned(),
                "foos".to_owned()
            ])
        );
    }
}
