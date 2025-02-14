use std::collections::HashSet;

use nickel_lang_core::{
    identifier::LocIdent,
    term::{make, RichTerm},
};
use schemars::schema::{InstanceType, RootSchema, Schema, SchemaObject, SingleOrVec};

use crate::references;

pub fn static_access<I, S>(record: S, fields: I) -> RichTerm
where
    I: IntoIterator<Item = S>,
    I::IntoIter: DoubleEndedIterator,
    S: Into<LocIdent>,
{
    make::static_access(make::var(record), fields)
}

/// Replace special escaping sequences by the actual character within one element of a JSON pointer
/// path. See the [JSON pointer syntax](https://datatracker.ietf.org/doc/html/rfc6901#section-3).
/// Currently, this just amounts to replace `~0` by `~` and `~1` by `/`.
pub fn decode_json_ptr_part(part: &str) -> String {
    part.replace("~0", "~").replace("~1", "/")
}

pub fn distinct<T: std::hash::Hash + Eq>(items: impl Iterator<Item = T>) -> bool {
    let mut seen = HashSet::new();
    for item in items {
        if !seen.insert(item) {
            return false;
        }
    }
    true
}

pub fn schema_types(s: &Schema, root_schema: &RootSchema) -> Option<SingleOrVec<InstanceType>> {
    match s {
        Schema::Bool(_) => None,
        Schema::Object(SchemaObject {
            instance_type: Some(instance_type),
            reference: None,
            ..
        }) => Some(instance_type.clone()),
        Schema::Object(SchemaObject {
            instance_type: None,
            reference: Some(reference),
            ..
        }) => {
            let ptr = references::resolve_ptr(reference)?;
            let s = ptr.resolve(root_schema)?;
            schema_types(&s, root_schema)
        }
        _ => None,
    }
}

pub fn plain_schema_types(
    s: &Schema,
    root_schema: &RootSchema,
) -> Option<SingleOrVec<InstanceType>> {
    match s {
        Schema::Bool(_) => None,
        Schema::Object(SchemaObject {
            instance_type: Some(instance_type),
            reference: None,
            format: None,
            enum_values: None,
            const_value: None,
            subschemas: None,
            number: None,
            string: None,
            array: None,
            object: None,
            metadata: _,
            extensions: _,
        }) => Some(instance_type.clone()),
        Schema::Object(SchemaObject {
            instance_type: None,
            reference: Some(reference),
            format: None,
            enum_values: None,
            const_value: None,
            subschemas: None,
            number: None,
            string: None,
            array: None,
            object: None,
            metadata: _,
            extensions: _,
        }) => {
            let ptr = references::resolve_ptr(reference)?;
            let s = ptr.resolve(root_schema)?;
            schema_types(&s, root_schema)
        }
        _ => None,
    }
}
