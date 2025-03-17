use std::collections::BTreeMap;

use crate::{
    object::{Obj, ObjectProperties, Property},
    schema::{Arr, Schema},
};

pub trait Traverse<T>: Sized {
    fn traverse<F>(self, f: &mut F) -> Self
    where
        F: FnMut(T) -> T;
}

impl Traverse<Schema> for Schema {
    fn traverse<F>(self, f: &mut F) -> Self
    where
        F: FnMut(Schema) -> Schema,
    {
        let val = match self {
            Schema::Always
            | Schema::Never
            | Schema::Null
            | Schema::Boolean
            | Schema::Const(_)
            | Schema::Enum(_)
            | Schema::Number(_)
            | Schema::String(_)
            | Schema::Ref(_) => self,
            Schema::Object(obj) => Schema::Object(obj.traverse(f)),
            Schema::Array(arr) => Schema::Array(arr.traverse(f)),
            Schema::AnyOf(vec) => Schema::AnyOf(vec.traverse(f)),
            Schema::OneOf(vec) => Schema::OneOf(vec.traverse(f)),
            Schema::AllOf(vec) => Schema::AllOf(vec.traverse(f)),
            Schema::Ite { iph, then, els } => Schema::Ite {
                iph: iph.traverse(f),
                then: then.traverse(f),
                els: els.traverse(f),
            },
            Schema::Not(schema) => Schema::Not(schema.traverse(f)),
        };

        f(val)
    }
}

impl<S, T: Traverse<S>> Traverse<S> for Box<T> {
    fn traverse<F>(self, f: &mut F) -> Self
    where
        F: FnMut(S) -> S,
    {
        Box::new((*self).traverse(f))
    }
}

impl<S, T: Traverse<S>> Traverse<S> for Option<T> {
    fn traverse<F>(self, f: &mut F) -> Self
    where
        F: FnMut(S) -> S,
    {
        self.map(|v| v.traverse(f))
    }
}

impl<S, T: Traverse<S>> Traverse<S> for Vec<T> {
    fn traverse<F>(self, f: &mut F) -> Self
    where
        F: FnMut(S) -> S,
    {
        self.into_iter().map(|x| x.traverse(f)).collect()
    }
}

impl<K: Ord + Eq, S, T: Traverse<S>> Traverse<S> for BTreeMap<K, T> {
    fn traverse<F>(self, f: &mut F) -> Self
    where
        F: FnMut(S) -> S,
    {
        self.into_iter().map(|(k, v)| (k, v.traverse(f))).collect()
    }
}

impl Traverse<Schema> for Property {
    fn traverse<F>(self, f: &mut F) -> Self
    where
        F: FnMut(Schema) -> Schema,
    {
        Property {
            doc: self.doc,
            optional: self.optional,
            schema: self.schema.traverse(f),
        }
    }
}

impl Traverse<Schema> for Obj {
    fn traverse<F>(self, f: &mut F) -> Self
    where
        F: FnMut(Schema) -> Schema,
    {
        match self {
            Obj::Any
            | Obj::MaxProperties(_)
            | Obj::MinProperties(_)
            | Obj::Required(_)
            | Obj::DependentFields(_) => self,
            Obj::PropertyNames(schema) => Obj::PropertyNames(schema.traverse(f)),
            Obj::DependentSchemas(deps) => Obj::DependentSchemas(deps.traverse(f)),
            Obj::Properties(props) => Obj::Properties(ObjectProperties {
                properties: props.properties.traverse(f),
                pattern_properties: props.pattern_properties.traverse(f),
                additional_properties: props.additional_properties.traverse(f),
            }),
        }
    }
}

impl Traverse<Schema> for Arr {
    fn traverse<F>(self, f: &mut F) -> Self
    where
        F: FnMut(Schema) -> Schema,
    {
        match self {
            Arr::Any | Arr::MaxItems(_) | Arr::MinItems(_) | Arr::UniqueItems => self,
            Arr::AllItems(schema) => Arr::AllItems(schema.traverse(f)),
            Arr::PerItem { initial, rest } => Arr::PerItem {
                initial: initial.traverse(f),
                rest: rest.traverse(f),
            },
            Arr::Contains(schema) => Arr::Contains(schema.traverse(f)),
        }
    }
}
