//! By-value and by-reference traversals of the schema tree.

use std::collections::BTreeMap;

use crate::{
    object::{Obj, ObjectProperties, Property},
    schema::{Array, Schema},
};

/// A trait for traversing a generic structure.
///
/// Currently, we only ever have `T = Schema`, and all of the traversals are
/// bottom-up.
pub trait Traverse<T>: Sized {
    /// Traverse by value, contructing a new tree.
    fn traverse<F>(self, f: &mut F) -> Self
    where
        F: FnMut(T) -> T;

    /// Traverse by reference.
    fn traverse_ref<F>(&self, f: &mut F)
    where
        F: FnMut(&T);
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
            Schema::IfThenElse { iph, then, els } => Schema::IfThenElse {
                iph: iph.traverse(f),
                then: then.traverse(f),
                els: els.traverse(f),
            },
            Schema::Not(schema) => Schema::Not(schema.traverse(f)),
        };

        f(val)
    }

    fn traverse_ref<F>(&self, f: &mut F)
    where
        F: FnMut(&Schema),
    {
        match self {
            Schema::Always
            | Schema::Never
            | Schema::Null
            | Schema::Boolean
            | Schema::Const(_)
            | Schema::Enum(_)
            | Schema::Number(_)
            | Schema::String(_)
            | Schema::Ref(_) => {}
            Schema::Object(obj) => obj.traverse_ref(f),
            Schema::Array(arr) => arr.traverse_ref(f),
            Schema::AnyOf(vec) => vec.traverse_ref(f),
            Schema::OneOf(vec) => vec.traverse_ref(f),
            Schema::AllOf(vec) => vec.traverse_ref(f),
            Schema::IfThenElse { iph, then, els } => {
                iph.traverse_ref(f);
                then.traverse_ref(f);
                els.traverse_ref(f);
            }
            Schema::Not(schema) => {
                schema.traverse_ref(f);
            }
        }
        f(self)
    }
}

impl<S, T: Traverse<S>> Traverse<S> for Box<T> {
    fn traverse<F>(self, f: &mut F) -> Self
    where
        F: FnMut(S) -> S,
    {
        Box::new((*self).traverse(f))
    }

    fn traverse_ref<F>(&self, f: &mut F)
    where
        F: FnMut(&S),
    {
        (**self).traverse_ref(f);
    }
}

impl<S, T: Traverse<S>> Traverse<S> for Option<T> {
    fn traverse<F>(self, f: &mut F) -> Self
    where
        F: FnMut(S) -> S,
    {
        self.map(|v| v.traverse(f))
    }

    fn traverse_ref<F>(&self, f: &mut F)
    where
        F: FnMut(&S),
    {
        if let Some(slf) = self.as_ref() {
            slf.traverse_ref(f);
        }
    }
}

impl<S, T: Traverse<S>> Traverse<S> for Vec<T> {
    fn traverse<F>(self, f: &mut F) -> Self
    where
        F: FnMut(S) -> S,
    {
        self.into_iter().map(|x| x.traverse(f)).collect()
    }

    fn traverse_ref<F>(&self, f: &mut F)
    where
        F: FnMut(&S),
    {
        for s in self {
            s.traverse_ref(f);
        }
    }
}

impl<K: Ord + Eq, S, T: Traverse<S>> Traverse<S> for BTreeMap<K, T> {
    fn traverse<F>(self, f: &mut F) -> Self
    where
        F: FnMut(S) -> S,
    {
        self.into_iter().map(|(k, v)| (k, v.traverse(f))).collect()
    }

    fn traverse_ref<F>(&self, f: &mut F)
    where
        F: FnMut(&S),
    {
        for s in self.values() {
            s.traverse_ref(f);
        }
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

    fn traverse_ref<F>(&self, f: &mut F)
    where
        F: FnMut(&Schema),
    {
        self.schema.traverse_ref(f);
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

    fn traverse_ref<F>(&self, f: &mut F)
    where
        F: FnMut(&Schema),
    {
        match self {
            Obj::Any
            | Obj::MaxProperties(_)
            | Obj::MinProperties(_)
            | Obj::Required(_)
            | Obj::DependentFields(_) => {}
            Obj::PropertyNames(schema) => schema.traverse_ref(f),
            Obj::DependentSchemas(deps) => deps.traverse_ref(f),
            Obj::Properties(props) => {
                props.properties.traverse_ref(f);
                props.pattern_properties.traverse_ref(f);
                props.additional_properties.traverse_ref(f);
            }
        }
    }
}

impl Traverse<Schema> for Array {
    fn traverse<F>(self, f: &mut F) -> Self
    where
        F: FnMut(Schema) -> Schema,
    {
        match self {
            Array::Any | Array::MaxItems(_) | Array::MinItems(_) | Array::UniqueItems => self,
            Array::AllItems(schema) => Array::AllItems(schema.traverse(f)),
            Array::PerItem { initial, rest } => Array::PerItem {
                initial: initial.traverse(f),
                rest: rest.traverse(f),
            },
            Array::Contains(schema) => Array::Contains(schema.traverse(f)),
        }
    }

    fn traverse_ref<F>(&self, f: &mut F)
    where
        F: FnMut(&Schema),
    {
        match self {
            Array::Any | Array::MaxItems(_) | Array::MinItems(_) | Array::UniqueItems => {}
            Array::AllItems(schema) => schema.traverse_ref(f),
            Array::PerItem { initial, rest } => {
                initial.traverse_ref(f);
                rest.traverse_ref(f);
            }
            Array::Contains(schema) => schema.traverse_ref(f),
        }
    }
}
