use schemars::schema::{
    ArrayValidation, ObjectValidation, RootSchema, Schema, SchemaObject, SingleOrVec,
    SubschemaValidation,
};

use crate::{references, utils::plain_schema_types};

// Some JSON schemas factor out common parts of schemas into definitions.
// This causes some difficulty for our analysis because information is represented
// in multiple different places, so this transformation tries to merge factored-out
// parts into a single schema.
//
// As a motivating example, the github-workflow schema factors out
//
// ```json
//    "eventObject": {
//      "oneOf": [
//        {
//          "type": "object"
//        },
//        {
//          "type": "null"
//        }
//      ],
//      "additionalProperties": true
//    },
// ```
//
// as a definition, and then references it in various places that might instead
// contain
//
// ```json
// "type": ["object", "null"],
// "additionalProperties": true
// ```
pub fn merge_defs(root_schema: RootSchema) -> RootSchema {
    let mut schema = root_schema.schema.clone();
    schema.post_visit(&mut MergeDefs { root: &root_schema });
    RootSchema {
        definitions: root_schema.definitions,
        meta_schema: root_schema.meta_schema,
        schema,
    }
}

// TODO: docme
pub fn lift_any_of_types(root_schema: RootSchema) -> RootSchema {
    let mut schema = root_schema.schema.clone();
    schema.post_visit(&mut UnionType { root: &root_schema });
    RootSchema {
        definitions: root_schema.definitions,
        meta_schema: root_schema.meta_schema,
        schema,
    }
}

trait ShallowMerge: Sized {
    fn shallow_merge(self, other: &Self) -> Option<Self>;
}

impl ShallowMerge for ObjectValidation {
    fn shallow_merge(mut self, other: &Self) -> Option<Self> {
        if no_clash(&self.max_properties, &other.max_properties)
            && no_clash(&self.min_properties, &other.min_properties)
            && no_clash(&self.property_names, &other.property_names)
            && no_clash(&self.additional_properties, &other.additional_properties)
            && (self.pattern_properties.is_empty() || other.pattern_properties.is_empty())
        {
            merge_opt(&mut self.max_properties, &other.max_properties);
            merge_opt(&mut self.min_properties, &other.min_properties);
            merge_opt(&mut self.property_names, &other.property_names);
            merge_opt(
                &mut self.additional_properties,
                &other.additional_properties,
            );
            self.pattern_properties
                .extend(other.pattern_properties.clone());
            Some(self)
        } else {
            None
        }
    }
}

impl ShallowMerge for ArrayValidation {
    fn shallow_merge(mut self, other: &Self) -> Option<Self> {
        if no_clash(&self.items, &other.items)
            && no_clash(&self.additional_items, &other.additional_items)
            && no_clash(&self.max_items, &other.max_items)
            && no_clash(&self.min_items, &other.min_items)
            && no_clash(&self.unique_items, &other.unique_items)
            && no_clash(&self.contains, &other.contains)
        {
            merge_opt(&mut self.items, &other.items);
            merge_opt(&mut self.additional_items, &other.additional_items);
            merge_opt(&mut self.max_items, &other.max_items);
            merge_opt(&mut self.min_items, &other.min_items);
            merge_opt(&mut self.unique_items, &other.unique_items);
            merge_opt(&mut self.contains, &other.contains);
            Some(self)
        } else {
            None
        }
    }
}

impl<T: ShallowMerge + Clone> ShallowMerge for Box<T> {
    fn shallow_merge(self, other: &Self) -> Option<Self> {
        (*self).shallow_merge(other).map(Box::new)
    }
}

impl<T: ShallowMerge + Clone> ShallowMerge for Option<T> {
    fn shallow_merge(self, other: &Self) -> Option<Self> {
        match (self, other) {
            (x, None) => Some(x),
            (None, x) => Some(x.clone()),
            (Some(x), Some(y)) => x.shallow_merge(y).map(Some),
        }
    }
}

fn no_clash<T: PartialEq>(x: &Option<T>, y: &Option<T>) -> bool {
    x.is_none() || y.is_none() || x == y
}

fn merge_opt<T: Clone>(x: &mut Option<T>, y: &Option<T>) {
    if x.is_none() {
        *x = y.clone();
    }
}

struct MergeDefs<'a> {
    root: &'a RootSchema,
}

impl VisitorMut for MergeDefs<'_> {
    fn visit_object(&mut self, obj: &mut SchemaObject) {
        if let Some(reference) = &obj.reference {
            if let Some(referent) =
                references::resolve_ptr(reference).and_then(|ptr| ptr.resolve(self.root))
            {
                match &*referent {
                    Schema::Bool(_) => {}
                    Schema::Object(other) => {
                        let can_merge = no_clash(&obj.instance_type, &other.instance_type)
                            && no_clash(&obj.format, &other.format)
                            && no_clash(&obj.subschemas, &other.subschemas)
                            && no_clash(&obj.number, &other.number)
                            && no_clash(&obj.string, &other.string);

                        let merged_obj = obj.object.clone().shallow_merge(&other.object);
                        let merged_arr = obj.array.clone().shallow_merge(&other.array);

                        if let (Some(merged_obj), Some(merged_arr), true) =
                            (merged_obj, merged_arr, can_merge)
                        {
                            merge_opt(&mut obj.instance_type, &other.instance_type);
                            merge_opt(&mut obj.format, &other.format);
                            merge_opt(&mut obj.subschemas, &other.subschemas);
                            merge_opt(&mut obj.number, &other.number);
                            merge_opt(&mut obj.string, &other.string);
                            obj.object = merged_obj;
                            obj.array = merged_arr;

                            // We've already resolved our reference, so just take the other one unconditionally.
                            obj.reference = other.reference.clone();

                            // At this point, we could try to follow the other
                            // reference too. That might introduce loops though,
                            // so let's not.
                        }
                    }
                }
            } else {
                obj.reference.take();
            }
        };
    }
}

trait VisitorMut {
    fn visit_object(&mut self, _obj: &mut SchemaObject) {}
}

trait PostVisit {
    fn post_visit<V: VisitorMut>(&mut self, visitor: &mut V);
}

impl PostVisit for Schema {
    fn post_visit<V: VisitorMut>(&mut self, visitor: &mut V) {
        if let Schema::Object(obj) = self {
            obj.post_visit(visitor);
        }
    }
}

impl PostVisit for SchemaObject {
    fn post_visit<V: VisitorMut>(&mut self, visitor: &mut V) {
        if let Some(subschemas) = &mut self.subschemas {
            subschemas.post_visit(visitor);
        }
        if let Some(array) = &mut self.array {
            array.post_visit(visitor);
        }
        if let Some(object) = &mut self.object {
            object.post_visit(visitor);
        }

        visitor.visit_object(self);
    }
}

impl PostVisit for SubschemaValidation {
    fn post_visit<V: VisitorMut>(&mut self, visitor: &mut V) {
        let children = [&mut self.all_of, &mut self.any_of, &mut self.one_of]
            .into_iter()
            .filter_map(|x| x.as_mut())
            .flat_map(|schemas| schemas.iter_mut())
            .chain(self.not.as_deref_mut())
            .chain(self.if_schema.as_deref_mut())
            .chain(self.then_schema.as_deref_mut())
            .chain(self.else_schema.as_deref_mut());

        for child in children {
            child.post_visit(visitor);
        }
    }
}
impl PostVisit for ArrayValidation {
    fn post_visit<V: VisitorMut>(&mut self, visitor: &mut V) {
        // SingleOrVec doesn't give a way to get a mutable iterator, so this was
        // the easiest way I found to iterate over it.
        if let Some(items) = &mut self.items {
            match items {
                schemars::schema::SingleOrVec::Single(x) => x.post_visit(visitor),
                schemars::schema::SingleOrVec::Vec(vec) => {
                    for x in vec {
                        x.post_visit(visitor)
                    }
                }
            }
        }
        let children = self.additional_items.iter_mut().chain(&mut self.contains);
        for child in children {
            child.post_visit(visitor);
        }
    }
}

impl PostVisit for ObjectValidation {
    fn post_visit<V: VisitorMut>(&mut self, visitor: &mut V) {
        let children = self
            .properties
            .values_mut()
            .chain(self.pattern_properties.values_mut())
            .chain(self.additional_properties.as_deref_mut())
            .chain(self.property_names.as_deref_mut());

        for child in children {
            child.post_visit(visitor);
        }
    }
}

struct UnionType<'a> {
    root: &'a RootSchema,
}

impl VisitorMut for UnionType<'_> {
    fn visit_object(&mut self, obj: &mut SchemaObject) {
        if obj.instance_type.is_some() {
            return;
        }

        if let Some(subschemas) = &mut obj.subschemas {
            match (&subschemas.any_of, &subschemas.one_of) {
                (None, Some(any_of)) | (Some(any_of), None) => {
                    let Some(plain_types) = any_of
                        .iter()
                        .map(|s| plain_schema_types(s, self.root))
                        .collect::<Option<Vec<_>>>()
                    else {
                        return;
                    };

                    let Some(mut plain_types) = plain_types
                        .into_iter()
                        .map(|tys| match tys {
                            SingleOrVec::Single(t) => Some(*t),
                            SingleOrVec::Vec(_) => None,
                        })
                        .collect::<Option<Vec<_>>>()
                    else {
                        return;
                    };

                    // Technically, if this is `one_of` then we should filter out any repeated types.
                    plain_types.sort();
                    plain_types.dedup();

                    subschemas.any_of = None;
                    subschemas.one_of = None;

                    obj.instance_type = match plain_types.len() {
                        0 => None,
                        1 => Some(SingleOrVec::Single(Box::new(plain_types[0]))),
                        _ => Some(SingleOrVec::Vec(plain_types)),
                    };
                }
                _ => {}
            }

            // We may have made the subschema validation trivial, in which case remove it.
            // (This could be a separate pass.)
            if matches!(
                **subschemas,
                SubschemaValidation {
                    all_of: None,
                    any_of: None,
                    one_of: None,
                    not: None,
                    if_schema: None,
                    then_schema: None,
                    else_schema: None
                }
            ) {
                obj.subschemas = None;
            }
        }
    }
}
