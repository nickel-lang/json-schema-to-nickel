//! This module implements some transforms of the JSON schema that preserve semantics
//! but make the schema easier to analyze.
//!
//! It turns out that `schemars`'s representation of schemas is pretty clunky for this
//! purpose. Probably we want a better intermediate representation.

use std::collections::HashSet;

use schemars::schema::{
    ArrayValidation, ObjectValidation, RootSchema, Schema, SchemaObject, SingleOrVec,
    SubschemaValidation,
};

use crate::{references, utils::plain_schema_types};

/// Some JSON schemas factor out common parts of schemas into definitions.
/// This causes some difficulty for our analysis because information is represented
/// in multiple different places, so this transformation tries to merge factored-out
/// parts into a single schema.
///
/// As a motivating example, the github-workflow schema factors out
///
/// ```json
///    "eventObject": {
///      "oneOf": [
///        {
///          "type": "object"
///        },
///        {
///          "type": "null"
///        }
///      ],
///      "additionalProperties": true
///    },
/// ```
///
/// as a definition, and then references it in various places. For example,
/// it defines a branch protection rule as
///
/// ```json
/// {
///   "$ref": "#/definitions/eventObject",
///   "properties": {
///     "types": ...
///   }
/// }
/// ```
///
/// instead of (which would be more convenient for our contract-generation
/// analysis):
///
/// ```json
/// {
///   "type": ["object", "null"],
///   "properties": {
///     "types": ...
///   },
///   "additionalProperties": true
/// }
/// ```
///
/// This transformation inlines the "$ref", but it doesn't do the translation
/// from "oneOf" to "type". For that, see [`lift_any_of_types`].
pub fn inline_defs(root_schema: RootSchema) -> RootSchema {
    let mut schema = root_schema.schema.clone();
    schema.post_visit(&mut MergeDefs { root: &root_schema });
    RootSchema {
        definitions: root_schema.definitions,
        meta_schema: root_schema.meta_schema,
        schema,
    }
}

/// The definition-merging of `merge_defs` is fine, but let's suppose we have
///
/// ```json
/// "branch_protection_rule": {
///   "$ref": "#/definitions/eventObject",
///   "properties": { ... }
/// }
/// ```
///
/// which then gets merged with the defs to become
///
/// ```json
/// "branch_protection_rule": {
///   "oneOf": [
///     {
///       "type": "object"
///     },
///     {
///       "type": "null"
///     }
///   ],
///   "additionalProperties": true
///   "properties": { ... }
/// }
/// ```
///
/// This still isn't great, because instead of "oneOf" with types inside, we'd prefer
/// just to have "type" at the top-level. That's what this transformation does.
pub fn lift_any_of_types(root_schema: RootSchema) -> RootSchema {
    let mut schema = root_schema.schema.clone();
    schema.post_visit(&mut LiftAnyOfTypes { root: &root_schema });
    RootSchema {
        definitions: root_schema.definitions,
        meta_schema: root_schema.meta_schema,
        schema,
    }
}

/// A shallow merge operation for schemas.
///
/// JSON schemas have a lot of implicit "and" operations; to a first approximation,
/// all of the validations in a schema are applied independently, and the schema as
/// a whole passes if and only they all pass individually. For example,
/// `{ "minProperties": 1, "type": ["object"] }`
/// is equivalent to
/// `{ "allOf": [ { "minProperties": 1 }, { "type": ["object"] } ] }`
///
/// This trait produces things more like the first representation, by merging
/// schemas together to produce schemas with more fields.
///
/// Exceptions to the "first approximation" include things like "properties"
/// and "additionalProperties", which cannot be validated independently.
trait ShallowMerge: Sized {
    fn shallow_merge(self, other: &Self) -> Option<Self>;
}

impl ShallowMerge for ObjectValidation {
    fn shallow_merge(mut self, other: &Self) -> Option<Self> {
        // Does `ov` have any of the three fields that work together?
        fn has_interactions(ov: &ObjectValidation) -> bool {
            !ov.properties.is_empty()
                || ov.additional_properties.is_some()
                || !ov.pattern_properties.is_empty()
        }

        if no_clash(&self.max_properties, &other.max_properties)
            && no_clash(&self.min_properties, &other.min_properties)
            && no_clash(&self.property_names, &other.property_names)
            && !(has_interactions(&self) && has_interactions(other))
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
            self.properties.extend(other.properties.clone());
            Some(self)
        } else {
            None
        }
    }
}

impl ShallowMerge for ArrayValidation {
    fn shallow_merge(mut self, other: &Self) -> Option<Self> {
        // Does `av` have either of the two fields that work together?
        fn has_interactions(av: &ArrayValidation) -> bool {
            av.items.is_some() || av.additional_items.is_some()
        }

        if !(has_interactions(&self) && has_interactions(other))
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

/// The visitor for implementing [`merge_defs`].
struct MergeDefs<'a> {
    root: &'a RootSchema,
}

impl VisitorMut for MergeDefs<'_> {
    fn visit_object(&mut self, obj: &mut SchemaObject) {
        if let Some(reference) = &obj.reference {
            if let Some(referent) =
                references::parse_ref(reference).and_then(|ptr| ptr.resolve(self.root))
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

/// A "visitor" trait for schemas.
///
/// An implementation of this is really just a fancy callback, as our schema
/// representation doesn't really have variants.
trait VisitorMut {
    fn visit_object(&mut self, _obj: &mut SchemaObject) {}
}

/// A trait for performing post-order traversal of the schema tree.
trait PostVisit {
    /// Visit the schema tree, invoking the visitor on all children of a node
    /// and then invoking it on the node itself.
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

/// The visitor for implementing [`lift_any_of_types`].
struct LiftAnyOfTypes<'a> {
    root: &'a RootSchema,
}

impl VisitorMut for LiftAnyOfTypes<'_> {
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

                    let Some(plain_types) = plain_types
                        .into_iter()
                        .map(|tys| match tys {
                            SingleOrVec::Single(t) => Some(*t),
                            SingleOrVec::Vec(_) => None,
                        })
                        .collect::<Option<Vec<_>>>()
                    else {
                        return;
                    };

                    // If there's a repeated type in a `oneOf` array then that type
                    // is technically disallowed. (Although most likely it's a mistake.)
                    let mut unique_types = HashSet::new();
                    let is_one_of = subschemas.one_of.is_some();
                    for t in plain_types {
                        if unique_types.insert(t) && is_one_of {
                            eprintln!("ignoring duplicated type {t:?} in oneOf");
                            unique_types.remove(&t);
                        }
                    }

                    let plain_types: Vec<_> = unique_types.into_iter().collect();

                    subschemas.any_of = None;
                    subschemas.one_of = None;

                    obj.instance_type = match plain_types.as_slice() {
                        [] => None,
                        [x] => Some(SingleOrVec::Single(Box::new(*x))),
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
