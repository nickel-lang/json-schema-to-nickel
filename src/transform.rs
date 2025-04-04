//! Schema transforms
//!
//! When we first create our intermediate representation from a JSON Schema, it
//! can be verbose and redundant. The transforms in this module preserve the
//! semantics of the IR but simplify it.

// TODO;
// - in the github example, why doesn't the {_ | Dyn} in services get removed?
// - simplify types in if/then expressions without an else

use std::collections::{BTreeMap, BTreeSet, HashSet};

use nickel_lang_core::term::{make, record::RecordData, RichTerm, Term};

use crate::{
    contract::ContractContextData,
    object::{Obj, ObjectProperties},
    references::{resolve_all, AcyclicReferences},
    schema::Schema,
    traverse::Traverse,
    typ::InstanceTypeSet,
    utils::{distinct, sequence},
};

/// Merges nested `AllOf`s and `AnyOf`s.
///
/// For example, `AllOf(a, b, AllOf(c, d))` becomes `AllOf(a, b, c, d)`.
///
/// This transform also simplifies empty and singleton `AllOf`s and `AnyOf`s.
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

/// Attempts to convert `OneOf` to `AnyOf`, by detecting whether
/// the alternatives are mutually exclusive.
pub fn one_to_any(schema: Schema, refs: &AcyclicReferences) -> Schema {
    fn distinct_types(s: &[Schema], refs: &AcyclicReferences) -> bool {
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

/// Simplifies a schema by repeatedly applying transformations until we hit a
/// fixed point.
pub fn simplify(mut schema: Schema, refs: &AcyclicReferences) -> Schema {
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

/// Inlines some references to the schemas that reference them.
///
/// As a motivating example from the github-workflow schema, they
/// factor out some type definitions in "eventObject": the definition
/// looks like
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
/// and gets used like
///
/// ```json
///    "branch_protection_rule": {
///      "$ref": "#/definitions/eventObject",
///      "properties": {
///        // ...
///      }
///    },
/// ```
///
/// In particular, the schema using the ref doesn't have its own "type"
/// annotation, which is annoying for our analysis. Inlining the definition
/// fixes this annoyance.
pub fn inline_refs(schema: Schema, refs: &AcyclicReferences) -> Schema {
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

/// Attempts to merge "required" and "properties" schemas.
///
/// In JSON Schema, "required" and "properties" are two orthogonal checks,
/// but in Nickel we like to have optionality annotations on the properties.
/// When this transform encounters
///
/// ```text
/// AllOf([
///   Required(["foo"]),
///   Properties({
///     foo -> ...
///     bar -> ...
///   })
/// ])
/// ```
///
/// it removes the "Required" and marks the "foo" in "Properties"
/// as being non-optional.
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

/// When encountering an `AllOf` in which different elements have different
/// sets of allowed types, propagates the intersection of the type sets to all
/// elements.
pub fn intersect_types(schema: Schema, refs: &AcyclicReferences) -> Schema {
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

/// If a "patternProperties" has a regex that only matches a small set of strings, turns
/// it into a "properties" schema that enumerates the strings.
///
/// Some real-world schemas use "patternProperties" just to save some repetition; for example,
/// `github-workflow.json` has
///
/// ```json
///   "patternProperties": {
///     "^(branche|tag|path)s(-ignore)?$": {
///       "type": "array"
///     }
///   },
/// ```
///
/// We'd prefer to list out all the properties here, because then we can make a proper record
/// contract.
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

fn all_shadowed_names(s: &Schema) -> HashSet<String> {
    let mut ret = HashSet::new();
    let mut shadowed = |s: &Schema| {
        if let Schema::Object(Obj::Properties(props)) = s {
            ret.extend(props.properties.keys().cloned());
        }
    };

    s.traverse_ref(&mut shadowed);
    ret
}

fn no_collisions_name(taken_names: &HashSet<String>, prefix: &str) -> String {
    let mut ret = prefix.to_owned();
    while taken_names.contains(&ret) {
        ret.push('_');
    }
    ret
}

fn all_ref_names(s: &Schema) -> HashSet<String> {
    let mut ret = HashSet::new();
    let mut ref_name = |s: &Schema| {
        if let Schema::Ref(s) = s {
            ret.insert(s.clone());
        }
    };

    s.traverse_ref(&mut ref_name);
    ret
}

pub fn to_nickel(s: &Schema, refs: &AcyclicReferences, import_term: RichTerm) -> RichTerm {
    let mut all_refs = all_ref_names(s);

    let mut unfollowed_refs = all_refs.clone();
    let mut shadowed_names = all_shadowed_names(s);

    while !unfollowed_refs.is_empty() {
        let mut next_refs = HashSet::new();
        for name in unfollowed_refs {
            if let Some(schema) = refs.get(&name) {
                let new_refs = all_ref_names(&schema);
                let new_shadowed_names = all_shadowed_names(&schema);
                next_refs.extend(new_refs);
                shadowed_names.extend(new_shadowed_names);
            }
        }

        unfollowed_refs = next_refs.difference(&all_refs).cloned().collect();
        all_refs.extend(next_refs);
    }

    let refs_name = no_collisions_name(&shadowed_names, "refs");
    let lib_name = no_collisions_name(&shadowed_names, "js2n");

    let ctx_data = ContractContextData::new(refs, &lib_name, &refs_name);
    let ctx = ctx_data.ctx();
    let main_contract = s.to_contract(ctx);

    let mut accessed = ctx.take_accessed_refs();
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

        let newly_accessed = ctx.take_accessed_refs();
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
        ctx.lib_name(),
        import_term,
        make::let_one_rec_in(ctx.refs_name(), refs_dict, sequence(main_contract)),
    )
}

pub fn convert(val: &serde_json::Value, lib_import: RichTerm) -> miette::Result<RichTerm> {
    let schema: Schema = val.try_into()?;
    let all_refs = resolve_all(val, &schema);
    let refs = AcyclicReferences::new(&all_refs);
    let simple_refs = all_refs
        .iter()
        .map(|(k, v)| (k.clone(), simplify(v.clone(), &refs)))
        .collect();
    let refs = AcyclicReferences::new(&simple_refs);
    let schema = inline_refs(schema, &refs);
    let schema = simplify(schema, &refs);

    Ok(to_nickel(&schema, &refs, lib_import))
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
