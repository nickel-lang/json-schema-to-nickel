//! Reference handling for JSON schema
//!
//! JSON schemas can reference other schemas at essentially arbitrary points through the special
//! [`$ref`](https://json-schema.org/draft/2020-12/json-schema-core#name-direct-references-with-ref)
//! attribute. Those references are very general: they are URI, which can point to a remote
//! resource of the network. The fragment of the URI is a [JSON
//! pointer](https://discord.com/channels/1174731094726295632/1179430745727586407/1225453786529660938),
//! which is a path within the JSON schema to a specific attribute (note that it's not JSON-schema
//! specific, but rather a general mechanism to index into a JSON value).
//!
//! We don't want to support the general case, at least for now, as it comes with its lot of
//! complexity. However, we want to at least be capable of resolving all local references (i.e.
//! reference to the current file).
//!
//! There are two different kinds of references:
//!
//! - references to top-level definitions in a schema. JSON schemas can contain a set of
//! definitions at the top level and reference them from other parts of the schema.
//! - references to other properties of the schema.
//!
//! In both cases, in order to resolve references, we might need either the contract or the
//! predicate version of the converted schemas (both the predicate and the contract) for each
//! definition and property, as we don't know yet if their usage will require the predicate form or
//! the contract form. During conversion, we simply suppose that they are accessible through
//! special values that are introduced at the top level by json-schema-to-nickel, e.g.
//! `___nickel_defs` or `___nickel_props_preds`. Thus, we can refer to them with a statically known
//! field path. We record along the way which properties or definitions are used, and if they are
//! used as a contract or as a predicate.
//!
//! At the end, we can elaborate the required special values like `___nickel_defs` and only include
//! the actually used in the final contract, to avoid bloating the result.

use crate::{contracts::TryAsContract, predicates::AsPredicate};
use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet};

use nickel_lang_core::{
    identifier::Ident,
    term::{
        record::{Field, FieldMetadata, RecordData},
        LetAttrs, RichTerm, Term,
    },
};
use schemars::schema::{RootSchema, Schema, SchemaObject};

use crate::{
    contracts::{Contract, Documentation},
    predicates::Predicate,
    utils::{decode_json_ptr_part, static_access},
    DEFINITIONS_MANGLED, ENVIRONMENT_MANGLED, PROPS_PREDICATES_MANGLED,
};

/// Specify if a reference is used in a context which requires a contract or a predicate.
#[derive(Clone, Debug, Copy)]
pub enum RefUsage {
    Contract,
    Predicate,
}

/// A representation of a field path in the final generated contract.
///
/// # Invariants
///
/// The path is guaranteed to be non-empty by construction. Do not directly mutate the underlying
/// path with the risk of making it empty.
#[derive(Hash, Clone, Debug, Default)]
pub struct FieldPath {
    path: Vec<String>,
}

pub struct EmptyFieldPath;

impl TryFrom<Vec<String>> for FieldPath {
    type Error = EmptyFieldPath;

    fn try_from(path: Vec<String>) -> Result<Self, Self::Error> {
        if path.is_empty() {
            return Err(EmptyFieldPath);
        }

        Ok(Self { path })
    }
}

impl From<FieldPath> for RichTerm {
    fn from(field_path: FieldPath) -> Self {
        // unwrap(): the `FieldPath` struct guarantees that the path is non-empty by construction.
        static_access(
            field_path.path.first().unwrap(),
            field_path.path.iter().skip(1),
        )
    }
}

/// A representation of JSON pointer, which is mostly a path within a JSON document toward a
/// specific value. See [JSON pointer](https://datatracker.ietf.org/doc/html/rfc6901).
#[derive(Hash, Clone, Debug, Default)]
pub struct JsonPointer {
    pub path: Vec<String>,
}

impl JsonPointer {
    /// Create a new JSON pointer from a string representation (valid according to RFC6901).
    pub fn new(ptr: &str) -> Self {
        Self {
            path: ptr.split('/').map(decode_json_ptr_part).collect(),
        }
    }

    /// Take a JSON pointer to a property and return the corresponding path in the final
    /// generated contract, that is, with all the intermediate `properties` stripped.
    ///
    /// For example, running [Self::try_as_field_path] on a JSON pointer
    /// `/properties/foo/properties/bar` will return the field path `["foo", "bar"]`.
    fn try_as_field_path(&self) -> Option<FieldPath> {
        let mut it = self.path.iter();
        let mut result = Vec::with_capacity(self.path.len() / 2);

        // We expect that the path can be grouped as a sequence of two elements, where the first
        // one is always `properties`, and the second one corresponds to the property name.
        while let Some(part) = it.next() {
            if part != "properties" {
                return None;
            }

            if let Some(name) = it.next() {
                result.push(name.clone());
            } else {
                return None;
            }
        }

        FieldPath::try_from(result).ok()
    }

    /// Tries to interpret `self` as pointing to a top-level definition. A JSON pointer points to a
    /// top-level definition if the path has exactly two elements and the first one is
    /// `definitions`.
    fn try_as_def(&self) -> Option<String> {
        if self.path.len() == 2 && self.path[0] == "definitions" {
            Some(self.path[1].clone())
        } else {
            None
        }
    }
}

/// The conversion of a JSON schema definition into a Nickel predicate and contract. We don't
/// always use both, so we only store the part which is actually used.
#[derive(Clone)]
pub struct ConvertedDef {
    doc: Option<Documentation>,
    predicate: Option<Predicate>,
    contract: Option<Contract>,
}

impl ConvertedDef {
    /// Take the contract part out of this definition and convert it to a record field with the
    /// appropriate definition. This method returns `None` if `self.contract` is `None`.
    ///
    /// After calling this method, `self.contract` will be `None`.
    pub fn contract_as_field(&mut self) -> Option<Field> {
        Self::as_field(self.contract.take(), self.doc.clone())
    }

    /// Take the predicate part out of this definition and convert it to a record field with the
    /// appropriate definition. This method returns `None` if `self.contract` is `None`.
    ///
    /// After calling this method, `self.predicate` will be `None`.
    pub fn predicate_as_field(&mut self) -> Option<Field> {
        Self::as_field(self.predicate.take(), self.doc.clone())
    }

    /// Helper including the logic common to `contract_as_field` and `predicate_as_field`.
    fn as_field<V>(value: Option<V>, doc: Option<Documentation>) -> Option<Field>
    where
        V: Clone + Into<RichTerm>,
    {
        let value = value?.into();

        Some(Field {
            value: Some(value),
            metadata: FieldMetadata {
                doc: doc.map(String::from),
                ..Default::default()
            },
            ..Default::default()
        })
    }
}

/// The conversion of a JSON schema property into a Nickel predicate.
#[derive(Clone)]
pub struct ConvertedProp {
    doc: Option<Documentation>,
    predicate: Predicate,
}

impl From<ConvertedProp> for Field {
    fn from(value: ConvertedProp) -> Self {
        Field {
            value: Some(value.predicate.into()),
            metadata: FieldMetadata {
                doc: value.doc.map(String::from),
                ..Default::default()
            },
            ..Default::default()
        }
    }
}

/// State recording which properties and definitions are actually used and how (as predicates or as
/// contracts).
#[derive(Clone, Default)]
pub struct RefsUsage {
    /// The definitions referenced as predicates somewhere in the schema.
    pub defs_predicates: HashSet<String>,
    /// The definitions referenced as contracts somewhere in the schema.
    pub defs_contracts: HashSet<String>,
    /// The properties referenced as predicates somewhere in the schema (stored as path).
    ///
    /// We don't need to keep track of the contracts, as they will unconditionally be constituents
    /// of the final schema.
    pub props_predicates: HashSet<Vec<String>>,
}

impl RefsUsage {
    /// The empty state
    pub fn new() -> Self {
        Self::default()
    }

    /// Return the set difference between all the definitions (either predicate or contract)
    /// referenced in `self` and all the definitions referenced in `other`.
    ///
    /// That is, [Self::defs_diff] returns `(self.defs_predicates | self.defs_contracts) -
    /// (other.defs_predicates | other.defs_contracts)`.
    pub fn defs_diff(&self, other: &RefsUsage) -> HashSet<String> {
        &(&self.defs_predicates | &self.defs_contracts)
            - &(&other.defs_predicates | &other.defs_contracts)
    }

    /// Extend the usages of `self` with the usages of `other`.
    pub fn extend(&mut self, other: RefsUsage) {
        self.defs_predicates.extend(other.defs_predicates);
        self.defs_contracts.extend(other.defs_contracts);
        self.props_predicates.extend(other.props_predicates);
    }
}

/// An environment of top level schema definitions and nested properties and their conversions into
/// Nickel predicates and contracts.
#[derive(Clone, Default)]
pub struct Environment {
    /// The top-level definition of the schema.
    definitions: HashMap<String, ConvertedDef>,
    /// The predicates of the properties of the schema. We only need to store the predicates, and
    /// not the contracts, as the contracts are simply accessible recursively in the resulting
    /// schema. For example, the contract for the reference `#/properties/foo/properties/bar` is
    /// simply `foo.bar`.
    ///
    /// The key is the path to the property. In our previous example, the key would be `["foo",
    /// "bar"]`.
    property_preds: HashMap<Vec<String>, ConvertedProp>,
}

/// Resolve a JSON schema reference to a Nickel term. The resulting Nickel expression will have a
/// different shape depending on the usage context and the type of reference (definition vs
/// property).
///
/// # Arguments
///
/// - `reference`: the JSON schema reference to resolve. It must be a valid URI
/// - `state`: the state used to record which properties and definitions are actually used, and
///   how. `resolve_ref` will update the state accordingly
/// - `usage`: the context in which the reference is used. Some contexts requires a predicate,
///   while other can do with a contract.
pub fn resolve_ref(reference: &str, state: &mut RefsUsage, usage: RefUsage) -> RichTerm {
    let unsupported_reference = || -> RichTerm {
        eprintln!(
            "
            Warning: skipping reference {reference} (replaced by an always succeeding \
            `Dyn` contract). The current version of `json-schema-to-nickel` only supports \
            internal references to top-level definitions or nested properties"
        );

        match usage {
            RefUsage::Contract => Contract::dynamic().into(),
            RefUsage::Predicate => Predicate::always().into(),
        }
    };

    if let Some(fragment) = reference.strip_prefix("#/") {
        let json_ptr = JsonPointer::new(fragment);

        if let Some(field_path) = json_ptr.try_as_field_path() {
            match usage {
                RefUsage::Contract => RichTerm::from(field_path),
                RefUsage::Predicate => {
                    // If we are referring to a property as a predicate, we need to keep track of it.
                    state.props_predicates.insert(field_path.path.clone());
                    // We don't index the properties element by element, as in
                    // `<PROPS_PREDICATES_MANGLED>.foo.bar.baz`, but we use the whole path with `/`
                    // as a separator as a key. See the documentation of `PROPS_PREDICATES_MANGLED`
                    // for more information.
                    static_access(
                        ENVIRONMENT_MANGLED,
                        [PROPS_PREDICATES_MANGLED, field_path.path.join("/").as_str()],
                    )
                }
            }
        } else if let Some(name) = json_ptr.try_as_def() {
            match usage {
                RefUsage::Contract => {
                    state.defs_contracts.insert(name.clone());
                    static_access(
                        ENVIRONMENT_MANGLED,
                        [DEFINITIONS_MANGLED, "contracts", name.as_ref()],
                    )
                }
                RefUsage::Predicate => {
                    state.defs_predicates.insert(name.clone());
                    static_access(
                        ENVIRONMENT_MANGLED,
                        [DEFINITIONS_MANGLED, "predicates", name.as_ref()],
                    )
                }
            }
        } else {
            unsupported_reference()
        }
    } else {
        unsupported_reference()
    }
}

impl Environment {
    /// The empty environment
    pub fn empty() -> Self {
        Self::default()
    }

    /// Create an environment from the top-level JSON schema and the record usage of refs during
    /// the conversion of this schema to a Nickel contract or predicate.
    ///
    /// Note that we have to repeat the creation process: when converting the referenced
    /// definitions, those definitions might themselves reference other definitions that were not
    /// used until now. We record those usages as well, and iterate until no new definition is ever
    /// referenced.
    pub fn new(root_schema: &RootSchema, mut refs_usage: RefsUsage) -> Self {
        let mut definitions = HashMap::new();
        let mut property_preds = HashMap::new();

        // The stack of definition to process. We might grow this stack as converting some
        // definitions references new ones.
        let mut def_stack: Vec<_> = refs_usage
            .defs_predicates
            .iter()
            .chain(&refs_usage.defs_contracts)
            .cloned()
            .collect();

        while let Some(def) = def_stack.pop() {
            let Some(schema) = root_schema.definitions.get(&def) else {
                eprintln!(
                    "Warning: definition `{def}` is referenced in the schema but couldn't be found"
                );
                continue;
            };

            let mut cur_usage = RefsUsage::new();

            let doc = Documentation::try_from(schema).ok();

            let predicate = refs_usage
                .defs_predicates
                .contains(&def)
                .then(|| schema.as_predicate(&mut cur_usage));

            let contract = refs_usage.defs_contracts.contains(&def).then(|| {
                schema.try_as_contract(&mut cur_usage).unwrap_or_else(|| {
                    Contract::from(
                        predicate
                            .clone()
                            .unwrap_or_else(|| schema.as_predicate(&mut cur_usage)),
                    )
                })
            });

            // Because of the iterative nature of the process, the definition might already be
            // present in `definitions` (for example, if it was referenced as a predicate, and then
            // later as a contract during the definition conversion phase). In this case, we simply
            // merge the entries.
            match definitions.entry(def) {
                Entry::Occupied(mut entry) => {
                    let entry: &mut ConvertedDef = entry.get_mut();
                    entry.contract = entry.contract.take().or(contract);
                    entry.predicate = entry.predicate.take().or(predicate);
                }
                Entry::Vacant(entry) => {
                    entry.insert(ConvertedDef {
                        doc,
                        predicate,
                        contract,
                    });
                }
            }

            // Adding the new usages to the stack
            let new_usages = cur_usage.defs_diff(&refs_usage);
            def_stack.extend(new_usages);

            // Update refs_usage with the usages from this iteration
            refs_usage.extend(cur_usage);
        }

        // We need to pass a ref usage object when converting properties and definitions to put
        // them in the environment. However, we don't care about properties, because they've been
        // converted at least once already (all properties unconditionally appear in the final
        // contract). Thus, converting those properties again shouldn't add new usage, and we can
        // ignore their usage.
        let mut usage_placeholder = RefsUsage::new();

        for path in refs_usage.props_predicates.iter() {
            let Some(schema) = get_property(&root_schema.schema, path) else {
                eprintln!(
                    "Warning: property `{}` is referenced in the schema but couldn't be found",
                    path.join("/")
                );
                continue;
            };

            let predicate = schema.as_predicate(&mut usage_placeholder);
            let doc = Documentation::try_from(schema).ok();

            property_preds.insert(path.clone(), ConvertedProp { doc, predicate });
        }

        Environment {
            definitions,
            property_preds,
        }
    }

    /// Wrap a Nickel [`RichTerm`] in a let binding containing the definitions
    /// from the environment. This is necessary for the Nickel access terms
    /// tracked in the environment to actually work.
    pub fn wrap(mut self, inner: RichTerm) -> RichTerm {
        let contracts = self
            .definitions
            .iter_mut()
            .filter_map(|(k, v)| Some((Ident::from(k), v.contract_as_field()?)))
            .collect();

        let predicates = self
            .definitions
            .into_iter()
            .filter_map(|(k, mut v)| Some((Ident::from(k), v.predicate_as_field()?)))
            .collect();

        let prop_preds = self
            .property_preds
            .into_iter()
            .map(|(k, v)| (Ident::from(k.join("/")), Field::from(v)))
            .collect();

        // All the definitions as a Nickel record
        let defs = Term::Record(RecordData::with_field_values(
            [
                (
                    Ident::from("contracts"),
                    Term::Record(RecordData {
                        fields: contracts,
                        ..Default::default()
                    })
                    .into(),
                ),
                (
                    Ident::from("predicates"),
                    Term::Record(RecordData {
                        fields: predicates,
                        ..Default::default()
                    })
                    .into(),
                ),
            ]
            .into_iter()
            .collect(),
        ))
        .into();

        // All the properties (predicates) as a Nickel record
        let props = Term::Record(RecordData {
            fields: prop_preds,
            ..Default::default()
        })
        .into();

        // The enclosing record, with one field for the definitions and one for the properties
        let global_env = Term::Record(RecordData::with_field_values(
            [
                (Ident::from(DEFINITIONS_MANGLED), defs),
                (Ident::from(PROPS_PREDICATES_MANGLED), props),
            ]
            .into_iter()
            .collect(),
        ));

        Term::Let(
            Ident::from(ENVIRONMENT_MANGLED),
            global_env.into(),
            inner,
            LetAttrs {
                rec: true,
                ..Default::default()
            },
        )
        .into()
    }
}

/// Get the property located at a path in a schema.
///
/// # Example
///
/// For a path `["foo", "bar"]`, this function will extract (if it exists) the schema corresponding
/// to the JSON pointer `properties/foo/properties/bar`.
///
/// # Return values
///
/// - Returns `Some(subschema)` if the path exists in the schema and points to `subschema`.
/// - Returns `None` if the path does not exist in the schema or the path is empty.
///
/// Note: it looks like we could return the original value upon empty path, but there's a mismatch:
/// we get a `SchemaObject` reference, and we must return a `Schema` reference. We can't convert
/// between the two (we can convert between the owned variants easily, but not for references).
/// Since we can special case empty paths before calling to `get_property` if really needed, it's
/// simpler to just return `None` here.
pub fn get_property<'a>(schema_obj: &'a SchemaObject, path: &[String]) -> Option<&'a Schema> {
    let mut current: Option<&Schema> = None;

    for prop in path {
        // We start from a schema object, but then always go from schemas to schemas, which requires
        // this bit of juggling.
        let current_obj = match current {
            // We had at least one iteration before and the current schema is an object, which means we
            // can indeed index into it.
            Some(Schema::Object(next)) => next,
            // We had at least one iteration before but the current schema isn't an object, we
            // can't index into it.
            Some(_) => return None,
            // This is the first iteration, so we start from the initial schema object
            None => schema_obj,
        };

        current = Some(current_obj.object.as_ref()?.properties.get(prop)?);
    }

    current
}
