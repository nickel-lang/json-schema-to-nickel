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

use std::collections::{BTreeMap, HashMap, HashSet};

use nickel_lang_core::{
    identifier::Ident,
    term::{
        record::{Field, FieldMetadata, RecordData},
        LetAttrs, RichTerm, Term,
    },
    typ::TypeF,
};
use schemars::schema::Schema;

use crate::{
    contracts::{contract_from_predicate, Contract, Documentation},
    predicates::Predicate,
    utils::{decode_json_ptr_part, static_access},
    DEFINITIONS_MANGLED, PROPS_PREDICATES_MANGLED,
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
    fn new(ptr: &str) -> Self {
        Self {
            path: ptr.split('/').map(decode_json_ptr_part).collect(),
        }
    }

    /// Take a path pointing to a property and returns the corresponding path in the final
    /// generated contract, that is, with all the intermediate `properties` stripped.
    ///
    /// For example, running [Self::as_field_path] on a JSON pointer
    /// `#/properties/foo/properties/bar` will return the field path `["foo", "bar"]`.
    fn try_as_field_path(&self) -> Option<FieldPath> {
        let mut it = self.path.iter();
        let mut result = Vec::with_capacity(self.path.len() / 2);

        // We expect that the path can be grouped as a sequence of two elements, where the first
        // one is always `properties`, and the second one corresponds is the property name.
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

/// The nickel predicate and contract generated for a schema.
#[derive(Clone)]
pub struct ConvertedSchema {
    doc: Option<Documentation>,
    predicate: Predicate,
    contract: Contract,
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
    /// We don't need to keep track of the contracts, as they will inconditionnally be constituents
    /// of the final schema.
    pub props_predicates: HashSet<Vec<String>>,
}

impl RefsUsage {
    /// The empty state
    pub fn new() -> Self {
        Self::default()
    }
}

/// An environment of top level schema definitions and nested properties and their conversions into
/// Nickel predicates and contracts.
#[derive(Clone, Default)]
pub struct Environment {
    /// The top-level definition of the schema.
    defs: HashMap<String, ConvertedSchema>,
    /// The predicates of the properties of the schema. We only need to store the predicates, and
    /// not the contract, as the contract are simply accessible recursively in the resulting
    /// schema. For example, the contract for the reference `#/properties/foo/properties/bar` is
    /// simply `foo.bar`.
    ///
    /// The key is the path the property. In our previous example, the key would be `["foo",
    /// "bar"]`.
    prop_predicates: HashMap<Vec<String>, ConvertedSchema>,
    /// Although we store every property in the environment, we will only need to access the one
    /// that are actually referenced in a `$ref` attribute, which is usually a small subset (and is
    /// often entirely empty, if there's no local reference to a property).
    ///
    /// Thus, while elaborating the schema, we keep track of the properties that are actually used,
    /// and only include those predicates in the final schema.
    used_props: HashSet<Vec<String>>,
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
            RefUsage::Contract => Term::Type(TypeF::Dyn.into()).into(),
            RefUsage::Predicate => static_access("predicates", ["always"]),
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
                    // We don't index the properties elements by elements, as in
                    // `<PROPS_PREDICATES_MANGLED>.foo.bar.baz`, but we use the whole path with `/`
                    // as a separator as a key. See the documentation of `PROPS_PREDICATES_MANGLED`
                    // for more information.
                    static_access(
                        PROPS_PREDICATES_MANGLED,
                        [field_path.path.join("/").as_str()],
                    )
                }
            }
        } else if let Some(name) = json_ptr.try_as_def() {
            match usage {
                RefUsage::Contract => {
                    state.defs_contracts.insert(name.clone());
                    static_access(DEFINITIONS_MANGLED, [name.as_ref(), "contract"])
                }
                RefUsage::Predicate => {
                    state.defs_predicates.insert(name.clone());
                    static_access(DEFINITIONS_MANGLED, [name.as_ref(), "predicate"])
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

    /// Create an environment from the top-level JSON schema and a ref state indicating which
    /// properties and definitions actually need to be considered.
    pub fn new(root_schema: Schema, state: &RefsUsage) -> Self {
        // FIXME: Definitions can have their own definitions. Does this handle that
        //        correctly? Does schema.rs even handle it correctly?
        todo!()
    }

    /// Wrap a Nickel [`RichTerm`] in a let binding containing the definitions
    /// from the environment. This is necessary for the Nickel access terms
    /// tracked in the environment to actually work.
    pub fn wrap(self, inner: RichTerm) -> RichTerm {
        let contracts = self
            .defs
            .iter()
            .map(|(k, v)| {
                (
                    Ident::from(k),
                    Field {
                        value: Some(v.contract.clone().into()),
                        metadata: FieldMetadata {
                            doc: v.doc.clone().map(String::from),
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                )
            })
            .collect();

        let predicates = self
            .defs
            .into_iter()
            .map(|(k, v)| {
                (
                    Ident::from(k),
                    Field {
                        value: Some(v.predicate.into()),
                        metadata: FieldMetadata {
                            doc: v.doc.map(String::from),
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                )
            })
            .collect();

        Term::Let(
            "definitions".into(),
            Term::Record(RecordData::with_field_values(
                [
                    (
                        Ident::from("contract"),
                        Term::Record(RecordData {
                            fields: contracts,
                            ..Default::default()
                        })
                        .into(),
                    ),
                    (
                        Ident::from("predicate"),
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
            .into(),
            inner,
            LetAttrs {
                rec: true,
                ..Default::default()
            },
        )
        .into()
    }
}

// /// Convert the `definitions` field of a json schema mapping identifiers to
// /// Schemas to an [`Environment`] struct mapping identifiers to Nickel terms
// impl From<&BTreeMap<String, Schema>> for Environment {
//     fn from(defs: &BTreeMap<String, Schema>) -> Self {
//         let terms = defs
//             .iter()
//             .map(|(name, schema)| {
//                 (
//                     name.clone(),
//                     ConvertedSchema {
//                         doc: Documentation::try_from(schema).ok(),
//                         contract: Contract::try_from(schema).unwrap_or_else(|()| {
//                             contract_from_predicate(Predicate::from(access_def(name).predicate))
//                         }),
//                         predicate: Predicate::from(schema),
//                     },
//                 )
//             })
//             .collect();
//         Environment::new(terms)
//     }
// }
