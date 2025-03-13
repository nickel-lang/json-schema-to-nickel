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
//!   definitions at the top level and reference them from other parts of the schema.
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
use fluent_uri;

use nickel_lang_core::term::RichTerm;

use crate::utils::{decode_json_ptr_part, static_access};

/// A representation of a field path in the final generated contract.
///
/// # Invariants
///
/// The path is guaranteed to be non-empty by construction. Do not directly mutate the underlying
/// path at the risk of making it empty.
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

/// A representation of a [JSON pointer](https://datatracker.ietf.org/doc/html/rfc6901) inside a
/// JSON Schema. This type is more structured that a generic JSON pointer, as it matches the
/// specific and constrained structure of a JSON Schema.
///
/// For example, a JSON Schema reference `#/properties/foo/contains/items/0/allOf/5` will be parsed
/// as a [SchemaPointer] of the form `[Properties("foo"), Contains, Items(0), allOf(5)]`.
#[derive(Hash, Clone, Debug, Default, Eq, PartialEq)]
pub struct SchemaPointer {
    pub path: Vec<SchemaPointerElt>,
}

impl SchemaPointer {
    /// Parse a string representation of a JSON pointer as a schema pointer.
    pub fn parse(json_ptr: &str) -> Result<Self, SchemaPointerParseError> {
        fn parse_array_idx(
            keyword: String,
            index_str: Option<String>,
        ) -> Result<usize, SchemaPointerParseError> {
            let index_str =
                index_str.ok_or_else(|| SchemaPointerParseError::MissingIndex(keyword.clone()))?;

            let index: usize =
                index_str
                    .parse()
                    .map_err(|_| SchemaPointerParseError::InvalidArrayIndex {
                        keyword,
                        index: index_str,
                    })?;

            Ok(index)
        }

        let mut path = Vec::new();
        let mut it = json_ptr.split('/').map(decode_json_ptr_part).peekable();

        while let Some(keyword) = it.next() {
            match keyword.as_ref() {
                "definitions" => path.push(SchemaPointerElt::Definitions(
                    it.next()
                        .ok_or(SchemaPointerParseError::MissingIndex(keyword))?,
                )),
                "properties" => path.push(SchemaPointerElt::Properties(
                    it.next()
                        .ok_or(SchemaPointerParseError::MissingIndex(keyword))?,
                )),
                "additionalProperties" => {
                    path.push(SchemaPointerElt::AdditionalProperties);
                }
                "items" => {
                    if let Ok(index) = parse_array_idx(keyword, it.peek().cloned()) {
                        // Actually consume the token we've peeked at
                        it.next();
                        path.push(SchemaPointerElt::ItemsIndexed(index));
                    } else {
                        path.push(SchemaPointerElt::ItemsSingle);
                    }
                }
                "contains" => {
                    path.push(SchemaPointerElt::Contains);
                }
                "allOf" => {
                    path.push(SchemaPointerElt::AllOf(parse_array_idx(
                        keyword,
                        it.next(),
                    )?));
                }
                "anyOf" => {
                    path.push(SchemaPointerElt::AnyOf(parse_array_idx(
                        keyword,
                        it.next(),
                    )?));
                }
                "oneOf" => {
                    path.push(SchemaPointerElt::OneOf(parse_array_idx(
                        keyword,
                        it.next(),
                    )?));
                }
                "not" => {
                    path.push(SchemaPointerElt::Not);
                }
                "then" => {
                    path.push(SchemaPointerElt::Then);
                }
                "else" => {
                    path.push(SchemaPointerElt::Else);
                }
                _ => return Err(SchemaPointerParseError::UnsupportedKeyword(keyword)),
            }
        }

        Ok(SchemaPointer { path })
    }

    /// Takes a schema pointer whose path is comprised only of properties and returns the
    /// corresponding path in the final generated contract, that is the sequence of property's
    /// names with all the intermediate `/properties` stripped.
    ///
    /// # Example
    ///
    /// Running [Self::try_as_field_path] on a schema pointer `/properties/foo/properties/bar` will
    /// return the field path `["foo", "bar"]`.
    ///
    /// # Return values
    ///
    /// If the path is empty or isn't composed only of properties, this method returns `None`.
    pub fn try_as_field_path(&self) -> Option<FieldPath> {
        let stripped: Option<Vec<_>> = self
            .path
            .iter()
            .map(|elt| match elt {
                SchemaPointerElt::Properties(name) => Some(name.clone()),
                _ => None,
            })
            .collect();

        FieldPath::try_from(stripped?).ok()
    }

    /// Returns `true` if the path is composed only of properties.
    pub fn is_only_props(&self) -> bool {
        self.path
            .iter()
            .all(|elt| matches!(elt, SchemaPointerElt::Properties(_)))
    }
}

impl std::fmt::Display for SchemaPointer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for elt in self.path.iter() {
            write!(f, "/{elt}")?;
        }

        Ok(())
    }
}

/// Element of a JSON pointer supported by json-schema-to-nickel. A JSON Pointer is parsed as a
/// sequence of [SchemaPointerElt] which locates a subschema within a JSON Schema. Each variant
/// corresponds to a JSON schema keyword.
///
/// Keywords containing a single schema (such as `contains`) don't need any additional data.
/// Keywords storing objects carries a string indicating which property of the object should be
/// accessed (e.g. `properties/foo`). Keywords storing arrays carries the index of the array to
/// access (e.g. `items/0`).
///
/// `prefixItems` is a JSON Schema keyword that could be supported as well but it's unfortunately
/// not supported by `schemars`, so we ignore it.
#[derive(Hash, Clone, Debug, Eq, PartialEq)]
pub enum SchemaPointerElt {
    Definitions(String),
    Properties(String),
    AdditionalProperties,
    /// An `items` followed by an array index.
    ///
    /// In a JSON Schema, `items` might be either a single schema or an array of schemas. In the
    /// single schema case, it doesn't need to be indexed in a json pointer, as in
    /// `#/items/properties/foo`. In the array case, an `items` must be followed by a natural
    /// number. As we want to parse schema pointer independently of the schema, whether an `items`
    /// is parsed as [Self::ItemsIndexed] or [Self::Items] depends on the next element of the path
    /// being a valid array index or not.
    ItemsIndexed(usize),
    /// An `items` not followed by a valid array index.
    ItemsSingle,
    Contains,
    AllOf(usize),
    AnyOf(usize),
    OneOf(usize),
    Not,
    Then,
    Else,
}

impl std::fmt::Display for SchemaPointerElt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SchemaPointerElt::Definitions(name) => write!(f, "definitions/{name}"),
            SchemaPointerElt::Properties(name) => write!(f, "properties/{name}"),
            SchemaPointerElt::AdditionalProperties => write!(f, "additionalProperties"),
            SchemaPointerElt::ItemsIndexed(index) => write!(f, "items/{index}"),
            SchemaPointerElt::ItemsSingle => write!(f, "items"),
            SchemaPointerElt::Contains => write!(f, "contains"),
            SchemaPointerElt::AllOf(index) => write!(f, "allOf/{index}"),
            SchemaPointerElt::AnyOf(index) => write!(f, "anyOf/{index}"),
            SchemaPointerElt::OneOf(index) => write!(f, "oneOf/{index}"),
            SchemaPointerElt::Not => write!(f, "not"),
            SchemaPointerElt::Then => write!(f, "then"),
            SchemaPointerElt::Else => write!(f, "else"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum SchemaPointerParseError {
    /// An element of a JSON pointer, supposedly referring to a JSON Schema keyword, isn't
    /// supported at the moment by json-schema-to-nickel (supported keyword are the variants of
    /// [SchemaPointerElt]). As the standard of JSON Schema is evolving, we don't make a difference
    /// between an unsupported keyword and an invalid one.
    UnsupportedKeyword(String),
    /// A JSON pointer keyword isn't properly followed by an index or a property name. For example
    /// `#/properties/foo/items` or `#/items/0/property` are invalid (they are incomplete).
    MissingIndex(String),
    /// An index into an array is not a valid number. For example, `#/items/foo`.
    InvalidArrayIndex { keyword: String, index: String },
}

impl std::fmt::Display for SchemaPointerParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SchemaPointerParseError::UnsupportedKeyword(keyword) => {
                write!(f, "Unsupported JSON schema keyword `{keyword}`")
            }
            SchemaPointerParseError::MissingIndex(keyword) => {
                write!(
                    f,
                    "`{keyword}` isn't properly followed by an index or the name of a property"
                )
            }
            SchemaPointerParseError::InvalidArrayIndex { keyword, index } => {
                write!(f, "`{keyword}` is an array which must be indexed by a number but is followed by `{index}`")
            }
        }
    }
}

pub fn resolve_ptr(reference: &str) -> Option<SchemaPointer> {
    let Ok(uri) = fluent_uri::Uri::parse(reference) else {
        eprintln!(
            "Warning: skipping reference `{reference}` (replaced by an always succeeding \
                contract). Failed to parse it as valid URI."
        );

        return None;
    };

    // If the URI has anything else than a fragment, we bail out.
    if uri.scheme().is_some()
        || uri.authority().is_some()
        || !uri.path().as_str().is_empty()
        || uri.query().is_some()
    {
        eprintln!(
            "Warning: skipping external reference `{reference}` (replaced by an always \
                succeeding contract). The current version of json-schema-to-nickel only supports \
                internal references"
        );

        return None;
    }

    // We don't support (yet) relative fragments.
    let Some(fragment) = uri
        .fragment()
        .map(|fragment| fragment.decode().into_string_lossy())
    else {
        eprintln!(
            "Warning: skipping reference `{reference}` (replaced by an always succeeding \
                contract). The URI doesn't have a fragment"
        );

        return None;
    };

    let Some(stripped) = fragment.strip_prefix('/') else {
        eprintln!(
            "Warning: skipping reference `{reference}` (replaced by an always succeeding \
                contract). json-schema-to-nickel doesn't handle references to element ids."
        );

        return None;
    };

    match SchemaPointer::parse(stripped) {
        Ok(ptr) => Some(ptr),
        Err(err) => {
            eprintln!(
                "Warning: skipping reference `{reference}` (replaced by an always \
                    succeeding contract). {err}"
            );

            None
        }
    }
}
