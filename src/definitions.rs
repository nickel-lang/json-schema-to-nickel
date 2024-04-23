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

use std::collections::HashSet;

use nickel_lang_core::{
    identifier::LocIdent,
    term::{
        record::{Field, FieldMetadata, RecordData},
        LetAttrs, RichTerm, Term,
    },
};

use schemars::schema::{RootSchema, Schema, SchemaObject, SingleOrVec};

use crate::{
    contracts::{AsPredicateContract, Contract, Documentation, TryAsContract},
    predicates::{AsPredicate, Predicate},
    utils::{decode_json_ptr_part, static_access},
    ENVIRONMENT_ID, MANGLING_PREFIX,
};

/// Specify if a reference is used in a context which requires a contract or a predicate.
#[derive(Clone, Debug, Copy)]
pub enum RefUsageContext {
    Contract,
    Predicate,
}

impl RefUsageContext {
    /// Generate a default conversion for the given usage, when the reference can't be found, can't
    /// be supported, etc. For contracts, it's the `Dyn` contract, and the always true predicate.
    pub fn default_term(&self) -> RichTerm {
        match self {
            RefUsageContext::Contract => Contract::dynamic().into(),
            RefUsageContext::Predicate => Predicate::always().into(),
        }
    }
}

impl std::fmt::Display for RefUsageContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RefUsageContext::Contract => write!(f, "contract"),
            RefUsageContext::Predicate => write!(f, "predicate"),
        }
    }
}

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

    /// Returns a Nickel term that accesses the value pointed by `self` by looking it up either in
    /// the references environment or directly in the final contract for pure property paths.
    pub fn access(&self, usage: RefUsageContext) -> RichTerm {
        match (self.try_as_field_path(), usage) {
            // The case of pure property paths is special, as we access them directly from within
            // the final contract, instead of looking them up in the references environment.
            (Some(field_path), RefUsageContext::Contract) => field_path.into(),
            _ => static_access(ENVIRONMENT_ID, [self.nickel_uid(usage).as_str()]),
        }
    }

    /// Returns a single mangled string uniquely identifying this pointer and its usage. This name
    /// is used to store the reference in (and load it from) the references environment.
    pub fn nickel_uid(&self, usage: RefUsageContext) -> String {
        let path = self
            .path
            .iter()
            .map(|elt| elt.to_string())
            .collect::<Vec<_>>()
            .join("/");

        format!("{MANGLING_PREFIX}:{path}!{usage}")
    }

    /// Returns `true` if the path is composed only of properties.
    pub fn is_only_props(&self) -> bool {
        self.path
            .iter()
            .all(|elt| matches!(elt, SchemaPointerElt::Properties(_)))
    }

    /// Returns the subschema pointed to by `self` in the given schema object.
    ///
    /// # Return values
    ///
    /// - Returns `Some(subschema)` if the path exists in the schema and points to `subschema`.
    /// - Returns `None` if the path does not exist in the schema or the path is empty.
    ///
    /// Note: it looks like we could return the original value upon empty path, but there's a type
    /// mismatch: we get a `SchemaObject` reference, and we must return a `Schema` reference. We
    /// can't convert between the two (we can convert between the owned variants easily, but not
    /// for references). Since we can special case empty paths before calling [Self::resolve] if
    /// really needed, it's simpler to just return `None` here.
    pub fn resolve<'a>(&self, root_schema: &'a RootSchema) -> Option<&'a Schema> {
        enum CurrentSchema<'a> {
            Schema(&'a Schema),
            Root(&'a RootSchema),
        }

        impl<'a> CurrentSchema<'a> {
            fn object<'b>(&'b self) -> Option<&'a SchemaObject> {
                match self {
                    CurrentSchema::Schema(Schema::Object(obj)) => Some(obj),
                    CurrentSchema::Schema(Schema::Bool(_)) => None,
                    CurrentSchema::Root(root) => Some(&root.schema),
                }
            }
        }

        fn warn_if_out_of_bounds<T>(vec: &[T], index: usize, keyword: &str) {
            if index >= vec.len() {
                eprintln!(
                    "Warning: out-of-bounds array access `{keyword}/{index}` in a reference. \
                    {keyword} has only {} element(s)",
                    vec.len()
                );
            }
        }

        let mut current = CurrentSchema::Root(root_schema);

        for elt in self.path.iter() {
            let new = match elt {
                SchemaPointerElt::Definitions(name) => {
                    if let CurrentSchema::Root(root) = current {
                        root.definitions.get(name).map(CurrentSchema::Schema)
                    } else {
                        eprintln!("Warning: couldn't access nested definition `{name}`. json-schema-to-nickel only supports references to top-level definitions");
                        None
                    }
                }
                SchemaPointerElt::Properties(prop) => current
                    .object()?
                    .object
                    .as_ref()?
                    .properties
                    .get(prop)
                    .map(CurrentSchema::Schema),
                SchemaPointerElt::AdditionalProperties => current
                    .object()?
                    .object
                    .as_ref()?
                    .additional_properties
                    .as_ref()
                    .map(|s| CurrentSchema::Schema(s.as_ref())),
                SchemaPointerElt::ItemsIndexed(index) => {
                    match current.object()?.array.as_ref()?.items.as_ref() {
                        Some(SingleOrVec::Vec(vec)) => {
                            warn_if_out_of_bounds(vec, *index, "items");
                            vec.get(*index).map(CurrentSchema::Schema)
                        }
                        Some(SingleOrVec::Single(_)) => {
                            eprintln!(
                                "Warning: trying to access `items` at index {index} in a \
                                reference, but `items` is a single schema in the current \
                                document, not an array"
                            );
                            None
                        }
                        _ => None,
                    }
                }
                SchemaPointerElt::ItemsSingle => match &current.object()?.array.as_ref()?.items {
                    Some(SingleOrVec::Single(sub)) => Some(CurrentSchema::Schema(sub.as_ref())),
                    Some(SingleOrVec::Vec(_)) => {
                        eprintln!(
                            "Warning: trying to access `items` in a reference without an index, \
                            but `items` is an array of schemas in the current document, not \
                            a single schema"
                        );
                        None
                    }
                    _ => None,
                },
                SchemaPointerElt::Contains => Some(CurrentSchema::Schema(
                    current.object()?.array.as_ref()?.contains.as_ref()?,
                )),
                SchemaPointerElt::AllOf(index) => {
                    let all_of = current.object()?.subschemas.as_ref()?.all_of.as_ref()?;
                    warn_if_out_of_bounds(all_of, *index, "allOf");
                    all_of.get(*index).map(CurrentSchema::Schema)
                }
                SchemaPointerElt::AnyOf(index) => {
                    let any_of = current.object()?.subschemas.as_ref()?.any_of.as_ref()?;
                    warn_if_out_of_bounds(any_of, *index, "anyOf");
                    any_of.get(*index).map(CurrentSchema::Schema)
                }
                SchemaPointerElt::OneOf(index) => {
                    let one_of = current.object()?.subschemas.as_ref()?.one_of.as_ref()?;
                    warn_if_out_of_bounds(one_of, *index, "oneOf");
                    one_of.get(*index).map(CurrentSchema::Schema)
                }
                SchemaPointerElt::Not => current
                    .object()?
                    .subschemas
                    .as_ref()?
                    .not
                    .as_ref()
                    .map(|s| CurrentSchema::Schema(s)),
                SchemaPointerElt::Then => current
                    .object()?
                    .subschemas
                    .as_ref()?
                    .then_schema
                    .as_ref()
                    .map(|s| CurrentSchema::Schema(s)),
                SchemaPointerElt::Else => current
                    .object()?
                    .subschemas
                    .as_ref()?
                    .else_schema
                    .as_ref()
                    .map(|s| CurrentSchema::Schema(s.as_ref())),
            }?;

            current = new;
        }

        if let CurrentSchema::Schema(current) = current {
            Some(current)
        } else {
            None
        }
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

/// The conversion of a JSON schema reference pointee (a definition, a property, or any subschema
/// really) into a Nickel predicate or contract.
#[derive(Clone)]
pub struct ConvertedRef {
    /// The schema pointer leading to this reference.
    pointer: SchemaPointer,
    /// The usage context of the reference.
    usage: RefUsageContext,
    /// The documentation associated with the reference pointee.
    doc: Option<Documentation>,
    /// The translation of the pointed schema into a Nickel term.
    term: RichTerm,
}

impl ConvertedRef {
    /// Return this reference as a Nickel record binding as it appears in the references environment,
    /// that is a pair of an identifier and a field value.
    pub fn into_binding(self) -> (LocIdent, Field) {
        (self.pointer.nickel_uid(self.usage).into(), self.into())
    }
}

impl From<ConvertedRef> for Field {
    fn from(value: ConvertedRef) -> Self {
        Field {
            value: Some(value.term),
            metadata: FieldMetadata {
                doc: value.doc.map(String::from),
                ..Default::default()
            },
            ..Default::default()
        }
    }
}

/// State recording which references are actually used and how (as predicates or as contracts).
#[derive(Clone, Default)]
pub struct RefsUsage {
    /// The references used as predicates somewhere in the schema.
    pub predicates: HashSet<SchemaPointer>,
    /// The references used as contracts somewhere in the schema (excluding properties).
    pub contracts: HashSet<SchemaPointer>,
}

impl RefsUsage {
    /// The empty state
    pub fn new() -> Self {
        Self::default()
    }

    /// Return the tuple of set difference between the predicates and the contracts referenced in
    /// `self` and referenced in `other`.
    pub fn diff(&self, other: &RefsUsage) -> RefsUsage {
        RefsUsage {
            predicates: &self.predicates - &other.predicates,
            contracts: &self.contracts - &other.contracts,
        }
    }

    /// Extend the usages of `self` with the usages of `other`.
    pub fn extend(&mut self, other: RefsUsage) {
        self.predicates.extend(other.predicates);
        self.contracts.extend(other.contracts);
    }

    /// Record the usage of a JSON Schema reference.
    pub fn record_usage(&mut self, reference: SchemaPointer, usage: RefUsageContext) {
        // We don't record a contract usage of a property reference.
        if !(reference.is_only_props() && matches!(usage, RefUsageContext::Contract)) {
            match usage {
                RefUsageContext::Contract => {
                    self.contracts.insert(reference);
                }
                RefUsageContext::Predicate => {
                    self.predicates.insert(reference);
                }
            }
        }
    }
}

/// Resolve a JSON schema reference to a Nickel term. The resulting Nickel expression will have a
/// different shape depending on the usage context and the type of reference (definition vs
/// property).
///
/// # Arguments
///
/// - `reference`: the JSON schema reference to resolve. It must be a valid URI. Currently only
/// local references are supported (i.e. URI starting with `#/`).
/// - `refs_usage`: the state used to record which references are actually used, and
///   how. `resolve_ref` will record this usage accordingly
/// - `usage`: the context in which the reference is used. Some contexts requires a predicate,
///   while other can do with a contract.
pub fn resolve_ref(
    reference: &str,
    refs_usage: &mut RefsUsage,
    usage: RefUsageContext,
) -> RichTerm {
    if let Some(fragment) = reference.strip_prefix("#/") {
        let schema_ptr = match SchemaPointer::parse(fragment) {
            Ok(ptr) => ptr,
            Err(err) => {
                eprintln!(
                    "Warning: skipping external reference {reference} (replaced by an always \
                    succeeding contract). {err}"
                );

                return usage.default_term();
            }
        };

        refs_usage.record_usage(schema_ptr.clone(), usage);
        schema_ptr.access(usage)
    } else {
        eprintln!(
            "
            Warning: skipping external reference {reference} (replaced by an always succeeding \
            contract). The current version of json-schema-to-nickel only supports internal \
            JSON pointer references"
        );

        usage.default_term()
    }
}

/// An environment of all reference pointees and their conversions into Nickel predicates and
/// contracts.
#[derive(Clone, Default)]
pub struct Environment {
    /// The list of all references used in the schema.
    references: Vec<ConvertedRef>,
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
    /// subschemas, those subschemas might themselves reference other subschemas that were not used
    /// until now. We record those usages as well, and iterate until no new definition is ever
    /// referenced.
    pub fn new(root_schema: &RootSchema, mut refs_usage: RefsUsage) -> Self {
        let mut references =
            Vec::with_capacity(refs_usage.predicates.len() + refs_usage.contracts.len());

        // The stack of references to process. We might grow this stack as converting some
        // references might refer to new subschemas.
        let mut ref_stack: Vec<_> = refs_usage
            .predicates
            .iter()
            .map(|ptr| (ptr.clone(), RefUsageContext::Predicate))
            .chain(
                refs_usage
                    .contracts
                    .iter()
                    .map(|ptr| (ptr.clone(), RefUsageContext::Contract)),
            )
            .collect();

        while let Some((schema_ptr, usage)) = ref_stack.pop() {
            let mut new_refs_usage = RefsUsage::new();

            let (doc, term) = {
                if let Some(schema) = schema_ptr.resolve(root_schema) {
                    let doc = Documentation::try_from(schema).ok();

                    let term = match usage {
                        RefUsageContext::Contract => schema
                            .try_as_contract(&mut new_refs_usage)
                            .unwrap_or_else(|| schema.as_predicate_contract(&mut new_refs_usage))
                            .into(),
                        RefUsageContext::Predicate => {
                            schema.as_predicate(&mut new_refs_usage).into()
                        }
                    };

                    (doc, term)
                } else {
                    eprintln!(
                    "Warning: definition `{schema_ptr}` is referenced in the schema but couldn't be found. Replaced with an always succeeding contract."
                    );

                    (None, usage.default_term())
                }
            };

            references.push(ConvertedRef {
                doc,
                term,
                usage,
                pointer: schema_ptr,
            });

            // Adding the new usages to the stack
            let usage_diff = new_refs_usage.diff(&refs_usage);

            ref_stack.extend(
                usage_diff
                    .predicates
                    .into_iter()
                    .map(|ptr| (ptr, RefUsageContext::Predicate)),
            );

            ref_stack.extend(
                usage_diff
                    .contracts
                    .into_iter()
                    .map(|ptr| (ptr, RefUsageContext::Contract)),
            );

            // Update refs_usage with the usages from this iteration
            refs_usage.extend(new_refs_usage);
        }

        Environment { references }
    }

    /// Wrap a Nickel [`RichTerm`] in a let binding containing the definitions
    /// from the environment. This is necessary for the Nickel access terms
    /// tracked in the environment to actually work.
    pub fn wrap(self, inner: RichTerm) -> RichTerm {
        let fields = self
            .references
            .into_iter()
            .map(ConvertedRef::into_binding)
            .collect();

        // All references are stored in the references environment, which is a flat record.
        let global_env = Term::Record(RecordData {
            fields,
            ..Default::default()
        });

        Term::Let(
            ENVIRONMENT_ID.into(),
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
