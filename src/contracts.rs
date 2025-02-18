//! # Nickel contract generation for JSON schemas
//!
//! Since generating lazy Nickel contracts for arbitrary JSON schemas is impossible, this module
//! restricts itself to generating record contracts for JSON schemas that are simple enough. A JSON
//! schema can be successfully turned into a record contract if it takes the form
//!
//! ```json
//! {
//!   "type": "object",
//!   "required": ...,
//!   "properties": {
//!     ...
//!   }
//! }
//! ```
//!
//! Schemas specifying a single primitive type are turned into proper contracts
//! as well, for example
//!
//! ```json
//! {
//!   "type": "boolean"
//! }
//! ```
//!
//! is turned into the Nickel type `Bool`.
use crate::{
    references::RefUsageContext,
    utils::{distinct, schema_types},
};
use nickel_lang_core::typ::EnumRowF;
use schemars::schema::{RootSchema, StringValidation, SubschemaValidation};
use std::collections::{BTreeMap, BTreeSet};

use nickel_lang_core::{
    label::Label,
    mk_app,
    term::{
        record::{Field, FieldMetadata, RecordAttrs, RecordData},
        LabeledType, RichTerm, Term, TypeAnnotation,
    },
    typ::{EnumRows, EnumRowsF, RecordRows, Type, TypeF},
};
use schemars::schema::{
    ArrayValidation, InstanceType, ObjectValidation, Schema, SchemaObject, SingleOrVec,
};
use serde_json::Value;

use crate::{
    predicates::{AsPredicate, Predicate},
    references::{self, RefsUsage},
    utils::static_access,
    PREDICATES_LIBRARY_ID,
};

fn only_ignored_fields<V>(extensions: &BTreeMap<String, V>) -> bool {
    const IGNORED_FIELDS: &[&str] = &["$comment"];
    !extensions
        .keys()
        .any(|x| !IGNORED_FIELDS.contains(&x.as_ref()))
}

/// Sometimes we can simplify schemas if they have a simple known type.
///
/// This enum captures the cases that we handle.
#[derive(Debug, Copy, Clone)]
enum SimpleType {
    /// A single type, like "object" or "array".
    Just(InstanceType),
    /// A single type, but it could also be null.
    Nullable(InstanceType),
    /// A more complicated type that we don't have special handling for.
    Complicated,
}

impl SimpleType {
    fn from_types(types: &SingleOrVec<InstanceType>) -> SimpleType {
        match types {
            SingleOrVec::Single(s) => SimpleType::Just(**s),
            SingleOrVec::Vec(vec) => match vec.as_slice() {
                [s] => SimpleType::Just(*s),
                [s, InstanceType::Null] | [InstanceType::Null, s] => SimpleType::Nullable(*s),
                _ => SimpleType::Complicated,
            },
        }
    }

    fn inner(&self) -> Option<&InstanceType> {
        match self {
            SimpleType::Nullable(instance_type) | SimpleType::Just(instance_type) => {
                Some(instance_type)
            }
            SimpleType::Complicated => None,
        }
    }
}

/// [`Contract`] represents the set of contracts that would be applied to a
/// value. This can be empty or many as in `a` or `a | Foo | Bar`, but this
/// list can also be converted to a single value using `predicates.always` and
/// std.contract.Sequence
#[derive(Clone, Debug)]
pub struct Contract {
    terms: Vec<RichTerm>,
    maybe_null: bool,
}

impl Contract {
    /// Convert a root JSON schema to a contract. Returns `None` if the schema couldn't be
    /// converted to a (lazy) contract, and thus requires to go through a predicate.
    /// Upon success, returns the contract and the references used in the schema.
    pub fn from_root_schema(root_schema: &RootSchema) -> Option<(Self, RefsUsage)> {
        let mut refs_usage = RefsUsage::new();

        root_schema
            .schema
            .try_as_contract(root_schema, &mut refs_usage)
            .map(|ctr| (ctr, refs_usage))
    }

    /// Return the `Dyn` contract, always succeeding.
    pub fn dynamic() -> Self {
        let dyn_type: Type = TypeF::Dyn.into();
        Term::Type {
            typ: dyn_type.clone(),
            // We don't care about the contract here, it's a run-time cache thing.
            contract: Term::Null.into(),
        }
        .into()
    }

    /// Return a contract that's satisfied by either "null" or this contract.
    pub fn or_null(self) -> Contract {
        Contract {
            maybe_null: true,
            ..self
        }
    }

    pub fn from_terms(terms: Vec<RichTerm>) -> Self {
        Self {
            terms,
            maybe_null: false,
        }
    }
}

/// [TryAsContract] is essentially like `TryInto<Contract>` but passes additional state around used for
/// effective reference resolution.
pub trait TryAsContract {
    /// Try to convert a JSON schema component `Self` to a contract. Returns `None` if the
    /// component couldn't be converted to a lazy contract, and thus requires to go through a
    /// predicate.
    ///
    /// `try_as_contract` will record the references used during the conversion through the `refs_usage` parameter.
    fn try_as_contract(
        &self,
        root_schema: &RootSchema,
        refs_usage: &mut RefsUsage,
    ) -> Option<Contract>;
}

pub trait AsPredicateContract {
    /// Convert a JSON schema to a contract by first converting it to a predicate, and then use
    /// json-schema-to-nickel's `from_predicate` helper. As opposed to [TryAsContract::try_as_contract], this
    /// conversion can't fail. However, it is less desirable (as it throws lazyness out of the
    /// window and is less LSP-friendly for e.g. completion), so we generally try to use
    /// [TryAsContract::try_as_contract] first.
    fn as_predicate_contract(&self, refs_usage: &mut RefsUsage) -> Contract;
}

impl<T> AsPredicateContract for T
where
    T: AsPredicate,
{
    fn as_predicate_contract(&self, refs_usage: &mut RefsUsage) -> Contract {
        Contract::from(self.as_predicate(refs_usage))
    }
}

impl TryAsContract for Schema {
    fn try_as_contract(
        &self,
        root_schema: &RootSchema,
        refs_usage: &mut RefsUsage,
    ) -> Option<Contract> {
        match self {
            Schema::Bool(true) => Some(Contract {
                terms: vec![],
                maybe_null: false,
            }),
            Schema::Bool(false) => None,
            Schema::Object(obj) => obj.try_as_contract(root_schema, refs_usage),
        }
    }
}

impl TryAsContract for SchemaObject {
    fn try_as_contract(
        &self,
        root_schema: &RootSchema,
        refs_usage: &mut RefsUsage,
    ) -> Option<Contract> {
        let instance_type = self.instance_type.as_ref().map(SimpleType::from_types);
        let simple_type = instance_type.as_ref().and_then(SimpleType::inner);

        let ctr = match (self, simple_type) {
            // a raw type
            (
                SchemaObject {
                    metadata: _,
                    instance_type: _,
                    format: _,
                    enum_values: None,
                    const_value: None,
                    subschemas: None,
                    number: None,
                    string: None,
                    array: None,
                    object: None,
                    reference: None,
                    extensions,
                },
                Some(instance_type),
            ) if only_ignored_fields(extensions) => {
                // We ultimately produce a Flat type based on a contract with
                // only a type in it. Semantically, this is kind of weird. But
                // the pretty printer doesn't care, and it simplifies our code
                // significantly.
                Some(Contract::from(instance_type))
            }
            // a reference to a definition
            (
                SchemaObject {
                    metadata: _,
                    instance_type: None,
                    format: None,
                    enum_values: None,
                    const_value: None,
                    subschemas: None,
                    number: None,
                    string: None,
                    array: None,
                    object: None,
                    reference: Some(reference),
                    extensions,
                },
                _,
            ) if only_ignored_fields(extensions) => Some(Contract::from(references::resolve_ref(
                reference,
                refs_usage,
                RefUsageContext::Contract,
            ))),
            // a freeform record
            (
                SchemaObject {
                    metadata: _,
                    instance_type: _,
                    format: None,
                    enum_values: None,
                    const_value: None,
                    subschemas: None,
                    number: None,
                    string: None,
                    array: None,
                    object: None,
                    reference: None,
                    extensions,
                },
                Some(InstanceType::Object),
            ) if only_ignored_fields(extensions) => {
                Some(Contract::from(Term::Record(RecordData {
                    attrs: RecordAttrs {
                        open: true,
                        ..Default::default()
                    },
                    ..Default::default()
                })))
            }
            // a record with sub-field types specified
            (
                SchemaObject {
                    metadata: _,
                    instance_type: _,
                    format: None,
                    enum_values: None,
                    const_value: None,
                    subschemas: None,
                    number: None,
                    string: None,
                    array: None,
                    object: Some(ov),
                    reference: None,
                    extensions,
                },
                Some(InstanceType::Object),
            ) if only_ignored_fields(extensions) => ov.try_as_contract(root_schema, refs_usage),
            // Enum contract with all strings
            // => | std.enum.TagOrString | [| 'foo, 'bar, 'baz |]
            (
                SchemaObject {
                    metadata: _,
                    instance_type: _,
                    format: None,
                    enum_values: Some(values),
                    const_value: None,
                    subschemas: None,
                    number: None,
                    string: None,
                    array: None,
                    object: None,
                    reference: None,
                    extensions: _,
                },
                Some(InstanceType::String),
            ) => {
                let enum_rows: EnumRows =
                    values
                        .iter()
                        .try_fold(EnumRows(EnumRowsF::Empty), |acc, value| {
                            let Value::String(id) = value else {
                                return None;
                            };

                            let row = EnumRowF {
                                id: id.into(),
                                typ: None,
                            };

                            Some(EnumRows(EnumRowsF::Extend {
                                row,
                                tail: Box::new(acc),
                            }))
                        })?;
                Some(Contract::from_terms(vec![
                    static_access("std", ["enum", "TagOrString"]),
                    Term::Type {
                        typ: TypeF::Enum(enum_rows).into(),
                        // We don't care about the contract here, it's a run-time cache thing.
                        contract: Term::Null.into(),
                    }
                    .into(),
                ]))
            }
            (
                SchemaObject {
                    metadata: _,
                    instance_type: _,
                    format: None,
                    enum_values: None,
                    const_value: None,
                    subschemas: None,
                    number: None,
                    string: None,
                    array: Some(av),
                    object: None,
                    reference: None,
                    extensions: _,
                },
                Some(InstanceType::Array),
            ) => av.try_as_contract(root_schema, refs_usage),
            (
                SchemaObject {
                    metadata: _,
                    instance_type: None,
                    format: None,
                    enum_values: None,
                    const_value: None,
                    subschemas: Some(subschemas),
                    number: None,
                    string: None,
                    array: None,
                    object: None,
                    reference: None,
                    extensions,
                },
                None,
            ) if only_ignored_fields(extensions) => {
                subschemas.try_as_contract(root_schema, refs_usage)
            }
            (
                SchemaObject {
                    metadata: _,
                    instance_type: _,
                    format: None,
                    enum_values: None,
                    const_value: None,
                    subschemas: None,
                    number: None,
                    string: Some(string),
                    array: None,
                    object: None,
                    reference: None,
                    extensions,
                },
                Some(InstanceType::String),
            ) if only_ignored_fields(extensions) => string.try_as_contract(root_schema, refs_usage),
            _ => {
                dbg!(self);
                eprintln!("NO MATCH");
                None
            }
        };

        match instance_type {
            Some(SimpleType::Nullable(_)) => Some(ctr?.or_null()),
            _ => ctr,
        }
    }
}

impl TryAsContract for ObjectValidation {
    fn try_as_contract(
        &self,
        root_schema: &RootSchema,
        refs_usage: &mut RefsUsage,
    ) -> Option<Contract> {
        fn is_open_record(additional: Option<&Schema>) -> bool {
            match additional {
                Some(Schema::Bool(open)) => *open,
                None => true,
                _ => unreachable!("additional_properties must be checked beforehand"),
            }
        }

        // box / deref patterns aren't stabilized, so we have to separate out
        // `additional_properties` as a separate pattern
        // SEE: https://github.com/rust-lang/rust/issues/29641
        // SEE: https://github.com/rust-lang/rust/issues/87121
        match (self, self.additional_properties.as_deref()) {
            (
                ObjectValidation {
                    max_properties: None,
                    min_properties: None,
                    required,
                    properties,
                    pattern_properties,
                    additional_properties,
                    property_names: None,
                },
                None | Some(Schema::Bool(_)),
            ) if pattern_properties.is_empty() => Some(Contract::from(generate_record_contract(
                required,
                properties,
                is_open_record(additional_properties.as_deref()),
                root_schema,
                refs_usage,
            ))),
            (
                ObjectValidation {
                    max_properties,
                    min_properties,
                    required,
                    properties,
                    pattern_properties,
                    additional_properties: _,
                    property_names: None,
                },
                Some(Schema::Bool(false)),
            ) if required.is_empty() && properties.is_empty() && pattern_properties.len() == 1 => {
                // unwrap: we just checked that there's one element
                let (regex, schema) = pattern_properties.iter().next().unwrap();
                Some(generate_dict_contract(
                    regex,
                    schema,
                    *min_properties,
                    *max_properties,
                    root_schema,
                    refs_usage,
                ))
            }
            _ => None,
        }
    }
}

fn generate_dict_contract(
    regex: &str,
    schema: &Schema,
    min_properties: Option<u32>,
    max_properties: Option<u32>,
    root_schema: &RootSchema,
    refs_usage: &mut RefsUsage,
) -> Contract {
    let elt_contract = schema
        .try_as_contract(root_schema, refs_usage)
        .unwrap_or_else(|| schema.as_predicate_contract(refs_usage));
    let elt_contract = RichTerm::from(elt_contract);
    let dict_schema = Term::Type {
        typ: TypeF::Dict {
            type_fields: Box::new(TypeF::Contract(elt_contract).into()),
            flavour: nickel_lang_core::typ::DictTypeFlavour::Contract,
        }
        .into(),
        contract: Term::Null.into(),
    };

    let patterns = mk_app!(
        static_access(
            PREDICATES_LIBRARY_ID,
            ["records", "contract", "patternFields"]
        ),
        Term::Str(regex.to_owned().into())
    );

    let min_contract = min_properties.map(|n| {
        mk_app!(
            static_access(
                PREDICATES_LIBRARY_ID,
                ["records", "contract", "minProperties"]
            ),
            Term::Num(n.into())
        )
    });
    let max_contract = max_properties.map(|n| {
        mk_app!(
            static_access(
                PREDICATES_LIBRARY_ID,
                ["records", "contract", "maxProperties"]
            ),
            Term::Num(n.into())
        )
    });

    Contract::from_terms(
        std::iter::once(dict_schema.into())
            .chain(Some(patterns))
            .chain(min_contract)
            .chain(max_contract)
            .collect(),
    )
}

impl TryAsContract for ArrayValidation {
    fn try_as_contract(
        &self,
        root_schema: &RootSchema,
        refs_usage: &mut RefsUsage,
    ) -> Option<Contract> {
        if let ArrayValidation {
            items: Some(SingleOrVec::Single(s)),
            additional_items: None,
            max_items,
            min_items,
            unique_items: None,
            contains: None,
        } = self
        {
            let elt = s
                .try_as_contract(root_schema, refs_usage)
                .unwrap_or_else(|| s.as_predicate_contract(refs_usage));
            let elt = RichTerm::from(elt);

            let min_ctr = min_items.map(|n| {
                if n == 1 {
                    static_access("std", ["array", "NonEmpty"])
                } else {
                    mk_app!(
                        static_access(PREDICATES_LIBRARY_ID, ["array", "contract", "minItems"]),
                        Term::Num(n.into())
                    )
                }
            });

            let max_ctr = max_items.map(|n| {
                mk_app!(
                    static_access(PREDICATES_LIBRARY_ID, ["array", "contract", "maxItems"]),
                    Term::Num(n.into())
                )
            });

            let terms: Vec<_> = min_ctr
                .into_iter()
                .chain(max_ctr)
                .chain(Some(
                    Term::Type {
                        typ: TypeF::Array(Box::new(TypeF::Contract(elt.clone()).into())).into(),
                        contract: Term::Null.into(),
                    }
                    .into(),
                ))
                .collect();

            Some(Contract::from_terms(terms))
        } else {
            None
        }
    }
}

impl TryAsContract for SubschemaValidation {
    fn try_as_contract(
        &self,
        root_schema: &RootSchema,
        refs_usage: &mut RefsUsage,
    ) -> Option<Contract> {
        match self {
            SubschemaValidation {
                all_of: Some(all_of),
                any_of: None,
                one_of: None,
                not: None,
                if_schema: None,
                then_schema: None,
                else_schema: None,
            } => all_of
                .iter()
                .map(|x| {
                    x.try_as_contract(root_schema, refs_usage)
                        .map(RichTerm::from)
                })
                .collect::<Option<Vec<_>>>()
                .map(Contract::from_terms),

            // If the various options in an any_of combinator all have eagerly-distinguishable
            // types then we can safely turn it into a lazy contract. The same goes for one_of,
            // because the types are all different and so one_of is effectively any_of.
            SubschemaValidation {
                all_of: None,
                any_of: Some(any_of),
                one_of: None,
                not: None,
                if_schema: None,
                then_schema: None,
                else_schema: None,
            }
            | SubschemaValidation {
                all_of: None,
                any_of: None,
                one_of: Some(any_of),
                not: None,
                if_schema: None,
                then_schema: None,
                else_schema: None,
            } => {
                let types = any_of
                    .iter()
                    .map(|s| {
                        schema_types(s, root_schema)
                            .and_then(|tys| SimpleType::from_types(&tys).inner().copied())
                    })
                    .collect::<Vec<_>>();

                // As long as all of the types are simple and distinct, we can
                // make a lazy union contract. There's one corner case where
                // this is technically not correct: if it's a `oneOf` and more
                // than one of the types is nullable then we should actually
                // forbid null. This case is most likely a bug in the contract,
                // though; probably they meant `anyOf`.
                if types.iter().all(|opt| opt.is_some()) && distinct(types.into_iter()) {
                    let ctrs = any_of.iter().map(|x| {
                        x.try_as_contract(root_schema, refs_usage)
                            .unwrap_or_else(|| x.as_predicate_contract(refs_usage))
                    });

                    let arr = Term::Array(ctrs.map(RichTerm::from).collect(), Default::default());
                    let any = mk_app!(static_access("std", ["contract", "any_of"]), arr);
                    Some(Contract::from_terms(vec![any]))
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}

impl TryAsContract for StringValidation {
    fn try_as_contract(
        &self,
        _root_schema: &RootSchema,
        _refs_usage: &mut RefsUsage,
    ) -> Option<Contract> {
        let pattern = self.pattern.as_ref().map(|s| {
            mk_app!(
                static_access(PREDICATES_LIBRARY_ID, ["strings", "contract", "Pattern"]),
                Term::Str(s.to_owned().into())
            )
        });
        let min_length = self.min_length.map(|len| {
            mk_app!(
                static_access(PREDICATES_LIBRARY_ID, ["strings", "contract", "MinLength"]),
                Term::Num(len.into())
            )
        });
        let max_length = self.min_length.map(|len| {
            mk_app!(
                static_access(PREDICATES_LIBRARY_ID, ["strings", "contract", "MaxLength"]),
                Term::Num(len.into())
            )
        });

        Some(Contract::from_terms(
            pattern
                .into_iter()
                .chain(min_length)
                .chain(max_length)
                .collect(),
        ))
    }
}

// The following conversions:
//
// 1. Are infallible
// 2. Don't do reference resolution
//
// We implement `From` directly instead of `TryConvert`.

impl From<RichTerm> for Contract {
    fn from(rt: RichTerm) -> Self {
        Contract::from_terms(vec![rt])
    }
}

impl From<Contract> for RichTerm {
    fn from(c: Contract) -> Self {
        let ret = match c.terms.as_slice() {
            [] => static_access(PREDICATES_LIBRARY_ID, ["contract", "always"]),
            // TODO: shouldn't need to clone here
            [rt] => rt.clone(),
            _ => {
                let arr = Term::Array(c.terms.into_iter().collect(), Default::default());
                mk_app!(static_access("std", ["contract", "Sequence"]), arr)
            }
        };
        if c.maybe_null {
            mk_app!(
                static_access(PREDICATES_LIBRARY_ID, ["contract", "or_null"]),
                ret
            )
        } else {
            ret
        }
    }
}

impl From<Term> for Contract {
    fn from(value: Term) -> Self {
        Contract::from(RichTerm::from(value))
    }
}

impl From<TypeF<Box<Type>, RecordRows, EnumRows, RichTerm>> for Contract {
    fn from(value: TypeF<Box<Type>, RecordRows, EnumRows, RichTerm>) -> Self {
        Contract::from(Term::Type {
            typ: Type::from(value),
            // We don't care about the contract here, it's a run-time cache thing.
            contract: Term::Null.into(),
        })
    }
}

impl From<&InstanceType> for Contract {
    fn from(value: &InstanceType) -> Contract {
        match value {
            InstanceType::Null => Contract::from(Predicate::from(mk_app!(
                static_access(PREDICATES_LIBRARY_ID, ["isType"]),
                Term::Enum("Null".into())
            ))),
            InstanceType::Boolean => Contract::from(TypeF::Bool),
            InstanceType::Object => Contract::from(Term::Record(RecordData {
                attrs: RecordAttrs {
                    open: true,
                    ..Default::default()
                },
                ..Default::default()
            })),
            InstanceType::Array => Contract::from(TypeF::Array(Box::new(TypeF::Dyn.into()))),
            InstanceType::Number => Contract::from(TypeF::Number),
            InstanceType::String => Contract::from(TypeF::String),
            InstanceType::Integer => Contract::from(static_access("std", ["number", "Integer"])),
        }
    }
}

impl From<Contract> for TypeAnnotation {
    fn from(c: Contract) -> Self {
        TypeAnnotation {
            typ: None,
            contracts: vec![LabeledType {
                typ: TypeF::Contract(c.into()).into(),
                label: Label::dummy(),
            }],
        }
    }
}

#[derive(Clone)]
pub struct Documentation(String);

impl From<Documentation> for String {
    fn from(value: Documentation) -> Self {
        value.0
    }
}

impl TryFrom<&SchemaObject> for Documentation {
    type Error = ();

    fn try_from(value: &SchemaObject) -> Result<Self, Self::Error> {
        match value {
            SchemaObject {
                metadata: Some(metadata),
                ..
            } => metadata.description.clone().map(Documentation).ok_or(()),
            _ => Err(()),
        }
    }
}

impl TryFrom<&Schema> for Documentation {
    type Error = ();

    fn try_from(value: &Schema) -> Result<Self, Self::Error> {
        match value {
            Schema::Bool(_) => Err(()),
            Schema::Object(obj) => obj.try_into(),
        }
    }
}

fn generate_record_contract(
    required: &BTreeSet<String>,
    properties: &BTreeMap<String, Schema>,
    open: bool,
    root_schema: &RootSchema,
    refs_usage: &mut RefsUsage,
) -> RichTerm {
    let fields = properties.iter().map(|(name, schema)| {
        // try to convert to a contract, otherwise convert the predicate version
        // to a contract
        let contract = schema
            .try_as_contract(root_schema, refs_usage)
            .unwrap_or_else(|| schema.as_predicate_contract(refs_usage));
        let doc = Documentation::try_from(schema).ok();
        (
            name.into(),
            Field {
                metadata: FieldMetadata {
                    annotation: contract.into(),
                    opt: !required.contains(name),
                    doc: doc.map(String::from),
                    ..Default::default()
                },
                ..Default::default()
            },
        )
    });
    Term::Record(RecordData {
        fields: fields.collect(),
        attrs: RecordAttrs {
            open,
            ..Default::default()
        },
        ..Default::default()
    })
    .into()
}

impl From<Predicate> for Contract {
    // Convert a predicate to a contract by calling a function similar to
    // `std.contract.from_predicate` (but which does a bit more about propagating meaningful error
    // messages)
    fn from(pred: Predicate) -> Self {
        mk_app!(
            static_access(PREDICATES_LIBRARY_ID, ["contract_from_predicate"]),
            pred
        )
        .into()
    }
}
