//! Object schemas
//!
//! The schemas in the module correspond fairly closely to the ones documented
//! [here](https://json-schema.org/draft-07/draft-handrews-json-schema-validation-01#rfc.section.6.5)

use std::collections::{BTreeMap, BTreeSet};

use nickel_lang_core::{
    identifier::LocIdent,
    label::Label,
    mk_app,
    term::{
        array::ArrayAttrs,
        make,
        record::{Field, FieldMetadata, RecordAttrs, RecordData},
        LabeledType, Number, RichTerm, Term, TypeAnnotation,
    },
    typ::{DictTypeFlavour, TypeF},
};
use serde::Serialize;

use crate::{
    contract::ContractContext,
    schema::Schema,
    utils::{num, sequence, type_contract},
};

/// A schema for validating objects.
///
/// Unlike in JSON Schema, all of these schemas assert that the value is an
/// object; in JSON Schema, the object validation keywords all assert that *if*
/// the value is an object then it satisfies some additional properties.
#[derive(Clone, Debug, Serialize, PartialEq, Eq, Hash)]
pub enum Obj {
    /// Asserts that the value is an object, and nothing else.
    Any,
    /// Validates the object's properties against sub-schemas.
    Properties(ObjectProperties),
    /// Asserts an upper bound on the number of properties.
    MaxProperties(Number),
    /// Asserts a lower bound on the number of properties.
    MinProperties(Number),
    /// Asserts that the object contains certain properties.
    ///
    /// TODO: maybe we can get rid of this by representing it with `Properties`
    Required(BTreeSet<String>),
    /// Asserts that the names of all properties in the object satisfy
    /// a schema.
    ///
    /// This could be simplified maybe, because we're guaranteed that this will
    /// only need to validate strings.
    PropertyNames(Box<Schema>),
    /// Asserts that if some properties are present then other properties
    /// are also present.
    ///
    /// Specifically, if the map contains `(key, values)` and the object
    /// contains the property named `key` then it must also contain all
    /// the properties whose names are in `values`.
    ///
    /// TODO: consider simplifying the representation to contain only a
    /// single `(key, values)` pair instead of a map. The general case can be
    /// represented by wrapping the simplified cases in an `AllOf`.
    DependentFields(BTreeMap<String, Vec<String>>),
    /// Asserts that if some properties are present then the entire object
    /// satisfies some schema.
    ///
    /// Specifically, if the map contains `(key, schema)` and the object
    /// contains the property named `key` then it must also satisfy `schema`.
    ///
    /// TODO: consider the same simplification as `DependentFields`.
    DependentSchemas(BTreeMap<String, Schema>),
}

/// The value in a JSON Schema "properties" object.
///
/// This would be most faithfully represented as just a `Schema`, but we
/// track some additional data.
#[derive(Clone, Debug, Serialize, PartialEq, Eq, Hash)]
pub struct Property {
    /// A description of this property.
    pub doc: Option<String>,
    /// The schema that applies to the property's value.
    pub schema: Schema,
    /// Whether the property is optional.
    ///
    /// In JSON Schema "properties", all properties are optional; non-optional
    /// properties are handled by the orthogonal "required" schema. To generate
    /// better Nickel contracts, we support explicit optionality here. This is
    /// always `true` when we first convert from JSON Schema, but transformations
    /// might set it to `false`.
    pub optional: bool,
}

impl From<Schema> for Property {
    fn from(s: Schema) -> Self {
        Property {
            doc: None,
            schema: s,
            optional: true,
        }
    }
}

/// Schemas for validating properties of an object.
///
/// This is a combination of the "properties", "patternProperties", and
/// "additionalProperties" keywords in JSON Schema.
#[derive(Clone, Debug, Serialize, PartialEq, Eq, Hash)]
pub struct ObjectProperties {
    /// Schemas for named properties of this object.
    pub properties: BTreeMap<String, Property>,
    /// Schemas for properties of this object whose names match regular
    /// expressions.
    ///
    /// A property can get matched by more than one regular expression; all of
    /// the matching schemas will be checked. Similarly, a property that already
    /// matched a name in `properties` can get matched by a pattern here.
    pub pattern_properties: BTreeMap<String, Schema>,
    /// A schema that's applied to all properties of this object that
    /// were not matched by any names in `properties` or any patterns in
    /// `pattern_properties`.
    pub additional_properties: Option<Box<Schema>>,
}

impl Obj {
    /// Converts this object schema to a Nickel contract.
    ///
    /// Can return multiple contracts that should be applied in sequence.
    pub fn to_contract(&self, ctx: ContractContext) -> Vec<RichTerm> {
        match self {
            Obj::Any => vec![type_contract(TypeF::Dict {
                type_fields: Box::new(TypeF::Dyn.into()),
                flavour: nickel_lang_core::typ::DictTypeFlavour::Contract,
            })],
            Obj::Properties(op) => op.to_special_contract(ctx).unwrap_or_else(|| {
                let additional = match op.additional_properties.as_deref() {
                    Some(Schema::Never) => Term::Enum("None".into()),
                    Some(s) => Term::EnumVariant {
                        tag: "Some".into(),
                        arg: sequence(s.to_contract(ctx)),
                        attrs: Default::default(),
                    },
                    None => Term::EnumVariant {
                        tag: "Some".into(),
                        arg: type_contract(TypeF::Dyn),
                        attrs: Default::default(),
                    },
                };
                let properties = Term::Record(RecordData::with_field_values(
                    op.properties
                        .iter()
                        .map(|(k, v)| (k.into(), sequence(v.schema.to_contract(ctx)))),
                ));
                let required = Term::Record(RecordData::with_field_values(
                    op.properties.iter().filter_map(|(k, v)| {
                        if !v.optional {
                            Some((k.into(), Term::Bool(true).into()))
                        } else {
                            None
                        }
                    }),
                ));
                let patterns = Term::Record(RecordData::with_field_values(
                    op.pattern_properties
                        .iter()
                        .map(|(k, v)| (k.into(), sequence(v.to_contract(ctx)))),
                ));

                vec![mk_app!(
                    ctx.js2n("record.Record"),
                    Term::Record(RecordData::with_field_values([
                        ("properties".into(), properties.into()),
                        ("required".into(), required.into()),
                        ("patterns".into(), patterns.into()),
                        ("additional".into(), additional.into())
                    ]))
                )]
            }),
            Obj::MaxProperties(n) => {
                vec![mk_app!(ctx.js2n("record.MaxProperties"), num(n))]
            }
            Obj::MinProperties(n) => {
                vec![mk_app!(ctx.js2n("record.MinProperties"), num(n))]
            }
            Obj::Required(names) => {
                vec![mk_app!(
                    ctx.js2n("record.Required"),
                    Term::Array(
                        names.iter().map(|s| Term::Str(s.into()).into()).collect(),
                        ArrayAttrs::default()
                    )
                )]
            }
            Obj::PropertyNames(schema) => {
                vec![mk_app!(
                    ctx.js2n("record.PropertyNames"),
                    sequence(schema.to_contract(ctx))
                )]
            }
            Obj::DependentFields(deps) => {
                vec![mk_app!(
                    ctx.js2n("record.DependentFields"),
                    Term::Record(RecordData::with_field_values(deps.iter().map(
                        |(key, value)| {
                            let arr = Term::Array(
                                value.iter().map(make::string).collect(),
                                Default::default(),
                            )
                            .into();
                            (LocIdent::from(key), arr)
                        }
                    )))
                )]
            }
            Obj::DependentSchemas(deps) => {
                vec![mk_app!(
                    ctx.js2n("record.DependentContracts"),
                    Term::Record(RecordData::with_field_values(deps.iter().map(
                        |(key, schema)| {
                            let contract = sequence(schema.to_contract(ctx));
                            (LocIdent::from(key), contract)
                        }
                    )))
                )]
            }
        }
    }
}

impl ObjectProperties {
    /// Attempts to produce idiomatic Nickel record contracts from this schema.
    ///
    /// Our js2n contracts library contains functions for creating Nickel contracts
    /// from arbitrary `ObjectProperties`, but the contracts it creates are not
    /// record contracts and so they miss things like field documentation. This
    /// method attempts to create a better contract, but it's allowed to fail.
    pub fn to_special_contract(&self, ctx: ContractContext) -> Option<Vec<RichTerm>> {
        let trivial_additional = matches!(
            self.additional_properties.as_deref(),
            None | Some(Schema::Always) | Some(Schema::Never)
        );
        let no_additional = self.additional_properties.as_deref() == Some(&Schema::Never);
        if self.pattern_properties.is_empty() && trivial_additional && !ctx.is_eager() {
            // A normal record contract, which may or may not be open. If all the element contracts
            // are trivial, this will even be an eager contract.
            let open = matches!(
                self.additional_properties.as_deref(),
                None | Some(Schema::Always)
            );

            let fields = self.properties.iter().map(|(name, prop)| {
                (
                    name.into(),
                    Field {
                        metadata: FieldMetadata {
                            annotation: TypeAnnotation {
                                typ: None,
                                contracts: prop
                                    .schema
                                    .to_contract(ctx)
                                    .into_iter()
                                    .map(|c| LabeledType {
                                        typ: TypeF::Contract(c).into(),
                                        label: Label::dummy(),
                                    })
                                    .collect(),
                            },
                            opt: prop.optional,
                            doc: prop.doc.clone(),
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                )
            });

            Some(vec![Term::Record(RecordData {
                fields: fields.collect(),
                attrs: RecordAttrs {
                    open,
                    ..Default::default()
                },
                ..Default::default()
            })
            .into()])
        } else if self.properties.is_empty()
            && self.pattern_properties.is_empty()
            && (!ctx.is_eager() || trivial_additional)
        {
            // No properties were specified, just a contract on the additional
            // properties. We mostly treat this as a dict, but if additional
            // properties are forbidden we treat it as an empty record.
            if no_additional {
                Some(vec![Term::Record(RecordData::default()).into()])
            } else {
                let additional_properties = self
                    .additional_properties
                    .as_deref()
                    .unwrap_or(&Schema::Always);
                Some(vec![dict_contract(additional_properties, ctx)])
            }
        } else if self.properties.is_empty()
            && self.pattern_properties.len() == 1
            && no_additional
            && (!ctx.is_eager()
                || self
                    .pattern_properties
                    .values()
                    .all(|s| s == &Schema::Always))
        {
            // unwrap: we checked for length 1
            let (pattern, schema) = self.pattern_properties.iter().next().unwrap();
            let dict = dict_contract(schema, ctx);
            let names = mk_app!(
                ctx.std("record.FieldsMatch"),
                Term::Str(pattern.to_owned().into())
            );
            Some(vec![dict, names])
        } else {
            None
        }
    }
}

fn dict_contract(elt_schema: &Schema, ctx: ContractContext) -> RichTerm {
    let elt_contract = elt_schema.to_contract(ctx);

    type_contract(TypeF::Dict {
        type_fields: Box::new(TypeF::Contract(sequence(elt_contract)).into()),
        flavour: DictTypeFlavour::Contract,
    })
}
