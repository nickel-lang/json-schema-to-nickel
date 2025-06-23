//! Object schemas
//!
//! The schemas in the module correspond fairly closely to the ones documented
//! [here](https://json-schema.org/draft-07/draft-handrews-json-schema-validation-01#rfc.section.6.5)

use std::collections::{BTreeMap, BTreeSet};

use nickel_lang_core::{
    bytecode::ast::{builder, record::FieldMetadata, Ast, Node},
    term::Number,
    typ::{DictTypeFlavour, TypeF},
};
use serde::Serialize;

use crate::{contract::ContractContext, schema::Schema, utils::type_contract};

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
    pub fn to_contract<'ast>(&self, ctx: ContractContext<'_, 'ast, '_>) -> Vec<Ast<'ast>> {
        match self {
            Obj::Any => vec![type_contract(
                ctx.alloc(),
                TypeF::Dict {
                    type_fields: ctx.alloc().type_data(TypeF::Dyn, Default::default()),
                    flavour: nickel_lang_core::typ::DictTypeFlavour::Contract,
                },
            )],
            Obj::Properties(op) => op.to_special_contract(ctx).unwrap_or_else(|| {
                let additional = match op.additional_properties.as_deref() {
                    Some(Schema::Never) => Node::EnumVariant {
                        tag: "None".into(),
                        arg: None,
                    },
                    Some(s) => Node::EnumVariant {
                        tag: "Some".into(),
                        arg: Some(ctx.alloc().alloc(ctx.sequence(s.to_contract(ctx)))),
                    },
                    None => Node::EnumVariant {
                        tag: "Some".into(),
                        arg: Some(ctx.alloc().alloc(type_contract(ctx.alloc(), TypeF::Dyn))),
                    },
                };
                let properties = builder::Record::new()
                    .fields(
                        ctx.alloc(),
                        op.properties.iter().map(|(k, v)| {
                            builder::Field::name(k).value(ctx.sequence(v.schema.to_contract(ctx)))
                        }),
                    )
                    .build(ctx.alloc());
                let required = builder::Record::new()
                    .fields(
                        ctx.alloc(),
                        op.properties.iter().filter_map(|(k, v)| {
                            if !v.optional {
                                Some(builder::Field::name(k).value(Node::Bool(true)))
                            } else {
                                None
                            }
                        }),
                    )
                    .build(ctx.alloc());
                let patterns = builder::Record::new()
                    .fields(
                        ctx.alloc(),
                        op.pattern_properties.iter().map(|(k, v)| {
                            builder::Field::name(k).value(ctx.sequence(v.to_contract(ctx)))
                        }),
                    )
                    .build(ctx.alloc());

                vec![ctx
                    .alloc()
                    .app(
                        ctx.js2n("record.Record"),
                        [builder::Record::new()
                            .fields(
                                ctx.alloc(),
                                [
                                    builder::Field::name("properties").value(properties),
                                    builder::Field::name("required").value(required),
                                    builder::Field::name("patterns").value(patterns),
                                    builder::Field::name("additional").value(additional),
                                ],
                            )
                            .build(ctx.alloc())],
                    )
                    .into()]
            }),
            Obj::MaxProperties(n) => {
                vec![ctx
                    .alloc()
                    .app(ctx.js2n("record.MaxProperties"), [ctx.num(n)])
                    .into()]
            }
            Obj::MinProperties(n) => {
                vec![ctx
                    .alloc()
                    .app(ctx.js2n("record.MinProperties"), [ctx.num(n)])
                    .into()]
            }
            Obj::Required(names) => {
                vec![ctx
                    .alloc()
                    .app(
                        ctx.js2n("record.Required"),
                        [ctx.alloc()
                            .array(names.iter().map(|s| ctx.alloc().string(s).into()))
                            .into()],
                    )
                    .into()]
            }
            Obj::PropertyNames(schema) => {
                vec![ctx
                    .alloc()
                    .app(
                        ctx.js2n("record.PropertyNames"),
                        [ctx.sequence(schema.to_contract(ctx))],
                    )
                    .into()]
            }
            Obj::DependentFields(deps) => {
                vec![ctx
                    .alloc()
                    .app(
                        ctx.js2n("record.DependentFields"),
                        [builder::Record::new()
                            .fields(
                                ctx.alloc(),
                                deps.iter().map(|(key, value)| {
                                    let arr = ctx
                                        .alloc()
                                        .array(value.iter().map(|s| ctx.alloc().string(s).into()));
                                    builder::Field::name(key).value(arr)
                                }),
                            )
                            .build(ctx.alloc())],
                    )
                    .into()]
            }
            Obj::DependentSchemas(deps) => {
                vec![ctx
                    .alloc()
                    .app(
                        ctx.js2n("record.DependentContracts"),
                        [builder::Record::new()
                            .fields(
                                ctx.alloc(),
                                deps.iter().map(|(key, schema)| {
                                    let contract = ctx.sequence(schema.to_contract(ctx));
                                    builder::Field::name(key).value(contract)
                                }),
                            )
                            .build(ctx.alloc())],
                    )
                    .into()]
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
    pub fn to_special_contract<'ast>(
        &self,
        ctx: ContractContext<'_, 'ast, '_>,
    ) -> Option<Vec<Ast<'ast>>> {
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
                let contracts = prop
                    .schema
                    .to_contract(ctx)
                    .into_iter()
                    .map(|c| TypeF::Contract(ctx.alloc().alloc(c)).into());
                builder::Field::name(name)
                    .metadata(FieldMetadata {
                        doc: prop.doc.as_ref().map(|s| ctx.alloc().alloc_str(s)),
                        opt: prop.optional,
                        ..Default::default()
                    })
                    .contracts(contracts)
                    .no_value()
            });

            Some(vec![builder::Record::new()
                .fields(ctx.alloc(), fields)
                .set_open(open)
                .build(ctx.alloc())])
        } else if self.properties.is_empty()
            && self.pattern_properties.is_empty()
            && (!ctx.is_eager() || trivial_additional)
        {
            // No properties were specified, just a contract on the additional
            // properties. We mostly treat this as a dict, but if additional
            // properties are forbidden we treat it as an empty record.
            if no_additional {
                Some(vec![builder::Record::new().build(ctx.alloc())])
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
            let names = ctx.alloc().app(
                ctx.std("record.FieldsMatch"),
                [ctx.alloc().string(pattern).into()],
            );
            Some(vec![dict, names.into()])
        } else {
            None
        }
    }
}

fn dict_contract<'ast>(elt_schema: &Schema, ctx: ContractContext<'_, 'ast, '_>) -> Ast<'ast> {
    let elt_contract = elt_schema.to_contract(ctx);

    type_contract(
        ctx.alloc(),
        TypeF::Dict {
            type_fields: ctx.alloc().type_data(
                TypeF::Contract(ctx.alloc().alloc(ctx.sequence(elt_contract))),
                Default::default(),
            ),
            flavour: DictTypeFlavour::Contract,
        },
    )
}
