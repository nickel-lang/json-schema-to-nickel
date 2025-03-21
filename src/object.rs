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

#[derive(Clone, Debug, Serialize, PartialEq, Eq, Hash)]
pub enum Obj {
    Any,
    Properties(ObjectProperties),
    MaxProperties(Number),
    MinProperties(Number),
    Required(BTreeSet<String>),
    // This could be simplified maybe, because we're guaranteed that this will only need to validate strings.
    PropertyNames(Box<Schema>),
    DependentFields(BTreeMap<String, Vec<String>>),
    DependentSchemas(BTreeMap<String, Schema>),
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq, Hash)]
pub struct Property {
    pub doc: Option<String>,
    pub schema: Schema,
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

#[derive(Clone, Debug, Serialize, PartialEq, Eq, Hash)]
pub struct ObjectProperties {
    pub properties: BTreeMap<String, Property>,
    pub pattern_properties: BTreeMap<String, Schema>,
    pub additional_properties: Option<Box<Schema>>,
}

impl Obj {
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
                ctx.js2n("record.FieldsMatch"),
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
