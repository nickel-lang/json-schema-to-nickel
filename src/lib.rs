use nickel_lang::{
    identifier::Ident,
    mk_app, mk_fun,
    position::TermPos,
    term::{
        make,
        record::{RecordAttrs, RecordData},
        BinaryOp, RichTerm, Term, UnaryOp,
    },
    types::{EnumRows, RecordRows, RecordRowsF, TypeF, Types},
};
use schemars::schema::{InstanceType, Schema, SchemaObject, SingleOrVec};

fn types(t: TypeF<Box<Types>, RecordRows, EnumRows>) -> Types {
    Types {
        types: t,
        pos: TermPos::None,
    }
}

fn instance_to_contract(instance: InstanceType) -> RichTerm {
    match instance {
        InstanceType::Number => Term::Var("Number".into()).into(),
        InstanceType::Boolean => Term::Var("Bool".into()).into(),
        InstanceType::String => Term::Var("String".into()).into(),
        InstanceType::Integer => Term::Var("std.number.Integer".into()).into(),
        InstanceType::Null => mk_fun!(
            "label",
            "value",
            mk_app!(
                make::op1(
                    UnaryOp::Ite(),
                    make::op2(BinaryOp::Eq(), make::var("value"), Term::Null)
                ),
                make::var("value"),
                mk_app!(
                    Term::Var("std.contract.blame_with".into()),
                    make::string("expected null"),
                    make::var("label")
                )
            )
        ),
        InstanceType::Object => Term::Record(RecordData {
            attrs: RecordAttrs { open: true },
            ..RecordData::empty()
        })
        .into(),
        InstanceType::Array => mk_app!(Term::Var("Array".into()), Term::Var("Dyn".into())),
    }
}

pub fn schema_to_contract(schema: Schema) -> RichTerm {
    let schema = match schema {
        Schema::Bool(true) => return Term::Var("Dyn".into()).into(),
        Schema::Bool(false) => {
            return mk_fun!(
                "label",
                "value",
                mk_app!(
                    Term::Var("std.contract.blame_with".into()),
                    make::string("Never contract evaluated"),
                    make::var("label")
                )
            )
        }
        Schema::Object(s) => s,
    };

    match schema {
        // Easy case, single type, nothing else
        SchemaObject {
            metadata: _,
            instance_type: Some(SingleOrVec::Single(instance)),
            format: None,
            enum_values: None,
            const_value: None,
            subschemas: None,
            number: None,
            string: None,
            array: None,
            object: None,
            reference: None,
            extensions: _,
        } => instance_to_contract(*instance),

        // Similar easy case, single type inside vec, nothing else
        SchemaObject {
            metadata: _,
            instance_type: Some(SingleOrVec::Vec(instances)),
            format: None,
            enum_values: None,
            const_value: None,
            subschemas: None,
            number: None,
            string: None,
            array: None,
            object: None,
            reference: None,
            extensions: _,
        } if instances.len() == 1 => instance_to_contract(instances[0]),

        _ => todo!(),
    }
}
