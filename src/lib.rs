use nickel_lang::{
    identifier::Ident,
    mk_app, mk_fun,
    term::{make, BinaryOp, Term, UnaryOp},
    types::{RecordRows, RecordRowsF, TypeF, Types},
};
use schemars::schema::{InstanceType, Schema, SchemaObject, SingleOrVec};

fn instance_to_types(instance: InstanceType) -> Types {
    match instance {
        InstanceType::Number => Types(TypeF::Num),
        InstanceType::Boolean => Types(TypeF::Bool),
        InstanceType::String => Types(TypeF::Str),
        InstanceType::Integer => Types(TypeF::Flat(make::op1(
            UnaryOp::StaticAccess(Ident::new("Int")),
            make::var("num"),
        ))),
        InstanceType::Null => Types(TypeF::Flat(mk_fun!(
            "label",
            "value",
            mk_app!(
                make::op1(
                    UnaryOp::Ite(),
                    make::op2(BinaryOp::Eq(), make::var("value"), Term::Null)
                ),
                make::var("value"),
                mk_app!(
                    make::op1(
                        UnaryOp::StaticAccess(Ident::new("blame_with")),
                        make::var("contract"),
                    ),
                    make::string("expected null"),
                    make::var("label")
                )
            )
        ))),
        InstanceType::Object => Types(TypeF::Record(RecordRows(RecordRowsF::TailDyn))),
        InstanceType::Array => Types(TypeF::Array(Box::new(Types(TypeF::Dyn)))),
    }
}

pub fn schema_to_types(schema: Schema) -> Types {
    let schema = match schema {
        Schema::Bool(true) => return Types(TypeF::Dyn),
        Schema::Bool(false) => {
            return Types(TypeF::Flat(mk_fun!(
                "label",
                "value",
                mk_app!(
                    make::op1(
                        UnaryOp::StaticAccess(Ident::new("blame_with")),
                        make::var("contract")
                    ),
                    make::string("Never contract evaluated"),
                    make::var("label")
                )
            )))
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
        } => instance_to_types(*instance),

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
        } if instances.len() == 1 => instance_to_types(instances[0]),

        _ => todo!(),
    }
}
