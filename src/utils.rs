use std::collections::HashSet;

use nickel_lang_core::{
    bytecode::ast::{primop::PrimOp, typ::TypeUnr, Ast, AstAlloc, Node},
    identifier::LocIdent,
};

pub fn static_access<I, S>(alloc: &AstAlloc, record: S, fields: I) -> Ast<'_>
where
    I: IntoIterator<Item = S>,
    I::IntoIter: DoubleEndedIterator,
    S: Into<LocIdent>,
{
    let mut ast = Node::Var(record.into()).into();
    for f in fields.into_iter() {
        ast = alloc
            .prim_op(PrimOp::RecordStatAccess(f.into()), [ast])
            .into();
    }
    ast
}

/// Replace special escaping sequences by the actual character within one element of a JSON pointer
/// path. See the [JSON pointer syntax](https://datatracker.ietf.org/doc/html/rfc6901#section-3).
/// Currently, this just amounts to replace `~0` by `~` and `~1` by `/`.
pub fn decode_json_ptr_part(part: &str) -> String {
    part.replace("~0", "~").replace("~1", "/")
}

/// Returns `true` if all the items in `items` are distinct.
pub fn distinct<T: std::hash::Hash + Eq>(items: impl Iterator<Item = T>) -> bool {
    let mut seen = HashSet::new();
    for item in items {
        if !seen.insert(item) {
            return false;
        }
    }
    true
}

/// Creates a contract that for checking a Nickel type.
pub fn type_contract<'a>(alloc: &'a AstAlloc, ty: TypeUnr<'a>) -> Ast<'a> {
    alloc.typ(ty.into()).into()
}

/// Turns a collection of contracts into a single contract.
pub fn sequence<'a>(alloc: &'a AstAlloc, mut contracts: Vec<Ast<'a>>) -> Ast<'a> {
    if contracts.len() == 1 {
        contracts.pop().unwrap()
    } else {
        alloc
            .app(
                static_access(alloc, "std", ["contract", "Sequence"]),
                [alloc.array(contracts).into()],
            )
            .into()
    }
}
