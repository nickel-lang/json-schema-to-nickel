use std::collections::HashSet;

use nickel_lang_core::{
    identifier::LocIdent,
    mk_app,
    term::{array::ArrayAttrs, make, Number, RichTerm, Term},
    typ::{EnumRows, RecordRows, Type, TypeF},
};

pub fn static_access<I, S>(record: S, fields: I) -> RichTerm
where
    I: IntoIterator<Item = S>,
    I::IntoIter: DoubleEndedIterator,
    S: Into<LocIdent>,
{
    make::static_access(make::var(record), fields)
}

/// Replace special escaping sequences by the actual character within one element of a JSON pointer
/// path. See the [JSON pointer syntax](https://datatracker.ietf.org/doc/html/rfc6901#section-3).
/// Currently, this just amounts to replace `~0` by `~` and `~1` by `/`.
pub fn decode_json_ptr_part(part: &str) -> String {
    part.replace("~0", "~").replace("~1", "/")
}

pub fn distinct<T: std::hash::Hash + Eq>(items: impl Iterator<Item = T>) -> bool {
    let mut seen = HashSet::new();
    for item in items {
        if !seen.insert(item) {
            return false;
        }
    }
    true
}

pub fn type_contract(ty: TypeF<Box<Type>, RecordRows, EnumRows, RichTerm>) -> RichTerm {
    Term::Type {
        typ: ty.into(),
        // We don't actually care about the contract -- it's a runtime thing.
        contract: Term::Null.into(),
    }
    .into()
}

pub fn num(x: &Number) -> RichTerm {
    Term::Num(x.clone()).into()
}

pub fn sequence(mut contracts: Vec<RichTerm>) -> RichTerm {
    if contracts.len() == 1 {
        contracts.pop().unwrap()
    } else {
        mk_app!(
            static_access("std", ["contract", "Sequence"]),
            Term::Array(contracts.into_iter().collect(), ArrayAttrs::default())
        )
    }
}
