use nickel_lang_core::{
    identifier::Ident,
    term::{make, RichTerm},
};

pub fn static_access<I, S>(record: S, fields: I) -> RichTerm
where
    I: IntoIterator<Item = S>,
    I::IntoIter: DoubleEndedIterator,
    S: Into<Ident>,
{
    make::static_access(make::var(record), fields)
}

/// Replace special escaping sequences by the actual character within one element of a JSON pointer
/// path. See the [JSON pointer syntax](https://datatracker.ietf.org/doc/html/rfc6901#section-3).
/// Currently, this just amounts to replace `~0` by `~` and `~1` by `/`.
pub fn decode_json_ptr_part(part: &str) -> String {
    part.replace("~0", "~").replace("~1", "/")
}
