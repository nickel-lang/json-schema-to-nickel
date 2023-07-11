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
