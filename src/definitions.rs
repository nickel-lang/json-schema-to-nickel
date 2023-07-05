use std::collections::HashMap;

use nickel_lang_core::term::RichTerm;

pub struct Access {
    pub contract: RichTerm,
    pub predicate: RichTerm,
}

pub struct Definition {
    pub access: Access,
    pub contract: RichTerm,
    pub predicate: RichTerm,
}

pub type Environment = HashMap<String, Definition>;
