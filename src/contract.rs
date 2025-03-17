use std::{
    cell::RefCell,
    collections::{BTreeMap, BTreeSet},
};

use nickel_lang_core::term::RichTerm;

use crate::{references::References, utils::static_access};

// TODO: provide a constructor and make fields private
#[derive(Clone, Copy)]
pub struct ContractContext<'a, 'refs> {
    pub refs: &'a References<'refs>,
    pub lib_name: &'a str,
    pub refs_name: &'a str,
    pub eager: bool,
    pub always_eager_refs: &'a BTreeMap<&'a str, bool>,
    pub accessed_refs: &'a RefCell<BTreeSet<(String, bool)>>,
}

impl ContractContext<'_, '_> {
    pub fn js2n(&self, path: &str) -> RichTerm {
        static_access(self.lib_name, path.split('.'))
    }

    pub fn std(&self, path: &str) -> RichTerm {
        static_access("std", path.split('.'))
    }

    pub fn ref_name<'b>(
        &'b self,
        name: &'b str,
        eager: bool,
    ) -> impl DoubleEndedIterator<Item = &'b str> {
        let trimmed_name = name.trim_start_matches(['#', '/']);
        eager
            .then_some("eager")
            .into_iter()
            .chain(trimmed_name.split('/'))
    }

    pub fn ref_term(&self, name: &str) -> RichTerm {
        let eager = self.eager && !self.always_eager_refs.get(name).unwrap_or(&true);
        // FIXME: once we convert to the new ast, make this an actual nested access
        let names: Vec<_> = self.ref_name(name, eager).collect();

        if self.refs.get(name).is_some() {
            self.accessed_refs
                .borrow_mut()
                .insert((name.to_owned(), eager));
            static_access(self.refs_name, [names.join(".").as_str()])
        } else {
            // TODO: warn here, because ideally we trim missing references early on.
            static_access(self.lib_name, ["Always"])
        }
    }

    pub fn eager(self) -> Self {
        Self {
            eager: true,
            ..self
        }
    }

    pub fn lazy(self) -> Self {
        Self {
            eager: false,
            ..self
        }
    }

    pub fn is_eager(&self) -> bool {
        self.eager
    }
}
