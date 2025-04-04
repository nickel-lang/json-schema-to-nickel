use std::{
    cell::RefCell,
    collections::{BTreeMap, BTreeSet},
};

use nickel_lang_core::term::RichTerm;

use crate::{references::AcyclicReferences, utils::static_access};

/// The owned part of a [`ContractContext`].
pub struct ContractContextData<'a, 'refs> {
    refs: &'a AcyclicReferences<'refs>,
    lib_name: &'a str,
    refs_name: &'a str,
    /// In general, a json-schema reference might be followed in an eager context
    /// *and* in a lazy context, and so we'll have to generate both the lazy and
    /// eager versions of the contract that the reference points to. We can avoid
    /// this when the reference points to a schema that's always eager.
    always_eager_refs: BTreeMap<&'a str, bool>,
    accessed_refs: RefCell<BTreeSet<(String, bool)>>,
}

/// Maintains the context used while translating schemas to nickel contracts.
///
/// Most notably, this includes:
///  - remembering whether a contract is forced to be eager
///  - keeping track of which json-schema refs were followed
#[derive(Clone, Copy)]
pub struct ContractContext<'a, 'refs> {
    eager: bool,
    inner: &'a ContractContextData<'a, 'refs>,
}

impl<'a, 'refs> ContractContextData<'a, 'refs> {
    pub fn new(refs: &'a AcyclicReferences<'refs>, lib_name: &'a str, refs_name: &'a str) -> Self {
        let always_eager = refs
            .iter()
            .map(|(name, schema)| (name, schema.is_always_eager(refs)))
            .collect();
        Self {
            refs,
            lib_name,
            refs_name,
            always_eager_refs: always_eager,
            accessed_refs: RefCell::new(BTreeSet::new()),
        }
    }

    pub fn ctx(&'a self) -> ContractContext<'a, 'refs> {
        ContractContext {
            eager: false,
            inner: self,
        }
    }
}

impl<'refs> ContractContext<'_, 'refs> {
    /// Returns a Nickel term pointing to a path in our contracts library.
    pub fn js2n(&self, path: &str) -> RichTerm {
        static_access(self.inner.lib_name, path.split('.'))
    }

    /// Returns a Nickel term pointing to a path in the standard library.
    pub fn std(&self, path: &str) -> RichTerm {
        // TODO: protect the name "std" against shadowing
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

    /// Returns a Nickel term pointing to a contract for the json-schema reference `name`.
    pub fn ref_term(&self, name: &str) -> RichTerm {
        let eager = self.eager && !self.inner.always_eager_refs.get(name).unwrap_or(&true);
        // TODO: once we convert to the new ast, make this an actual nested access
        let names: Vec<_> = self.ref_name(name, eager).collect();

        if self.inner.refs.get(name).is_some() {
            self.inner
                .accessed_refs
                .borrow_mut()
                .insert((name.to_owned(), eager));
            static_access(self.inner.refs_name, [names.join(".").as_str()])
        } else {
            // TODO: warn here, because ideally we trim missing references early on.
            static_access(self.inner.lib_name, ["Always"])
        }
    }

    /// Returns the name that the generated contracts use to refer to our
    /// contracts library.
    ///
    /// This is probably something like "js2n", but it might be modified to
    /// avoid name collisions.
    pub fn lib_name(&self) -> &str {
        self.inner.lib_name
    }

    /// Returns the name that the generated contracts use to refer to our
    /// collection of reference targets.
    ///
    /// This is probably something like "refs", but it might be modified to
    /// avoid name collisions.
    pub fn refs_name(&self) -> &str {
        self.inner.refs_name
    }

    /// Returns all the schema references we know about.
    pub fn refs(&self) -> &AcyclicReferences<'refs> {
        self.inner.refs
    }

    /// Returns a new context that forces an eager contract.
    pub fn eager(self) -> Self {
        Self {
            eager: true,
            ..self
        }
    }

    /// Returns a new context that forces a lazy contract.
    pub fn lazy(self) -> Self {
        Self {
            eager: false,
            ..self
        }
    }

    /// In this context, do we need to provide an eager contract?
    pub fn is_eager(&self) -> bool {
        self.eager
    }

    /// Returns a set of all json-schema references that were accessed (through
    /// calls to `ref_term`) since this method was last called.
    ///
    /// Along with each reference name, returns a bool indicating whether it was
    /// accessed in an eager context.
    pub fn take_accessed_refs(&self) -> BTreeSet<(String, bool)> {
        std::mem::take(&mut self.inner.accessed_refs.borrow_mut())
    }
}
