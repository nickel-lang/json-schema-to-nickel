use std::{
    cell::RefCell,
    collections::{BTreeMap, BTreeSet},
};

use nickel_lang_core::{
    bytecode::ast::{
        record::{FieldDef, FieldPathElem},
        Ast, AstAlloc, Node,
    },
    term::Number,
};

use crate::{references::AcyclicReferences, utils::static_access};

/// The owned part of a [`ContractContext`].
///
/// Keeps track of which json-schema refs were followed, so that the final
/// generated Nickel can ignore things that it doesn't need.
pub struct ContractContextData<'a, 'ast, 'refs> {
    /// All available json-schema refs.
    refs: &'a AcyclicReferences<'refs>,
    /// The name that generated contracts should use to refer to the json-schema
    /// lib. This name will be chosen in advance so that it is guaranteed never
    /// to be shadowed by any name in the generated contracts.
    lib_name: &'a str,
    /// The name that generated contracts should use to refer to the standard library.
    /// This name will be chosen in advance so that it is guaranteed never
    /// to be shadowed by any name in the generated contracts.
    std_name: &'a str,
    /// The name that generated contracts should use to refer to the record
    /// containing all the json-schema refs. Guaranteed not to be shadowed.
    ///
    /// For example, the json-schema ref "#/definitions/glob" can be found at
    /// the Nickel path `<refs_name>.definitions.glob`.
    refs_name: &'a str,
    /// In general, a json-schema reference might be followed in an eager context
    /// *and* in a lazy context, and so we'll have to generate both the lazy and
    /// eager versions of the contract that the reference points to. We can avoid
    /// this when the reference points to a schema that's always eager.
    always_eager_refs: BTreeMap<&'a str, bool>,
    /// The set of refs that were accessed, and for each one whether it was
    /// accessed in an eager context.
    accessed_refs: RefCell<BTreeSet<(String, bool)>>,
    /// An allocator for Nickel AST nodes.
    alloc: &'ast AstAlloc,
}

/// Maintains the context used while translating schemas to nickel contracts.
#[derive(Clone, Copy)]
pub struct ContractContext<'a, 'ast, 'refs> {
    /// Is the contract we're translating forced to be eager?
    eager: bool,
    inner: &'a ContractContextData<'a, 'ast, 'refs>,
}

impl<'a, 'ast, 'refs> ContractContextData<'a, 'ast, 'refs> {
    pub fn new(
        refs: &'a AcyclicReferences<'refs>,
        lib_name: &'a str,
        std_name: &'a str,
        refs_name: &'a str,
        alloc: &'ast AstAlloc,
    ) -> Self {
        let always_eager = refs
            .iter()
            .map(|(name, schema)| (name, schema.is_always_eager(refs)))
            .collect();
        Self {
            refs,
            lib_name,
            std_name,
            refs_name,
            alloc,
            always_eager_refs: always_eager,
            accessed_refs: RefCell::new(BTreeSet::new()),
        }
    }

    pub fn ctx(&'a self) -> ContractContext<'a, 'ast, 'refs> {
        ContractContext {
            eager: false,
            inner: self,
        }
    }
}

impl<'ast, 'refs> ContractContext<'_, 'ast, 'refs> {
    /// Returns a Nickel term pointing to a path in our contracts library.
    pub fn js2n(&self, path: &str) -> Ast<'ast> {
        static_access(self.inner.alloc, self.inner.lib_name, path.split('.'))
    }

    /// Returns a Nickel term pointing to a path in the standard library.
    pub fn std(&self, path: &str) -> Ast<'ast> {
        static_access(self.inner.alloc, self.inner.std_name, path.split('.'))
    }

    /// Splits a json-schema ref name into its path components.
    ///
    /// "#/definitions/glob" becomes `["definitions", "glob"]`
    /// (or `["eager", "definitions", "glob"]` if it's eager).
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
    pub fn ref_term(&self, name: &str) -> Ast<'ast> {
        let eager = self.eager && !self.inner.always_eager_refs.get(name).unwrap_or(&true);
        let names = self.ref_name(name, eager);

        if self.inner.refs.get(name).is_some() {
            self.inner
                .accessed_refs
                .borrow_mut()
                .insert((name.to_owned(), eager));
            static_access(self.inner.alloc, self.inner.refs_name, names)
        } else {
            // TODO: warn here, because ideally we trim missing references early on.
            static_access(self.inner.alloc, self.inner.lib_name, ["Always"])
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

    /// Returns the name that the generated contracts use to refer to the
    /// standard library.
    ///
    /// This is probably just "std", but it might be modified to
    /// avoid name collisions.
    pub fn std_name(&self) -> &str {
        self.inner.std_name
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

    /// Returns an allocator for Nickel ASTs.
    pub fn alloc(&self) -> &'ast AstAlloc {
        self.inner.alloc
    }

    /// Turns a collection of contracts into a single contract.
    pub fn sequence(&self, mut contracts: Vec<Ast<'ast>>) -> Ast<'ast> {
        if contracts.len() == 1 {
            contracts.pop().unwrap()
        } else {
            self.alloc()
                .app(
                    static_access(self.alloc(), "std", ["contract", "Sequence"]),
                    [self.alloc().array(contracts).into()],
                )
                .into()
        }
    }

    pub fn num(&self, x: &Number) -> Ast<'ast> {
        self.alloc().number(x.clone()).into()
    }

    pub fn from_json(&self, val: &serde_json::Value) -> Ast<'ast> {
        let node = match val {
            serde_json::Value::Null => Node::Null,
            serde_json::Value::Bool(b) => Node::Bool(*b),
            serde_json::Value::Number(number) => {
                if let Some(n) = number.as_f64() {
                    // unwrap: JSON has no infinities or NaNs, so the conversion to
                    // rational will succeed
                    self.alloc().number(n.try_into().unwrap())
                } else if let Some(n) = number.as_i128() {
                    self.alloc().number(n.into())
                } else {
                    // serde_json doesn't give us a way to exhaustively match, but
                    // at least for now we've covered all cases
                    unimplemented!()
                }
            }
            serde_json::Value::String(s) => self.alloc().string(s),
            serde_json::Value::Array(xs) => {
                self.alloc().array(xs.iter().map(|x| self.from_json(x)))
            }
            serde_json::Value::Object(map) => {
                let field_defs = map.iter().map(|(k, v)| FieldDef {
                    path: self.alloc().alloc_many([FieldPathElem::Ident(k.into())]),
                    metadata: Default::default(),
                    value: Some(self.from_json(v)),
                    pos: Default::default(),
                });
                Node::Record(self.alloc().record_data([], field_defs, false))
            }
        };
        node.into()
    }
}
