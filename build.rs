use std::{
    env,
    error::Error,
    ffi::OsString,
    fs,
    path::{Path, PathBuf},
};

use nickel_lang_core::{
    cache::{Cache, ErrorTolerance},
    match_sharedterm,
    parser::{grammar::TermParser, lexer::Lexer, ErrorTolerantParser},
    term::{RichTerm, Term, Traverse, TraverseOrder},
};

// Currently nickel does not have a package management system. Until there's
// an idiomatic way to refer to the library we've defined, we're just going to
// inline it into the generated nickel code.
//
// Problem: We can't do that naively because we have `import` statements.
// Solution: Inline all the imports
//
// Problem: We don't even know where the library will be, or if it will even be
//          available, at json_schema_to_nickel runtime.
// Solution: We grab the contents of the file at json_schema_to_nickel compile
//           time, and make it available through the build script. If we didn't
//           have the nickel imports we could just use `include_bytes!` directly
//
// Problem: The resulting RichTerm cannot be serialized into the
//          json_schema_to_nickel binary, because it contains allocated fields.
// Solution: We do a little dance where we pretty print it into a string at
//           build time, and re-parse it at runtime. We will then pretty-print
//           it *again* at runtime.

fn inline_imports(path: impl Into<OsString>) -> Result<RichTerm, Box<dyn Error>> {
    fn helper(
        path: impl Into<OsString>,
        cache: &mut Cache,
        parser: &TermParser,
    ) -> Result<RichTerm, Box<dyn Error>> {
        let path = &path.into();
        let file_id = cache.add_file(path.clone())?;
        let lexer = Lexer::new(cache.source(file_id));
        let rt = parser.parse_strict(file_id, lexer).unwrap();
        rt.traverse::<_, _, Box<dyn Error>>(
            &|subterm: RichTerm, cache| {
                match_sharedterm! { subterm.term, with {
                    Term::Import(p) => {
                        let mut pb: PathBuf = path.into();
                        pb.set_file_name(p);
                        helper(pb, cache, parser)
                    }
                } else {
                    Ok(subterm)
                // XXX: how is the formatting supposed to work for these curly braces??
                }}
            },
            cache,
            TraverseOrder::BottomUp,
        )
    }

    let mut cache = Cache::new(ErrorTolerance::Strict);
    let parser = TermParser::new();
    helper(path, &mut cache, &parser)
}

fn main() -> Result<(), Box<dyn Error>> {
    let lib = inline_imports("./lib/predicates.ncl")?;

    let out_dir = env::var_os("OUT_DIR").unwrap();
    let comptime_rust_lib = Path::new(&out_dir).join("predicates.ncl");
    fs::write(&comptime_rust_lib, format!("{lib}"))?;
    Ok(())
}
