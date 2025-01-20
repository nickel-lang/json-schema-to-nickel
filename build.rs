use std::{
    env,
    error::Error,
    ffi::OsString,
    fs,
    path::{Path, PathBuf},
};

use nickel_lang_core::{
    cache::InputFormat,
    eval::cache::CacheImpl,
    match_sharedterm,
    program::Program,
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

/// parses the nickel file at `path` and substitues any manually performs import
/// resolution, splicing the files imported inline into the AST
fn inline_imports(path: impl Into<OsString>) -> Result<RichTerm, Box<dyn Error>> {
    let path = &path.into();
    let mut program: Program<CacheImpl> = Program::new_from_file(path.clone(), std::io::stderr())?;
    let rt = program.parse().map_err(|e| program.report_as_str(e))?;

    rt.traverse::<_, Box<dyn Error>>(
        &mut |subterm: RichTerm| {
            match_sharedterm!(match (subterm.term) {
                Term::Import {
                    path: import_path_rel,
                    format: InputFormat::Nickel,
                } => {
                    let mut import_path_abs: PathBuf = path.into();
                    import_path_abs.set_file_name(import_path_rel);
                    inline_imports(import_path_abs)
                }
                _ => Ok(subterm),
            })
        },
        TraverseOrder::BottomUp,
    )
}

fn main() -> Result<(), Box<dyn Error>> {
    let lib = inline_imports("./lib/predicates.ncl")?;

    let out_dir = env::var_os("OUT_DIR").ok_or("environment variable OUT_DIR doesn't exist")?;
    let gen_lib_path = Path::new(&out_dir).join("predicates.ncl");
    fs::write(gen_lib_path, format!("{lib}"))?;
    Ok(())
}
