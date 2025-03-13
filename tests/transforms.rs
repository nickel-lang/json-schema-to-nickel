use insta::assert_ron_snapshot;
use json_schema_to_nickel::{
    intermediate::{self, inline_refs, simplify},
    references::References,
    schema::Schema,
};
use libtest_mimic::{Arguments, Trial};
use std::{path::Path, process::ExitCode};

pub fn main() -> ExitCode {
    let args = Arguments::from_args();
    let root = env!("CARGO_MANIFEST_DIR");

    let manifest_glob = glob::glob(&format!("{root}/tests/transforms/inputs/**/*.json")).unwrap();

    let tests: Vec<_> = manifest_glob
        .map(|p| {
            let path = p.unwrap();
            let name = path.strip_prefix(root).unwrap().to_owned();
            Trial::test(name.display().to_string(), move || {
                snapshot_ir(&path, name.to_str().unwrap());
                Ok(())
            })
        })
        .collect();

    libtest_mimic::run(&args, tests).exit_code()
}

fn snapshot_ir(path: &Path, name: &str) {
    // TODO: allow the test input to customize the transforms that get applied
    let file = std::fs::read_to_string(path).unwrap();
    let val: serde_json::Value = serde_json::from_str(&file).unwrap();
    let schema: Schema = (&val).try_into().unwrap();

    let (schema, all_refs) = intermediate::resolve_references_recursive(&val, schema);
    let refs = References::new(&all_refs);
    let simple_refs = all_refs
        .iter()
        .map(|(k, v)| (k.clone(), simplify(v.clone(), &refs)))
        .collect();
    let refs = References::new(&simple_refs);
    let schema = inline_refs(schema, &refs);
    let schema = simplify(schema, &refs);

    assert_ron_snapshot!(name, schema);
}
