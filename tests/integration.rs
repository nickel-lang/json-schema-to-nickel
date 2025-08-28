use insta::assert_snapshot;
use json_schema_to_nickel::convert;
use libtest_mimic::{Arguments, Trial};
use nickel_lang_core::{bytecode::ast::AstAlloc, pretty::*};
use pretty::DocBuilder;
use std::{path::Path, process::ExitCode};

pub fn main() -> ExitCode {
    let args = Arguments::from_args();
    let root = env!("CARGO_MANIFEST_DIR");

    let manifest_glob = glob::glob(&format!("{root}/tests/integration/inputs/**/*.json")).unwrap();

    let tests: Vec<_> = manifest_glob
        .map(|p| {
            let path = p.unwrap();
            let name = path.strip_prefix(root).unwrap().to_owned();
            Trial::test(name.display().to_string(), move || {
                generate(&path, name.to_str().unwrap());
                Ok(())
            })
        })
        .collect();

    libtest_mimic::run(&args, tests).exit_code()
}

fn generate(path: &Path, name: &str) {
    let file = std::fs::read_to_string(path).unwrap();
    let val: serde_json::Value = serde_json::from_str(&file).unwrap();

    let lib_term = nickel_lang_core::bytecode::ast::Node::Var("FAKE_JS2N_LIB".into()).into();
    let alloc = AstAlloc::new();
    let contract = convert(&val, lib_term, &alloc).unwrap();

    let pretty_alloc = nickel_lang_core::bytecode::pretty::Allocator::default();
    let types: DocBuilder<'_, _, ()> = contract.pretty(&pretty_alloc);

    let mut out = Vec::new();
    types.render(80, &mut out).unwrap();
    let out = String::from_utf8(out).unwrap();

    assert_snapshot!(name, out);
}
