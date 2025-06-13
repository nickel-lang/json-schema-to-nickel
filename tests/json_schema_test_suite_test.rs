use std::{io::stderr, path::Path, process::ExitCode};

use json_schema_to_nickel::inline_lib;
use libtest_mimic::{Arguments, Trial};
use nickel_lang_core::{
    error::{report, Error, EvalError, NullReporter},
    eval::cache::lazy::CBNCache,
    program::Program,
    term::RichTerm,
};
use regex::Regex;
use stringreader::StringReader;

#[derive(serde::Deserialize)]
#[serde(transparent)]
struct TestFile {
    schema_tests: Vec<SchemaTest>,
}

#[derive(serde::Deserialize)]
struct SchemaTest {
    schema: serde_json::Value,
    tests: Vec<Example>,
}

#[derive(serde::Deserialize)]
struct Example {
    data: serde_json::Value,
    valid: bool,
}

pub fn main() -> ExitCode {
    let args = Arguments::from_args();
    let root = env!("CARGO_MANIFEST_DIR");
    let test_dir = format!("{root}/vendor/JSON-Schema-Test-Suite/tests/draft7");

    let test_glob = glob::glob(&format!("{test_dir}/**/*.json")).unwrap();

    let ignore_regex = Regex::new(dbg!(&SKIP_TESTS.join("|"))).unwrap();
    assert!(ignore_regex.is_match("optional/format/date_0_12"));

    let tests: Vec<_> = test_glob
        .flat_map(|p| {
            let path = p.unwrap();
            let name = path
                .strip_prefix(&test_dir)
                .unwrap()
                .to_owned()
                .display()
                .to_string()
                .strip_suffix(".json")
                .unwrap()
                .to_owned();
            gather_tests(name, &path, &ignore_regex)
        })
        .collect();

    libtest_mimic::run(&args, tests).exit_code()
}

fn gather_tests(name: String, path: &Path, ignore_regex: &Regex) -> Vec<Trial> {
    let contents = std::fs::read_to_string(path).unwrap();
    let test_file: TestFile = serde_json::from_str(&contents).unwrap();

    test_file
        .schema_tests
        .into_iter()
        .enumerate()
        .flat_map(|(schema_idx, schema_test)| {
            let name = name.clone();
            schema_test
                .tests
                .into_iter()
                .enumerate()
                .map(move |(test_idx, test)| {
                    let name = format!("{name}_{schema_idx}_{test_idx}");
                    let schema = schema_test.schema.clone();
                    let ignore = ignore_regex.is_match(&name);
                    Trial::test(name, move || {
                        translation_typecheck_test(schema, test.data, test.valid);
                        Ok(())
                    })
                    .with_ignored_flag(ignore)
                })
        })
        .collect()
}

const SKIP_TESTS: &[&str] = &[
    "optional\\/format.*",
    // contains an external reference (remote URI)
    "definitions_0_1.*",
    "id_.*",
    // schemars doesn't accept floats as the value of `maxItems`
    "maxItems_1_.*",
    // schemars doesn't accept floats as the value of `maxLength`
    "maxLength_1_.*",
    // schemars doesn't accept floats as the value of `maxProperties`
    "maxProperties_1_.*",
    // schemars doesn't accept floats as the value of `minItems`
    "minItems_1_.*",
    // schemars doesn't accept floats as the value of `minLength`
    "minLength_1_.*",
    // schemars doesn't accept floats as the value of `minProperties`
    "minProperties_1_.*",
    "optional/bignum.*",
    "optional/content.*",
    "optional/cross-draft.*",
    "optional/ecmascript-regex.*",
    // no.
    "refRemote_.*",
    // TODO: make reference handling robust
    // The following are references that aren't yet handled by js2n (remote URIs, local files
    // and non-top level definitions)

    // reference to the whole schema `#` not yet supported
    "ref_0_3.*",
    // reference to bare URI `node` (no fragment, no leading slash). Should fail
    // because invalid, but js2n replace that by a `Dyn` contract
    "ref_12_1.*",
    // reference to an anchor "#foo" not yet supported
    "ref_14_1.*",
    // anchor + remote URI
    "ref_15_1.*",
    // external reference (remote URI)
    "ref_16_1.*",
    // reference to a local definition ("#/definitions/inner" but the definition
    // isn't at the top-level? I'm not even sure how it should be handled in all
    // generality) + absolute local URI
    "ref_18_0.*",
    // Same as ref_18_0
    "ref_18_1.*",
    // external reference (local file URI)
    "ref_18_2.*",
    // external reference (local file URI),
    "ref_19_2.*",
    // Same as refs_18_0
    "ref_19_0.*",
    // Same as refs_18_0
    "ref_19_1.*",
    // external reference (remote URI)
    "ref_20_1.*",
    // urn:uuid URI scheme not supported
    "ref_21_1.*",
    // urn:uuid URI scheme not supported
    "ref_26_1.*",
    // urn:uuid URI scheme not supported,
    "ref_27_1.*",
    // external reference (remote URI)
    "ref_28_0.*",
    // external reference (remote URI)
    "ref_29_0.*",
    // external reference (remote URI)
    "ref_30_0.*",
    // external reference (absolute path /absref/foobar.json)
    "ref_31_1.*",
    // not related to external ref, but js2n doesn't properly ignore other components
    // when the `ref` field is used
    "ref_5_1.*",
    // reference to a local file (foo.json)
    "ref_6_0.*",
    // external reference (remote URI)
    "ref_7_1.*",
    // we don't handle `$id` at all, yet
    "unknownKeyword_.*",
];

fn translation_typecheck_test(
    schema: serde_json::Value,
    test_case: serde_json::Value,
    is_valid: bool,
) {
    let contract = json_schema_to_nickel::convert(&schema, inline_lib()).unwrap();

    let instance: RichTerm = serde_json::from_value(test_case).unwrap();

    let program = format!("{} | ({})", instance, contract);
    eprintln!("{}", instance);

    let mut prog = Program::<CBNCache>::new_from_source(
        StringReader::new(&program),
        "test",
        stderr(),
        NullReporter {},
    )
    .unwrap();
    let actual = prog.eval_full();

    match (is_valid, actual) {
        (true, Ok(_)) => {}
        (true, Err(e)) => {
            report::report(
                &mut prog.files(),
                e,
                report::ErrorFormat::Text,
                Default::default(),
            );
            panic!("expected success, got evaluation error");
        }
        // For now, experience shows that a contract failure can be one of those three errors:
        // field missing, missing field def, or generic contract error (blame error)
        (false, Err(Error::EvalError(EvalError::BlameError { .. })))
        | (false, Err(Error::EvalError(EvalError::MissingFieldDef { .. })))
        | (false, Err(Error::EvalError(EvalError::FieldMissing { .. }))) => {}
        (false, Ok(_)) => panic!("expected blame error, got success"),
        (false, Err(e)) => panic!("expected blame error, got different error {e:#?}"),
    }
}
