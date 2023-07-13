use std::io::stderr;

use json_schema_test_suite::{json_schema_test_suite, TestCase};
use json_schema_to_nickel::{
    definitions::Environment, predicates::schema_to_predicate, root_schema, wrap_predicate,
};
use nickel_lang_core::{eval::cache::lazy::CBNCache, program::Program, term::RichTerm};
use stringreader::StringReader;

#[json_schema_test_suite("vendor/JSON-Schema-Test-Suite", "draft7", {
    "optional_format_.*",
    "definitions_.*",
    "id_.*",
    "maxItems_1_.*", // schemars doesn't accept floats as the value of `maxItems`
    "maxLength_1_.*", // schemars doesn't accept floats as the value of `maxLength`
    "maxProperties_1_.*", // schemars doesn't accept floats as the value of `maxProperties`
    "minItems_1_.*", // schemars doesn't accept floats as the value of `minItems`
    "minLength_1_.*", // schemars doesn't accept floats as the value of `minLength`
    "minProperties_1_.*", // schemars doesn't accept floats as the value of `minProperties`
    "optional_bignum_.*",
    "optional_content_.*",
    "optional_cross_draft_.*",
    "optional_ecmascript_regex_.*",
    "optional_float_overflow_0_0", // pretty printer outputs scientific notation numbers which Nickel doesn't support
    "multipleOf_4_0", // pretty printer outputs scientific notation numbers which Nickel doesn't support
    "refRemote_.*", // no.
    "ref_.*", // TODO: make reference handling robust
    "unknownKeyword_.*", // we don't handle `$id` at all, yet
})]
fn translation_typecheck_test(
    // TODO: fork json_schema_test_suite and remove the mock server
    _server_address: &str,
    test_case: TestCase,
) {
    let contract = if test_case.schema.is_object() {
        root_schema(&dbg!(serde_json::from_value(test_case.schema).unwrap()))
    } else {
        wrap_predicate(
            Environment::empty(),
            schema_to_predicate(&dbg!(serde_json::from_value(test_case.schema).unwrap())),
        )
    };

    let instance: RichTerm = serde_json::from_value(test_case.instance).unwrap();

    // FIXME: this relies on `./lib/predicates.nix` being accessible from the working directory
    let program = format!("{} | ({})", instance, contract);
    eprintln!("{}", program);

    let actual =
        Program::<CBNCache>::new_from_source(StringReader::new(&program), "test", stderr())
            .unwrap()
            .eval_full();

    assert_eq!(
        test_case.is_valid,
        matches!(actual, Ok(_)),
        "got: {:#?}",
        actual
    );
}
