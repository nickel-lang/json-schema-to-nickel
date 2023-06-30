use std::io::stderr;

use json_schema_test_suite::{json_schema_test_suite, TestCase};
use nickel_lang_core::{eval::cache::lazy::CBNCache, program::Program, term::RichTerm};
use stringreader::StringReader;

#[json_schema_test_suite("vendor/JSON-Schema-Test-Suite", "draft7", {
    "optional_format_.*",
    "definitions_.*",
    "dependencies_.*", // TODO: implement dependencies
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
    "propertyNames_.*", // TODO: implement propertyNames
    "refRemote_.*",
    "ref_.*", // TODO: make reference handling robust
    "unknownKeyword_.*", // we don't handle `$id` at all, yet
})]
fn translation_typecheck_test(
    // TODO: fork json_schema_test_suite and remove the mock server
    _server_address: &str,
    test_case: TestCase,
) {
    let contract = if test_case.schema.is_object() {
        json_schema_to_nickel::root_schema(dbg!(serde_json::from_value(test_case.schema).unwrap()))
    } else {
        json_schema_to_nickel::wrap_predicate(
            json_schema_to_nickel::schema_to_predicate(dbg!(serde_json::from_value(
                test_case.schema
            )
            .unwrap())),
            std::iter::empty(),
        )
    };

    let instance: RichTerm = serde_json::from_value(test_case.instance).unwrap();

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
