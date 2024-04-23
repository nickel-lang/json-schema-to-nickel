use schemars::schema::Schema;
use std::io::stderr;

use json_schema_test_suite::{json_schema_test_suite, TestCase};
use json_schema_to_nickel::{
    predicates::AsPredicate, references::Environment, root_schema, wrap_contract,
};
use nickel_lang_core::{
    error::{Error, EvalError},
    eval::cache::lazy::CBNCache,
    program::Program,
    term::RichTerm,
};
use stringreader::StringReader;

#[json_schema_test_suite("vendor/JSON-Schema-Test-Suite", "draft7", {
    "optional_format_.*",
    "definitions_0_1.*", // contains an external reference (remote URI)
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
    "refRemote_.*", // no.
    // TODO: make reference handling robust
    // The following are references that aren't yet handled by js2n (remote URIs, local files
    // and non-top level definitions)
    "ref_0_3.*", // reference to the whole schema `#` not yet supported
    "ref_12_1.*", // reference to bare URI `node` (no fragment, no leading slash). Should fail
                  // because invalid, but js2n replace that by a `Dyn` contract
    "ref_14_1.*", // reference to an anchor "#foo" not yet supported
    "ref_15_1.*", // anchor + remote URI
    "ref_16_1.*", // external reference (remote URI)
    "ref_18_0.*", // reference to a local definition ("#/definitions/inner" but the definition
                  // isn't at the top-level? I'm not even sure how it should be handled in all
                  // generality) + absolute local URI
    "ref_18_1.*", // Same as ref_18_0
                  // generality) + absolute local URI
    "ref_18_2.*", // external reference (local file URI)
    "ref_19_2.*", // external reference (local file URI),
    "ref_19_0.*", // Same as refs_18_0
    "ref_19_1.*", // Same as refs_18_0
    "ref_20_1.*", // external reference (remote URI)
    "ref_21_1.*", // urn:uuid URI scheme not supported
    "ref_26_1.*", // urn:uuid URI scheme not supported
    "ref_27_1.*", // urn:uuid URI scheme not supported,
    "ref_28_0.*", // external reference (remote URI)
    "ref_29_0.*", // external reference (remote URI)
    "ref_30_0.*", // external reference (remote URI)
    "ref_31_1.*", // external reference (absolute path /absref/foobar.json)
    "ref_5_1.*", // not related to external ref, but js2n doesn't properly ignore other components
                 // when the `ref` field is used
    "ref_6_0.*", // reference to a local file (foo.json)
    "ref_7_1.*", // external reference (remote URI)
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
        let schema: Schema = dbg!(serde_json::from_value(test_case.schema).unwrap());

        wrap_contract(
            Environment::empty(),
            schema.as_predicate(&mut Default::default()).into(),
        )
    };

    let instance: RichTerm = serde_json::from_value(test_case.instance).unwrap();

    // FIXME: this relies on `./lib/predicates.nix` being accessible from the
    // working directory
    let program = format!("{} | ({})", instance, contract);
    eprintln!("{}", program);

    let actual =
        Program::<CBNCache>::new_from_source(StringReader::new(&program), "test", stderr())
            .unwrap()
            .eval_full();

    match (test_case.is_valid, actual) {
        (true, Ok(_)) => {}
        (true, Err(e)) => panic!("expected success, got evaluation error {e:#?}"),
        // For now, experience shows that a contract failure can be one of those three errors:
        // field missing, missing field def, or generic contract error (blame error)
        (false, Err(Error::EvalError(EvalError::BlameError { .. })))
        | (false, Err(Error::EvalError(EvalError::MissingFieldDef { .. })))
        | (false, Err(Error::EvalError(EvalError::FieldMissing { .. }))) => {}
        (false, Ok(_)) => panic!("expected blame error, got success"),
        (false, Err(e)) => panic!("expected blame error, got different error {e:#?}"),
    }
}
