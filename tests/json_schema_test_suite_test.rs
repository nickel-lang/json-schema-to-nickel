use json_schema_test_suite::{json_schema_test_suite, TestCase};
use nickel_lang::{program::Program, term::RichTerm, types::TypeF};
use schemars::schema::Schema;
use stringreader::StringReader;

#[json_schema_test_suite("vendor/JSON-Schema-Test-Suite", "draft7")]
fn translation_typecheck_test(
    // TODO: fork json_schema_test_suite and remove the mock server
    _server_address: &str,
    test_case: TestCase,
) {
    let schema: Schema = serde_json::from_value(test_case.schema).unwrap();
    let types = json_schema_to_nickel::schema_to_types(dbg!(schema));
    let instance: RichTerm = serde_json::from_value(test_case.instance).unwrap();

    let actual = Program::new_from_source(
        StringReader::new(&dbg!((match types.0 {
            TypeF::Flat(ref t) => format!("{} | ({})", instance, t),
            _ => format!("{} | ({})", instance, types),
        })
        .replace("\u{a0}", " "))), // TODO: fix nbsp in stringification of { ; Dyn }
        "test",
    )
    .unwrap()
    .eval_full();

    assert_eq!(
        test_case.is_valid,
        matches!(actual, Ok(_)),
        "got: {:#?}",
        actual
    );
}
