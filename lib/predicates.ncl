let error_lib = import "./error.ncl" in

{
  from_simple_predicate
    : String -> (Dyn -> Bool) -> (
      Dyn -> [|
        'Ok,
        'Error { message : String, source : Array [| 'Array Number, 'Field String |] }
      |]
    )
    | doc m%"
      Convert a simple boolean predicate into a predicate supporting error messages
    "%
    = fun msg pred x =>
      if pred x then
        'Ok
      else
        error_lib.mk_error msg,

  never
    : Dyn -> [|
      'Ok,
      'Error { message : String, source : Array [| 'Array Number, 'Field String |] }
    |]
    =
      "never"
      |> error_lib.mk_error
      |> std.function.const,

  always
    : Dyn -> [|
      'Ok,
      'Error { message : String, source : Array [| 'Array Number, 'Field String |] }
    |]
    = std.function.const 'Ok,

  oneOf
    : Array (
      Dyn -> [|
        'Ok,
        'Error { message : String, source : Array [| 'Array Number, 'Field String |] }
      |]
    ) -> Dyn -> [|
      'Ok,
      'Error { message : String, source : Array [| 'Array Number, 'Field String |] }
    |]
    | doc m%"
      `oneOf preds` succeeds if precisely one of the predicates in `preds` succeeds
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.7.3]
    "%
    = fun preds x =>
      let count_ok
        : Array [|
          'Ok,
          'Error { message : String, source : Array [| 'Array Number, 'Field String |] }
        |] -> Number
        = fun results =>
          std.array.fold_left (fun n b => if b == 'Ok then n + 1 else n) 0 results
      in
      let results = preds |> std.array.map (fun pred => pred x) in
      let count = count_ok results in
      if count == 0 then
        let errors =
          std.array.flat_map
            (match {
              'Ok => [],
              'Error error => [error]
            }
            )
            results
        in
        'Error (error_lib.combine "oneOf: none of the options matched" errors)
      else if count > 1 then
        error_lib.mk_error "oneOf: more than one of the options matched"
      else
        'Ok,

  anyOf
    : Array (
      Dyn -> [|
        'Ok,
        'Error { message : String, source : Array [| 'Array Number, 'Field String |] }
      |]
    ) -> Dyn -> [|
      'Ok,
      'Error { message : String, source : Array [| 'Array Number, 'Field String |] }
    |]
    | doc m%"
      `anyOf preds` succeeds if at least one of the predicates in `preds` succeeds
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.7.2]
    "%
    = fun preds x =>
      preds
      |> std.array.fold_right
        (fun pred acc =>
          acc
          |> match {
            'Ok => 'Ok,
            'Errors errors =>
              pred x
              |> match {
                'Ok => 'Ok,
                'Error error => 'Errors (std.array.prepend error errors),
              }
          }
        )
        ('Errors [])
      |> match {
        'Ok => 'Ok,
        'Errors errors => 'Error (error_lib.combine "anyOf: none of the options matched" errors),
      },

  allOf
    : Array (
      Dyn -> [|
        'Ok,
        'Error { message : String, source : Array [| 'Array Number, 'Field String |] }
      |]
    ) -> Dyn -> [|
      'Ok,
      'Error { message : String, source : Array [| 'Array Number, 'Field String |] }
    |]
    | doc m%"
      `allOf preds` succeeds if all of the predicates in `preds` succeed
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.7.1]
    "%
    = fun preds x =>
      preds
      |> std.array.fold_right
        (fun pred acc =>
          pred x
          |> match {
            'Ok => acc,
            error @ ('Error _) => error,
          }
        )
        'Ok,

  not
    : (
      Dyn -> [|
        'Ok,
        'Error { message : String, source : Array [| 'Array Number, 'Field String |] }
      |]
    ) -> Dyn -> [|
      'Ok,
      'Error { message : String, source : Array [| 'Array Number, 'Field String |] }
    |]
    | doc m%"
      `not pred` succeeds if and only if `pred` fails
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.7.4]
    "%
    = fun pred x =>
      pred x
      |> match {
        'Ok => error_lib.mk_error "Inverted predicate succeeded unexpectedly",
        'Error _ => 'Ok,
      },

  ifThenElse
    : (
      Dyn -> [|
        'Ok,
        'Error { message : String, source : Array [| 'Array Number, 'Field String |] }
      |]
    ) -> (
      Dyn -> [|
        'Ok,
        'Error { message : String, source : Array [| 'Array Number, 'Field String |] }
      |]
    ) -> (
      Dyn -> [|
        'Ok,
        'Error { message : String, source : Array [| 'Array Number, 'Field String |] }
      |]
    ) -> Dyn -> [|
      'Ok,
      'Error { message : String, source : Array [| 'Array Number, 'Field String |] }
    |]
    | doc m%"
      `ifThenElse i t e` first checks if the predicate `i` succeeds. If it does, it's equivalent to `t`, otherwise it's equivalent to `e`.
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.6]
    "%
    = fun cond ok err x =>
      cond x
      |> match {
        'Ok => ok x,
        'Error _ => err x,
      },

  enum
    : Array Dyn -> Dyn -> [|
      'Ok,
      'Error { message : String, source : Array [| 'Array Number, 'Field String |] }
    |]
    | doc m%"
      `enum values x` succeeds if and only if `x` is equal to one of the elements of `values`.
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.1.2]
    "%
    =
      let checkEqual = fun input variant =>
        input == variant || (std.is_enum input && (std.string.from_enum input == variant | Bool))
      in
      fun values x =>
        values
        |> std.array.fold_right
          (fun value acc =>
            if checkEqual x value then
              'Ok
            else
              acc
          )
          (error_lib.mk_error "expected one of %{std.serialize 'Json (values | Dyn)}"),

  const
    : Dyn -> Dyn -> [|
      'Ok,
      'Error { message : String, source : Array [| 'Array Number, 'Field String |] }
    |]
    | doc m%"
      `const value x` succeeds if and only if `x` is equal to `value`
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.1.3]
    "%
    = fun value x =>
      if x == value then
        'Ok
      else
        error_lib.mk_error "expected %{std.serialize 'Json value}",

  isType
    : [|
      'Array,
      'Bool,
      'Integer,
      'Null,
      'Number,
      'Record,
      'String,
    |] -> (
      Dyn -> [|
        'Ok,
        'Error { message : String, source : Array [| 'Array Number, 'Field String |] }
      |]
    )
    = fun t =>
      t
      |> match {
        'Null => from_simple_predicate "expected `null`" (fun x => x == null),
        'Integer => from_simple_predicate "expected an integer" (fun x => std.is_number x && std.number.is_integer (x | Number)),
        _ => from_simple_predicate "value is not of type %{std.string.from_enum t}" (fun x => std.typeof x == t),
      },

  arrays = import "./arrays.ncl",
  records = import "./records.ncl",
  numbers = import "./numbers.ncl",
  strings = import "./strings.ncl",

  contract_from_predicate
    : (
      Dyn -> [|
        'Ok,
        'Error { message : String, source : Array [| 'Array Number, 'Field String |] }
      |]
    ) -> Dyn
    = fun predicate =>
      std.contract.from_validator (fun value =>
        predicate value
        |> error_lib.to_validator_result
      ),

    or_null : Dyn -> Dyn = fun Contract => std.contract.custom (fun label value =>
      if value == null then
      'Ok value
      else std.contract.check Contract label value
    ),

    always_ctr : Dyn = std.contract.custom (fun label value => 'Ok value)
}
