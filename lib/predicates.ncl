{
  from_simple_predicate
    : String -> (Dyn -> Bool) -> (Dyn -> [| 'Ok, 'Err String |])
    | doc m%"
      Convert a simple boolean predicate into a predicate supporting error messages
    "%
    = fun error' pred x => if pred x then 'Ok else 'Err error',

  never : Dyn -> [| 'Ok, 'Err String |]
    = std.function.const ('Err "never"),

  always : Dyn -> [| 'Ok, 'Err String |]
    = std.function.const 'Ok,

  oneOf
    : Array (Dyn -> [| 'Ok, 'Err String |]) -> Dyn -> [| 'Ok, 'Err String |]
    | doc m%"
      `oneOf preds` succeeds if precisely one of the predicates in `preds` succeeds
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.7.3]
    "%
    = fun preds x =>
      let count_ok : Array [| 'Ok, 'Err String |] -> Number = fun results =>
          std.array.fold_left (fun n b => if b == 'Ok then n + 1 else n) 0 results
        in
      let results = preds |> std.array.map (fun pred => pred x) in
      let count = count_ok results in
      if count == 0 then
        let errors = std.array.flat_map (fun result => result |> match { 'Ok => [], 'Err s => ["  - %{s}"] }) results
        in
        'Err "oneOf: none of the options matched\n%{std.string.join "\n" errors}"
      else if count > 1 then
        'Err "oneOf: more than one of the options matched"
      else
        'Ok,

  anyOf
    : Array (Dyn -> [| 'Ok, 'Err String |]) -> Dyn -> [| 'Ok, 'Err String |]
    | doc m%"
      `anyOf preds` succeeds if at least one of the predicates in `preds` succeeds
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.7.2]
    "%
    = fun preds x =>
      preds
      |> std.array.fold_right
        (
          fun pred acc =>
            acc |> match {
              'Ok => 'Ok,
              'Errs msgs => pred x |> match {
                'Ok => 'Ok,
                'Err msg => 'Errs (["  - %{msg}"] @ msgs),
              }
            }
        )
        ('Errs [])
      |> match {
        'Ok => 'Ok,
        'Errs msgs => 'Err "anyOf: none of the options matched\n%{std.string.join "\n" msgs}"
      },

  allOf
    : Array (Dyn -> [| 'Ok, 'Err String |]) -> Dyn -> [| 'Ok, 'Err String |]
    | doc m%"
      `allOf preds` succeeds if all of the predicates in `preds` succeed
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.7.1]
    "%
   = fun preds x =>
      preds
      |> std.array.fold_right
        (
          fun pred acc =>
            pred x |> match {
              'Ok => acc,
              'Err s => 'Err s,
            }
        )
        'Ok,

  not
    : (Dyn -> [| 'Ok, 'Err String |]) -> Dyn -> [| 'Ok, 'Err String |]
    | doc m%"
      `not pred` succeeds if and only if `pred` fails
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.7.4]
    "%
    = fun pred x =>
      pred x |> match {
        'Ok => 'Err "Inverted predicate succeeded unexpectedly",
        'Err msg => 'Ok
      },

  ifThenElse
    : (Dyn -> [| 'Ok, 'Err String |]) -> (Dyn -> [| 'Ok, 'Err String |]) -> (Dyn -> [| 'Ok, 'Err String |]) -> Dyn -> [| 'Ok, 'Err String |]
    | doc m%"
      `ifThenElse i t e` first checks if the predicate `i` succeeds. If it does, it's equivalent to `t`, otherwise it's equivalent to `e`.
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.6]
    "%
    = fun i t e x =>
      i x |> match {
        'Ok => t x,
        'Err msg => e x
      },

  enum
    : Array Dyn -> Dyn -> [| 'Ok, 'Err String |]
    | doc m%"
      `enum values x` succeeds if and only if `x` is equal to one of the elements of `values`.
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.1.2]
    "%
    = let
      checkEqual = fun input variant =>
        input == variant || (std.is_enum input && (std.string.from_enum input == variant | Bool))
    in fun values x =>
      values
      |> std.array.fold_right
        (
          fun value acc =>
            if checkEqual x value then
              'Ok
            else
              acc
        )
        ('Err "expected one of %{std.serialize 'Json (values | Dyn)}"),

  const
    : Dyn -> Dyn -> [| 'Ok, 'Err String |]
    | doc m%"
      `const value x` succeeds if and only if `x` is equal to `value`
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.1.3]
    "%
    = fun value x =>
      if x == value then
        'Ok
      else
        'Err "expected %{std.serialize 'Json value}",

  isType
    : [|
      'Array,
      'Bool,
      'Integer,
      'Null,
      'Number,
      'Record,
      'String,
    |] -> (Dyn -> [| 'Ok, 'Err String |])
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

  contract_from_predicate : (Dyn -> [| 'Ok, 'Err String |]) -> Dyn -> Dyn -> Dyn
    = fun predicate label value =>
      predicate value |> match {
        'Ok => value,
        'Err msg => std.contract.blame_with_message msg label | Dyn,
      }
}

