{
  from_simple_predicate
    : String -> (Dyn -> Bool) -> (Dyn -> { success : Bool, error : String })
    | doc m%"
      Convert a simple boolean predicate into a predicate supporting error messages
    "%
    = fun error' pred x =>
      { success = pred x, error = error' },

  never : Dyn -> { success : Bool, error : String }
    = std.function.const { success = false, error = "never" },

  always : Dyn -> { success : Bool, error : String }
    = std.function.const { success = true, error = "" },

  oneOf
    : Array (Dyn -> { success : Bool, error : String }) -> Dyn -> { success : Bool, error : String }
    | doc m%"
      `oneOf preds` succeeds if precisely one of the predicates in `preds` succeeds
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.7.3]
    "%
    = fun preds x =>
      let count_true : Array { success : Bool, error : String } -> Number = fun results =>
          std.array.fold_left (fun n b => if b.success then n + 1 else n) 0 results
        in
      let results = preds |> std.array.map (fun pred => pred x) in
      let count = count_true results in
      if count == 0 then
        let errors = std.array.map (fun result => "  - %{result.error}") results
        in
        { success = false, error = "oneOf: none of the options matched\n%{std.string.join "\n" errors}" }
      else if count > 1 then
        { success = false, error = "oneOf: more than one of the options matched" }
      else
        { success = true, error = "" },

  anyOf
    : Array (Dyn -> { success : Bool, error : String }) -> Dyn -> { success : Bool, error : String }
    | doc m%"
      `anyOf preds` succeeds if at least one of the predicates in `preds` succeeds
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.7.2]
    "%
    = fun preds x =>
      preds
      |> std.array.fold_right
        (
          fun pred acc =>
            let result = pred x in
            if result.success || acc.success then
              { success = true, errors = [] }
            else
              { success = false, errors = ["  - %{result.error}"] @ acc.errors }
        )
        { success = false, errors = [] }
      |> (
        fun result =>
          if result.success then
            { success = true, error = "" }
          else
            {
              success = false,
              error = "anyOf: none of the options matched\n%{std.string.join "\n" result.errors}"
            }
      ),

  allOf
    : Array (Dyn -> { success : Bool, error : String }) -> Dyn -> { success : Bool, error : String }
    | doc m%"
      `allOf preds` succeeds if all of the predicates in `preds` succeed
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.7.1]
    "%
   = fun preds x =>
      preds
      |> std.array.fold_right
        (
          fun pred acc =>
            let result = pred x in
            if !result.success then
              result
            else
              acc
        )
        { success = true, error = "" },

  not
    : (Dyn -> { success : Bool, error : String }) -> Dyn -> { success : Bool, error : String }
    | doc m%"
      `not pred` succeeds if and only if `pred` fails
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.7.4]
    "%
    = fun pred x =>
      let result = pred x in
      if result.success then
        { success = false, error = "Inverted predicate succeeded unexpectedly" }
      else
        { success = true, error = "" },

  ifThenElse
    : (Dyn -> { success : Bool, error : String }) -> (Dyn -> { success : Bool, error : String }) -> (Dyn -> { success : Bool, error : String }) -> Dyn -> { success : Bool, error : String }
    | doc m%"
      `ifThenElse i t e` first checks if the predicate `i` succeeds. If it does, it's equivalent to `t`, otherwise it's equivalent to `e`.
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.6]
    "%
    = fun i t e x =>
      let { success, .. } = i x in
      if success then
        t x
      else
        e x,

  enum
    : Array Dyn -> Dyn -> { success : Bool, error : String }
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
              { success = true, error = "" }
            else
              acc
        )
        { success = false, error = "expected one of %{std.serialize 'Json (values | Dyn)}" },

  const
    : Dyn -> Dyn -> { success : Bool, error : String }
    | doc m%"
      `const value x` succeeds if and only if `x` is equalt to `value`
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.1.3]
    "%
    = fun value x =>
      if x == value then
        { success = true, error = "" }
      else
        { success = false, error = "expected %{std.serialize 'Json value}" },

  isType
    : [|
      'Array,
      'Bool,
      'Integer,
      'Null,
      'Number,
      'Record,
      'String,
    |] -> (Dyn -> { success : Bool, error : String })
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

  contract_from_predicate : (Dyn -> { success : Bool, error : String }) -> Dyn -> Dyn -> Dyn
    = fun predicate label value =>
      let { success, error } = predicate value
      in
      if success then
        value
      else
        (std.contract.blame_with_message error label | Dyn),
}

