{
  from_simple_predicate
    : String -> (Dyn -> Bool) -> (Dyn -> { success : Bool, error : String })
    | doc m%"
      Convert a simple boolean predicate into a predicate supporting error messages
    "%
    = fun error' pred x =>
      { success = pred x, error = error' },

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
            let { success, error } = pred x in
            if success then
              { success = true, error = "" }
            else
              std.record.update "error" error acc | { success : Bool, error : String }
        )
        { success = false, error = "you should never see this" },

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
            let { success, error = error' } = pred x in
            if !success then
              { success = false, error = error' }
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

  # dependentSchemas
  #   : { _ : Dyn -> { success : Bool, error : String } } -> Dyn -> { success : Bool, error : String }
  #   | doc m%"
  #     Cf. [https://datatracker.ietf.org/doc/html/draft-bhutton-json-schema-01#section-10.2.2.4]
  #   "%
  #   = fun preds x =>
  #     if std.typeof x != 'Record then
  #       { success = true, error = "" }
  #     else
  #       x
  #       |> (
  #         preds
  #         |> std.record.fields
  #         |> std.array.filter (fun field => std.record.has_field field (x | { _ : Dyn }))
  #         |> std.array.map (fun field => preds."%{field}")
  #         |> allOf
  #       ),

  arrayOf
    : (Dyn -> { success : Bool, error : String }) -> Dyn -> { success : Bool, error : String }
    | doc m%"
      Apply a predicate to all elements of an array, succeeding if all
      applications succeed. If the value isn't an array, fail.
    "%
    = fun pred x =>
      if !std.is_array x then
        { success = false, error = "value isn't an array" }
      else
        let x = x | Array Dyn in
        x
        |> std.array.fold_right
          (
            fun x acc =>
              let { success, error = error' } = pred x in
              if !success then
                { success = false, error = error' }
              else
                acc
          )
          { success = true, error = "" },

  items
    : Array (Dyn -> { success : Bool, error : String }) -> Dyn -> { success : Bool, error : String }
    | doc m%"
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.4.1]
    "%
    = fun preds x =>
      if !std.is_array x then
        { success = true, error = "" }
      else
        let x = x | Array Dyn in
        let length_to_check = std.number.min (std.array.length preds) (std.array.length x) in
        std.array.range 0 length_to_check
        |> std.array.fold_right
          (
            fun i acc =>
              let { success, error = error' } = (std.array.at i preds) (std.array.at i x) in
              if !success then
                { success = false, error = error' }
              else
                acc
          )
          { success = true, error = "" },

  additionalItems
    : (Dyn -> { success : Bool, error : String }) -> Number -> Dyn -> { success : Bool, error : String }
    | doc m%"
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.4.2]
    "%
    = fun pred start x =>
      if !std.is_array x then
        { success = true, error = "" }
      else
        let x = x | Array Dyn in
        let value_length = std.array.length x in
        if start >= value_length then
          { success = true, error = "" }
        else
          (std.array.slice start value_length x | Dyn)
          |> arrayOf pred,

  maxItems
    : Number -> Dyn -> { success : Bool, error : String }
    | doc m%"
      `maxItems n x` fails if `x` is an array of length strictly greater than `n` and succeeds otherwise.
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.4.3]
    "%
    = fun n x =>
      if !std.is_array x then
        { success = true, error = "" }
      else if std.array.length (x | Array Dyn) > n then
        { success = false, error = "array is longer than %{std.string.from_number n}" }
      else
        { success = true, error = "" },

  isType
    : [|
      'Number,
      'Bool,
      'String,
      'Array,
      'Record
    |] -> (Dyn -> { success : Bool, error : String })
    = fun t => from_simple_predicate "value is not of type %{std.string.from_enum t}" (fun x => std.typeof x == t),
}

