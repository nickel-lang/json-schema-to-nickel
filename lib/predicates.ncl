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
              let result = pred x in
              if !result.success then
                result
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
              let result = (std.array.at i preds) (std.array.at i x) in
              if !result.success then
                result
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

  minItems
    : Number -> Dyn -> { success : Bool, error : String }
    | doc m%"
      `minItems n x` fails if `x` is an array of length strictly smaller than `n` and succeeds otherwise.
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.4.4]
    "%
    = fun n x =>
      if !std.is_array x then
        { success = true, error = "" }
      else if std.array.length (x | Array Dyn) < n then
        { success = false, error = "array is longer than %{std.string.from_number n}" }
      else
        { success = true, error = "" },

  uniqueItems
    : Dyn -> { success : Bool, error : String }
    | doc m%"
      Succeeds for any array if its elements are pairwise distinct.
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.4.5]
    "%
    =
      let find_duplicate
        : Array Dyn -> { has_duplicate : Bool, duplicate : Dyn, seen : { _ : Bool } }
        | doc m%"
            Try to find a duplicate in an array of Nickel values. For
            efficienyc, this function uses JSON serialization to index into
            a Nickel record. Consequently all values in the array must be
            serializable to JSON.
          "%
        =
          std.array.fold_right
            (
              fun elt acc =>
                if acc.has_duplicate then
                  acc
                else
                  let index = std.serialize 'Json elt in
                  if std.record.has_field index acc.seen then
                    { has_duplicate = true, duplicate = elt, seen = acc.seen }
                  else
                    { has_duplicate = false, duplicate = null, seen = std.record.insert index true acc.seen }
            )
            { has_duplicate = false, duplicate = null, seen = {} }
        in
      fun x =>
        if !std.is_array x then
          { success = true, error = "" }
        else
          let { has_duplicate, duplicate, .. } = find_duplicate (x | Array Dyn)
          in
          if has_duplicate then
            { success = false, error = "duplicate found: %{std.serialize 'Json duplicate}" }
          else
            { success = true, error = "" },

  maxProperties
    : Number -> Dyn -> { success : Bool, error : String }
    | doc m%"
      `maxProperties n x` fails if `x` is a record containing stricly more than `n` fields.
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.5.1]
    "%
    = fun n x =>
      if !std.is_record x then
        { success = true, error = "" }
      else if std.record.length (x | { _ : Dyn }) > n then
        { success = false, error = "record contains more than %{std.string.from_number n} fields" }
      else
        { success = true, error = "" },

  minProperties
    : Number -> Dyn -> { success : Bool, error : String }
    | doc m%"
      `minProperties n x` fails if `x` is a record containing stricly less than `n` fields.
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.5.1]
    "%
    = fun n x =>
      if !std.is_record x then
        { success = true, error = "" }
      else if std.record.length (x | { _ : Dyn }) < n then
        { success = false, error = "record contains fewer than %{std.string.from_number n} fields" }
      else
        { success = true, error = "" },

  required
    : Array String -> Dyn -> { success : Bool, error : String }
    | doc m%"
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.5.3]
    "%
    = fun fields x =>
      if !std.is_record x then
        { success = true, error = "" }
      else
        fields
        |> std.array.fold_right
          (
            fun field acc =>
              if !std.record.has_field field (x | { _ : Dyn }) then
                { success = false, error = "missing required field %{field}" }
              else
                acc
          )
          { success = true, error = "" },

  record
    : { _ : Dyn -> { success : Bool, error : String } } -> { _ : Dyn -> { success : Bool, error : String } } -> Bool -> (Dyn -> { success : Bool, error : String }) -> Dyn -> { success : Bool, error : String }
    | doc m%"
      `record properties patternProperties additionalAllowed
      additionalProperties x` is a combination of the `properties`,
      `patternProperties` and `additionalProperties` validation keywords in
      JSON schema.

      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.5]
    "%
    = fun properties patternProperties additionalAllowed additionalProperties x =>
      if !std.is_record x then
        { success = true, error = "" }
      else
        let x = x | { _ : Dyn } in
        let check_properties : { success : Bool, error : String, checked : { _ : Bool } }
          =
            std.record.to_array properties
            |> std.array.fold_right
              (
                fun { field, value } acc =>
                  if !std.record.has_field field x then
                    acc
                  else
                    let result = value x."%{field}" in
                    if !result.success then
                      { success = false, error = m%"field "%{field}" didn't validate: %{result.error}"%, checked : { _ : Bool } = {} }
                    else
                      { success = acc.success, error = acc.error, checked = std.record.insert field true acc.checked }
              )
              { success = true, error = "", checked = {} }
          in
        let matching_fields : String -> { _ : Dyn }
          = fun pattern =>
            let matcher = std.string.is_match pattern
            in
            std.record.to_array x
            |> std.array.fold_left
              (
                fun acc { field, value } =>
                  if matcher field then
                    std.record.insert field value acc
                  else
                    acc
              )
              {}
          in
        let check_pattern_properties : { success : Bool, error : String, checked : { _ : Bool } }
          =
            std.record.to_array patternProperties
            |> std.array.fold_right
              (
                fun { field = pattern, value = pred } acc =>
                  let result =
                    matching_fields pattern
                    |> std.record.to_array
                    |> std.array.fold_right
                      (
                        fun { field, value } acc =>
                          let result = pred value in
                          if !result.success then
                            { success = false, error = m%"field "%{field}" didn't validate: %{result.error}"%, checked : { _ : Bool } = {} }
                          else
                            { success = acc.success, error = acc.error, checked = std.record.insert field true acc.checked }
                      )
                      { success = true, error = "", checked : { _ : Bool } = {} }
                  in
                  if !result.success then
                    result
                  else
                    {
                      success = acc.success,
                      error = acc.error,
                      checked =
                        std.record.fields result.checked
                        |> std.array.fold_left
                          (
                            fun r field =>
                              if !std.record.has_field field r then
                                std.record.insert field true r
                              else
                                r
                          )
                          acc.checked
                    }
              )
              { success = true, error = "", checked : { _ : Bool } = {} }
          in
        let remaining_fields =
          (std.record.fields check_properties.checked @ std.record.fields check_pattern_properties.checked)
          |> std.array.fold_left
            (
              fun acc field =>
                if !std.record.has_field field acc then
                  acc
                else
                  std.record.remove field acc
            )
            x
        in
        let check_additional_properties : { success : Bool, error : String }
          =
            if !additionalAllowed && !std.record.is_empty remaining_fields then
              { success = false, error = "extra fields %{std.serialize 'Json (std.record.fields remaining_fields | Dyn)}" }
            else
              std.record.to_array remaining_fields
              |> std.array.fold_right
                (
                  fun { field, value } acc =>
                    let result = additionalProperties value in
                    if !result.success then
                      { success = false, error = m%"field "%{field}" didn't validate: %{result.error}"% }
                    else
                      acc
                )
                { success = true, error = "" }
          in
        if !check_properties.success then
          { success = false, error = check_properties.error }
        else if !check_pattern_properties.success then
          { success = false, error = check_pattern_properties.error }
        else
          check_additional_properties,

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

