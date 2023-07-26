{
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
                      { success = false, error = m%"field `%{field}` didn't validate: %{result.error}"%, checked : { _ : Bool } = {} }
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
                            { success = false, error = m%"field `%{field}` didn't validate: %{result.error}"%, checked : { _ : Bool } = {} }
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
                      { success = false, error = m%"field `%{field}` didn't validate: %{result.error}"% }
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

  propertyNames
    : (Dyn -> { success : Bool, error : String }) -> Dyn -> { success : Bool, error : String }
    | doc m%"
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.5.8]
    "%
    = fun pred x =>
      if !std.is_record x then
        { success = true, error = "" }
      else
        (x | { _ : Dyn })
        |> std.record.fields
        |> std.array.fold_right
          (
            fun field acc =>
              let result = pred (field | Dyn) in
              if !result.success then
                { success = false, error = "field `%{field}` did not validate against `propertyNames` schema" }
              else
                acc
          )
          { success = true, error = "" },

  dependencies
    : { _ : Dyn } -> Dyn -> { success : Bool, error : String }
    | doc m%"
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.5.7]
    "%
    =
      let mustHaveFields : Array String -> { _ : Dyn } -> { success : Bool, error : String }
        = fun fields x =>
          fields
          |> std.array.fold_right
            (
              fun field acc =>
                if !std.record.has_field field x then
                  { success = false, error = "expected field `%{field}`" }
                else
                  acc
            )
            { success = true, error = "" }
        in
      fun deps x =>
        if !std.is_record x then
          { success = true, error = "" }
        else
          let x = x | { _ : Dyn } in
          deps
          |> std.record.to_array
          |> std.array.fold_right
            (
              fun { field, value } acc =>
                if !std.record.has_field field x then
                  acc
                else
                  let result =
                    if std.is_array value then
                      mustHaveFields (value | Array String) x
                    else
                      let pred = value | Dyn -> { success : Bool, error : String } in
                      pred (x | Dyn)
                  in
                  if !result.success then
                    { success = false, error = "dependency of `%{field}` failed: %{result.error}" }
                  else
                    acc
            )
            { success = true, error = "" },
}

