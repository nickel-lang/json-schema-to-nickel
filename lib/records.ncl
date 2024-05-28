{
  maxProperties
    : Number -> Dyn -> [| 'Ok, 'Err String |]
    | doc m%"
      `maxProperties n x` fails if `x` is a record containing stricly more than `n` fields.
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.5.1]
    "%
    = fun n x =>
      if !std.is_record x then
        'Ok
      else if std.record.length (x | { _ : Dyn }) > n then
        'Err "record contains more than %{std.string.from_number n} fields"
      else
        'Ok,

  minProperties
    : Number -> Dyn -> [| 'Ok, 'Err String |]
    | doc m%"
      `minProperties n x` fails if `x` is a record containing stricly less than `n` fields.
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.5.1]
    "%
    = fun n x =>
      if !std.is_record x then
        'Ok
      else if std.record.length (x | { _ : Dyn }) < n then
        'Err "record contains fewer than %{std.string.from_number n} fields"
      else
        'Ok,

  required
    : Array String -> Dyn -> [| 'Ok, 'Err String |]
    | doc m%"
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.5.3]
    "%
    = fun fields x =>
      if !std.is_record x then
        'Ok
      else
        fields
        |> std.array.fold_right
          (
            fun field acc =>
              if !std.record.has_field field (x | { _ : Dyn }) then
                'Err "missing required field %{field}"
              else
                acc
          )
          'Ok,

  record
    : { _ : Dyn -> [| 'Ok, 'Err String |] } -> { _ : Dyn -> [| 'Ok, 'Err String |] } -> Bool -> (Dyn -> [| 'Ok, 'Err String |]) -> Dyn -> [| 'Ok, 'Err String |]
    | doc m%"
      `record properties patternProperties additionalAllowed
      additionalProperties x` is a combination of the `properties`,
      `patternProperties` and `additionalProperties` validation keywords in
      JSON schema.

      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.5]
    "%
    = fun properties patternProperties additionalAllowed additionalProperties x =>
      if !std.is_record x then
        'Ok
      else
        let x = x | { _ : Dyn } in
        let check_properties : { result : [| 'Ok, 'Err String |], checked : { _ : Bool } }
          =
            std.record.to_array properties
            |> std.array.fold_right
              (
                fun { field, value } acc =>
                  if !std.record.has_field field x then
                    acc
                  else
                    value x."%{field}" |> match {
                      'Err msg => { result = 'Err m%"field `%{field}` didn't validate: %{msg}"%, checked : { _ : Bool } = {} },
                      'Ok => { result = acc.result, checked = std.record.insert field true acc.checked },
                    }
              )
              { result = 'Ok, checked = {} }
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
        let check_pattern_properties : { result: [| 'Ok, 'Err String |], checked : { _ : Bool } }
          =
            std.record.to_array patternProperties
            |> std.array.fold_right
              (
                fun { field = pattern, value = pred } acc =>
                  matching_fields pattern
                  |> std.record.to_array
                  |> std.array.fold_right
                    (
                      fun { field, value } acc =>
                        pred value |> match {
                          'Err msg => { result = 'Err m%"field `%{field}` didn't validate: %{msg}"%, checked : { _ : Bool } = {} },
                          'Ok => { result = acc.result, checked = std.record.insert field true acc.checked },
                        }
                    )
                    { result = 'Ok, checked : { _ : Bool } = {} }
                  |> match {
                    r@{ result = 'Err msg, .. } => r,
                    { result = 'Ok, checked = ch } => {
                      result = acc.result,
                      checked =
                        std.record.fields ch
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
                  }
              )
              { result = 'Ok, checked : { _ : Bool } = {} }
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
        let check_additional_properties : [| 'Ok, 'Err String |]
          =
            if !additionalAllowed && !std.record.is_empty remaining_fields then
              'Err "extra fields %{std.serialize 'Json (std.record.fields remaining_fields | Dyn)}"
            else
              std.record.to_array remaining_fields
              |> std.array.fold_right
                (
                  fun { field, value } acc =>
                    additionalProperties value |> match {
                      'Err msg => 'Err m%"field `%{field}` didn't validate: %{msg}"%,
                      'Ok => acc,
                    }
                )
                'Ok
          in
        check_properties.result |> match {
          'Err msg => check_properties.result,
          'Ok => check_pattern_properties.result |> match {
            'Err msg => check_pattern_properties.result,
            'Ok => check_additional_properties,
          }
        },

  propertyNames
    : (Dyn -> [| 'Ok, 'Err String |]) -> Dyn -> [| 'Ok, 'Err String |]
    | doc m%"
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.5.8]
    "%
    = fun pred x =>
      if !std.is_record x then
        'Ok
      else
        (x | { _ : Dyn })
        |> std.record.fields
        |> std.array.fold_right
          (
            fun field acc =>
              pred (field | Dyn) |> match {
                'Err msg => 'Err "field `%{field}` did not validate against `propertyNames` schema",
                'Ok => acc,
              }
          )
          'Ok,

  dependencies
    : { _ : Dyn } -> Dyn -> [| 'Ok, 'Err String |]
    | doc m%"
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.5.7]
    "%
    =
      let mustHaveFields : Array String -> { _ : Dyn } -> [| 'Ok, 'Err String |]
        = fun fields x =>
          fields
          |> std.array.fold_right
            (
              fun field acc =>
                if !std.record.has_field field x then
                  'Err "expected field `%{field}`"
                else
                  acc
            )
            'Ok
        in
      fun deps x =>
        if !std.is_record x then
          'Ok
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
                      let pred = value | Dyn -> [| 'Ok, 'Err String |] in
                      pred (x | Dyn)
                  in
                  result |> match {
                    'Err msg => 'Err "dependency of `%{field}` failed: %{msg}",
                    'Ok => acc,
                  }
            )
            'Ok,
}

