let error_lib = import "./error.ncl" in

{
  maxProperties
    : Number -> Dyn -> [|
      'Ok,
      'Error { message : String, source : Array [| 'Array Number, 'Field String |] }
    |]
    | doc m%"
      `maxProperties n x` fails if `x` is a record containing stricly more than `n` fields.
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.5.1]
    "%
    = fun n x =>
      if !std.is_record x then
        'Ok
      else if std.record.length (x | { _ : Dyn }) > n then
        'Error {
          message = "record contains more than %{std.string.from_number n} fields",
          source = [],
        }
      else
        'Ok,

  minProperties
    : Number -> Dyn -> [|
      'Ok,
      'Error { message : String, source : Array [| 'Array Number, 'Field String |] }
    |]
    | doc m%"
      `minProperties n x` fails if `x` is a record containing stricly less than `n` fields.
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.5.1]
    "%
    = fun n x =>
      if !std.is_record x then
        'Ok
      else if std.record.length (x | { _ : Dyn }) < n then
        'Error {
          message = "record contains fewer than %{std.string.from_number n} fields",
          source = [],
        }
      else
        'Ok,

  required
    : Array String -> Dyn -> [|
      'Ok,
      'Error { message : String, source : Array [| 'Array Number, 'Field String |] }
    |]
    | doc m%"
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.5.3]
    "%
    = fun fields x =>
      if !std.is_record x then
        'Ok
      else
        fields
        |> std.array.fold_right
          (fun field acc =>
            if !std.record.has_field field (x | { _ : Dyn }) then
              'Error {
                message = "missing required field %{field}",
                source = [],
              }
            else
              acc
          )
          'Ok,

  record
    : {
      _ : Dyn -> [|
        'Ok,
        'Error { message : String, source : Array [| 'Array Number, 'Field String |] }
      |]
    } -> {
      _ : Dyn -> [|
        'Ok,
        'Error { message : String, source : Array [| 'Array Number, 'Field String |] }
      |]
    } -> Bool -> (
      Dyn -> [|
        'Ok,
        'Error { message : String, source : Array [| 'Array Number, 'Field String |] }
      |]
    ) -> Dyn -> [|
      'Ok,
      'Error { message : String, source : Array [| 'Array Number, 'Field String |] }
    |]
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
        let check_properties : { result : [| 'Ok, 'Error { message : String, source : Array [| 'Array Number, 'Field String |] } |], checked : { _ : Bool } } =
          std.record.to_array properties
          |> std.array.fold_right
            (fun { field, value } acc =>
              if !std.record.has_field field x then
                acc
              else
                value x."%{field}"
                |> error_lib.at_field field
                |> match {
                  'Ok => { result = acc.result, checked = std.record.insert field true acc.checked },
                  error @ ('Error _) => { result = error, checked = {} },
                }
            )
            { result = 'Ok, checked = {} }
        in
        let matching_fields : String -> { _ : Dyn } = fun pattern =>
          let matcher = std.string.is_match pattern
          in
          std.record.to_array x
          |> std.array.fold_left
            (fun acc { field, value } =>
              if matcher field then
                std.record.insert field value acc
              else
                acc
            )
            {}
        in
        let check_pattern_properties
          : {
            result
              : [|
                'Ok,
                'Error { message : String, source : Array [| 'Array Number, 'Field String |] }
              |],
            checked : { _ : Bool }
          }
          =
            std.record.to_array patternProperties
            |> std.array.fold_right
              (fun { field = pattern, value = pred } acc =>
                matching_fields pattern
                |> std.record.to_array
                |> std.array.fold_right
                  (fun { field, value } acc =>
                    pred value
                    |> match {
                      'Ok => { result = acc.result, checked = std.record.insert field true acc.checked },
                      error @ ('Error _) => { result = error_lib.at_field field error, checked : { _ : Bool } = {} },
                    }
                  )
                  { result = 'Ok, checked : { _ : Bool } = {} }
                |> match {
                  r @ { result = 'Error _, .. } => r,
                  { result = 'Ok, checked = ch } =>
                    {
                      result = acc.result,
                      checked =
                        std.record.fields ch
                        |> std.array.fold_left
                          (fun r field =>
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
            (fun acc field =>
              if !std.record.has_field field acc then
                acc
              else
                std.record.remove field acc
            )
            x
        in
        let check_additional_properties
          : [|
            'Ok,
            'Error { message : String, source : Array [| 'Array Number, 'Field String |] }
          |]
          =
            if !additionalAllowed && !std.record.is_empty remaining_fields then
              'Error {
                message = "extra fields %{std.serialize 'Json (std.record.fields remaining_fields | Dyn)}",
                source = [],
              }
            else
              std.record.to_array remaining_fields
              |> std.array.fold_right
                (fun { field, value } acc =>
                  additionalProperties value
                  |> error_lib.at_field field
                )
                'Ok
        in
        check_properties.result
        |> match {
          'Ok =>
            check_pattern_properties.result
            |> match {
              'Ok => check_additional_properties,
              error @ ('Error _) => error,
            },
          error @ ('Error _) => error,
        },

  propertyNames
    : (
      Dyn -> [|
        'Ok,
        'Error { message : String, source : Array [| 'Array Number, 'Field String |] }
      |]
    ) -> Dyn -> [| 'Ok, 'Error { message : String, source : Array [| 'Array Number, 'Field String |] } |]
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
          (fun field acc =>
            pred (field | Dyn)
            |> match {
              'Error error =>
                'Error {
                  message = "field `%{field}` did not validate against `propertyNames` schema: %{error.message}",
                  source = std.array.prepend ('Field field) error.source,
                },
              'Ok => acc,
            }
          )
          'Ok,

  dependencies
    : { _ : Dyn } -> Dyn -> [|
      'Ok,
      'Error { message : String, source : Array [| 'Array Number, 'Field String |] }
    |]
    | doc m%"
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.5.7]
    "%
    =
      let mustHaveFields
        : Array String -> { _ : Dyn } -> [|
          'Ok,
          'Error { message : String, source : Array [| 'Array Number, 'Field String |] }
        |]
        = fun fields x =>
          fields
          |> std.array.fold_right
            (fun field acc =>
              if !std.record.has_field field x then
                'Error {
                  message = "expected field `%{field}`",
                  source = [],
                }
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
            (fun { field, value } acc =>
              if !std.record.has_field field x then
                acc
              else
                let result =
                  if std.is_array value then
                    mustHaveFields (value | Array String) x
                  else
                    let pred = value | Dyn -> [| 'Ok, 'Error { message : String, source : Array [| 'Array Number, 'Field String |] } |] in
                    pred (x | Dyn)
                in
                result
                |> match {
                  'Ok => acc,
                  'Error error =>
                    'Error {
                      message = "dependency of `%{field}` failed: %{error.message}",
                      source = std.array.prepend ('Field field) error.source,
                    },
                }
            )
            'Ok,
}
