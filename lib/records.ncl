let ErrData = { message | optional | String, notes | optional | Array String } in
{
  MaxProperties
    | Number -> Dyn
    | doc m%"
      A contract that checks whether a record has at most a certain number of properties.

      # Examples

      ```nickel multiline
      { foo = 1, bar = 2 } | MaxProperties 2

      { foo = 1, bar = 2 } | MaxProperties 1
      # => error: expected a record with at most 1 fields
      ```
    "%
    = fun n =>
      std.contract.from_validator (fun value =>
        if !std.is_record value then
          'Error { message = "expected a record" }
        else if std.record.length value > n then
          'Error { message = "expected a record with at most %{std.string.from_number n} fields" }
        else
          'Ok
      ),

  MinProperties
    | Number -> Dyn
    | doc m%"
      A contract that checks whether a record has at least a certain number of properties.

      # Examples

      ```nickel multiline
      { foo = 1, bar = 2 } | MinProperties 2

      { foo = 1, bar = 2 } | MinProperties 3
      # => error: expected a record with at least 3 fields
      ```
    "%
    = fun n =>
      std.contract.from_validator (fun value =>
        if !std.is_record value then
          'Error { message = "expected a record" }
        else if std.record.length value < n then
          'Error { message = "expected a record with at least %{std.string.from_number n} fields" }
        else
          'Ok
      ),

  Required
    | Array String -> Dyn
    | doc m%"
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.5.3]
    "%,
  # = fun fields x =>
  #   if !std.is_record x then
  #     'Ok
  #   else
  #     fields
  #     |> std.array.fold_right
  #       (fun field acc =>
  #         if !std.record.has_field field (x | { _ : Dyn }) then
  #           error_lib.mk_error "missing required field %{field}"
  #         else
  #           acc
  #       )
  #       'Ok,

  Record
    | { properties | { _ | Dyn }, patterns | { _ | Dyn }, additional | [| 'None, 'Some Dyn |] } -> Dyn
    | doc m%"
      A contract for checking JSON Schema object properties.

      In `Record { properties, patterns, additional }`, properties and patterns
      should be dicts containing contracts as values. These contracts will be
      applied to the appropriate fields of your record. `additional` can be
      `None` (in which case fields not matching `properties` or `patterns` are
      forbidden) or `Some contract` (in which case `contract` will be applied to
      every field not matched by `properties` and `patterns`).

      The contract returned by `Record` is sort of eager: it will call
      `std.contract.check` on the appropriate field values, and then it will
      propagate any lazy contracts returned by `std.contract.check`. This makes
      `Record` a little more eager than standard record contracts, and if all
      the contracts you provide are eager then `Record` will be fully eager.

      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.5]

      # Examples

      ```nickel multiline
      { foo = 1 } | Record { properties = { foo = Number }, patterns = {}, additional = 'None }
      # => { foo = 1 }

      # Properties aren't required to be present.
      { } | Record { properties = { foo = Number }, patterns = {}, additional = 'None }
      # => { }

      # If something in properties also matches a pattern, both contracts are applied.
      { foo = 1.1 } | Record { properties = { foo = Number }, patterns = { "." = std.number.Integer }, additional = 'None }
      # => error: expected an integer

      # additional being 'None means that unmatched fields are forbidden
      { bar = 1 } | Record { properties = { foo = Number }, patterns = {}, additional = 'None }
      # => error: extra fields

      # fields that are matched by a pattern don't count as "additional"
      { bar = 1 } | Record { properties = { foo = Number }, patterns = { "." = Number }, additional = 'None }
      # => { bar = 1 }

      # additional being 'Some applies the extra contract to unmatched fields
      { bar = 1 } | Record { properties = { foo = Number }, patterns = {}, additional = 'Some String }
      # => error

      # Inner contracts are propagated
      { foo = { bar = 1 }} | Record { properties = { foo = { bar | String } }, patterns = {}, additional = 'None }
      # => error: contract broken by the value of `bar`

      # Inner contracts aren't evaluated eagerly
      ({ foo = { bar = 1 }, baz = 2} | Record { properties = { foo = { bar | String } }, patterns = {}, additional = 'Some Dyn }).baz
      # => 2
      ```
    "%
    = fun { properties, patterns, additional } =>
      std.contract.custom (fun label x =>
        if !std.is_record x then
          'Error { message = "expected a record" }
        else
          let x = x | { _ | Dyn } in
          let checked_properties
            : [| 'Ok { _ | Dyn }, 'Error ErrData |]
            =
              std.record.to_array (properties | { _ : Dyn })
              |> std.array.try_fold_left
                (fun acc { field, value = contract } =>
                  if !std.record.has_field field x then
                    'Ok acc
                  else
                    std.contract.check contract label x."%{field}"
                    |> match {
                      'Ok checked_value => 'Ok (std.record.insert field checked_value acc),
                      'Error e => 'Error e,
                    }
                )
                ({} | { _ | Dyn })
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
          let checked_pattern_properties
            : [| 'Ok { _ | Dyn }, 'Error ErrData |]
            =
              std.record.to_array (patterns | { _ : Dyn })
              |> std.array.try_fold_left
                (fun acc { field = pattern, value = contract } =>
                  matching_fields pattern
                  |> std.record.to_array
                  |> std.array.try_fold_left
                    (fun acc { field, value } =>
                      std.contract.check contract label value
                      |> match {
                        'Ok checked_value => 'Ok (std.record.insert field checked_value acc),
                        'Error e => 'Error e,
                      }
                    )
                    ({} | { _ | Dyn })
                  |> match {
                    'Ok checked_dict => 'Ok ((acc & checked_dict) | { _ | Dyn }),
                    'Error e => 'Error e,
                  }
                )
                ({} | { _ | Dyn })
          in
          [checked_properties, checked_pattern_properties]
          |> match {
            ['Error e, _] => 'Error e,
            [_, 'Error e] => 'Error e,
            ['Ok checked_properties, 'Ok checked_pattern_properties] =>
              let remaining_fields : { _ | Dyn } =
                (std.record.fields (checked_properties | { _ : Dyn }) @ std.record.fields (checked_pattern_properties | { _ : Dyn }))
                |> std.array.fold_left
                  (fun acc field =>
                    if !std.record.has_field field acc then
                      acc
                    else
                      std.record.remove field acc
                  )
                  x
              in
              let checked_additional
                : [| 'Ok { _ | Dyn }, 'Error ErrData |]
                =
                  (additional | [| 'None, 'Some Dyn |])
                  |> match {
                    'None =>
                      if !std.record.is_empty remaining_fields then
                        'Error ({ message = "extra fields %{std.serialize 'Json (std.record.fields remaining_fields | Dyn)}" } | ErrData)
                      else
                        'Ok ({} | { _ | Dyn }),
                    'Some contract =>
                      std.record.to_array remaining_fields
                      |> std.array.try_fold_left
                        (fun acc { field, value } =>
                          std.contract.check contract label value
                          |> match {
                            'Ok checked_value => 'Ok (std.record.insert field checked_value acc),
                            'Error e => 'Error e,
                          }
                        )
                        ({} | { _ | Dyn })
                  }
              in
              checked_additional
              |> match {
                'Error e => 'Error e,
                'Ok checked_additional => 'Ok (checked_properties & checked_pattern_properties & checked_additional)
              }
          }
      ),

  # dependencies
  #   : { _ : Dyn } -> Dyn -> [|
  #     'Ok,
  #     'Error { message : String, source : Array [| 'Array Number, 'Field String |] }
  #   |]
  #   | doc m%"
  #     Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.5.7]
  #   "%
  #   =
  #     let mustHaveFields
  #       : Array String -> { _ : Dyn } -> [|
  #         'Ok,
  #         'Error { message : String, source : Array [| 'Array Number, 'Field String |] }
  #       |]
  #       = fun fields x =>
  #         fields
  #         |> std.array.fold_right
  #           (fun field acc =>
  #             if !std.record.has_field field x then
  #               error_lib.mk_error "expected field `%{field}`"
  #             else
  #               acc
  #           )
  #           'Ok
  #     in
  #     fun deps x =>
  #       if !std.is_record x then
  #         'Ok
  #       else
  #         let x = x | { _ : Dyn } in
  #         deps
  #         |> std.record.to_array
  #         |> std.array.fold_right
  #           (fun { field, value } acc =>
  #             if !std.record.has_field field x then
  #               acc
  #             else
  #               let result =
  #                 if std.is_array value then
  #                   mustHaveFields (value | Array String) x
  #                 else
  #                   let pred = value | Dyn -> [| 'Ok, 'Error { message : String, source : Array [| 'Array Number, 'Field String |] } |] in
  #                   pred (x | Dyn)
  #               in
  #               result
  #               |> match {
  #                 'Ok => acc,
  #                 'Error error =>
  #                   'Error {
  #                     message = "dependency of `%{field}` failed: %{error.message}",
  #                     source = std.array.prepend ('Field field) error.source,
  #                   },
  #               }
  #           )
  #           'Ok,

  # TODO: replace with std.record.FieldsMatch once that's in a nickel release
  FieldsMatch | String -> Dyn = fun regex =>
    let is_match = std.string.is_match regex in
    std.contract.from_validator (fun x =>
      if !std.is_record x then
        'Error { message = "expected a record" }
      else
        let bad_field_names =
          std.record.fields x
          |> std.array.filter (fun k => !(is_match k))
          |> std.array.map (fun k => m%""${k}""%)
        in
        if std.array.length bad_field_names > 0 then
          'Error {
            message = "invalid field names %{std.string.join "," bad_field_names}",
            notes = ["expected fields to match %{regex}"]
          }
        else
          'Ok
    ),
}
