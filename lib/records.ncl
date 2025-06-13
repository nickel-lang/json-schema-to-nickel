let ErrData = { message | optional | String, notes | optional | Array String, blame_location | optional | Dyn } in
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
      A contract asserting that a record has certain fields.

      # Examples


      ```nickel multiline
      { foo = 1, bar = 2 } | Required [ "foo" ]
      # => { foo = 1, bar = 2 }

      { foo = 1, bar = 2 } | Required [ "baz" ]
      # => error: missing required field baz

      "baz" | Required [ "baz" ]
      # => error: expected a record
      ```
    "%
    = fun fields =>
      std.contract.from_validator (fun value =>
        if !std.is_record value then
          'Error { message = "expected a record" }
        else
          fields
          |> std.array.try_fold_left
            (fun acc field =>
              if !std.record.has_field field value then
                'Error { message = "missing required field %{field}" }
              else
                'Ok acc
            )
            null
          |> match {
            'Error e => 'Error e,
            'Ok _ => 'Ok
          }
      ),

  Record
    | { properties | { _ | Dyn }, required | { _ | Dyn }, patterns | { _ | Dyn }, additional | [| 'None, 'Some Dyn |] } -> Dyn
    | doc m%"
      A contract for checking JSON Schema object properties.

      In `Record { properties, required, patterns, additional }`, properties and patterns
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
      { foo = 1 } | Record { properties = { foo = Number }, required = {}, patterns = {}, additional = 'None }
      # => { foo = 1 }

      # Properties aren't required to be present.
      { } | Record { properties = { foo = Number }, required = {}, patterns = {}, additional = 'None }
      # => { }

      # Properties aren't required to be present.
      { } | Record { properties = { foo = Number }, required = { foo = null }, patterns = {}, additional = 'None }
      # => error

      # If something in properties also matches a pattern, both contracts are applied.
      { foo = 1.1 } | Record { properties = { foo = Number }, required = {}, patterns = { "." = std.number.Integer }, additional = 'None }
      # => error: expected an integer

      # additional being 'None means that unmatched fields are forbidden
      { bar = 1 } | Record { properties = { foo = Number }, required = {}, patterns = {}, additional = 'None }
      # => error: extra fields

      # fields that are matched by a pattern don't count as "additional"
      { bar = 1 } | Record { properties = { foo = Number }, required = {}, patterns = { "." = Number }, additional = 'None }
      # => { bar = 1 }

      # additional being 'Some applies the extra contract to unmatched fields
      { bar = 1 } | Record { properties = { foo = Number }, required = {}, patterns = {}, additional = 'Some String }
      # => error

      # Inner contracts are propagated
      { foo = { bar = 1 }} | Record { properties = { foo = { bar | String } }, required = {}, patterns = {}, additional = 'None }
      # => error: contract broken by the value of `bar`

      # Inner contracts aren't evaluated eagerly
      ({ foo = { bar = 1 }, baz = 2} | Record { properties = { foo = { bar | String } }, required = {}, patterns = {}, additional = 'Some Dyn }).baz
      # => 2
      ```
    "%
    = fun { properties, required, patterns, additional } =>
      std.contract.custom (fun label x =>
        if !std.is_record x then
          'Error { message = "expected a record" }
        else
          let x = x | { _ | Dyn } in
          let required = required | { _ | Dyn } in
          # Build a dict of everything checked by "properties". The field values
          # in this dict have any inner contracts applied.
          let checked_properties
            : [| 'Ok { _ | Dyn }, 'Error ErrData |]
            =
              std.record.to_array (properties | { _ : Dyn })
              |> std.array.try_fold_left
                (fun acc { field, value = contract } =>
                  if !std.record.has_field field x then
                    if std.record.has_field field required then
                      'Error ({ message = "missing required field `%{field}`" } | ErrData)
                    else
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
          # Build a dict of everything checked by "patterns". The field values
          # in this dict have any inner contracts applied. This can have overlap
          # with "checked_properties"; they'll get merged together later.
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

  DependentFields
    | { _ | Array String } -> Dyn
    | doc m%"
      A contract for asserting that if some fields are present, other fields are
      also present.

      Specifically, `DependentFields { foo = ["bar", "baz"] }` is a contract
      asserting that if the field `foo` is present then the fields `bar` and
      `baz` are also present. This corresponds to the part of JSON schema's
      [dependencies](https://json-schema.org/draft-07/draft-handrews-json-schema-validation-01#rfc.section.6.5.7)
      keyword in which the values are arrays.

      This is an eager contract.

      ```nickel multiline
      { foo = 1, bar = 2 } | DependentFields { foo = ["bar"] }

      { foo = 1, baz = 2 } | DependentFields { foo = ["bar"] }
      # => error: expected field bar

      { baz = 2 } | DependentFields { foo = ["bar"] }
      ```
    "%
    = fun dependencies =>
      std.contract.from_validator (fun value =>
        if !std.is_record value then
          'Error { message = "expected a record" }
        else
          let must_have_fields : Array String -> { _ : Dyn } -> [| 'Ok, 'Error ErrData |] = fun fields x =>
            fields
            |> std.array.fold_right
              (fun field acc =>
                if !std.record.has_field field x then
                  'Error ({ message = "expected field %{field}" } | ErrData)
                else
                  acc
              )
              'Ok
          in
          dependencies
          |> std.record.to_array
          |> std.array.fold_right
            (fun { field, value = dependent_fields } acc =>
              if std.record.has_field field value then
                must_have_fields dependent_fields value
                |> match {
                  'Ok => acc,
                  'Error e => 'Error (e & { notes = ["because field %{field} was present"] }),
                }
              else
                acc
            )
            'Ok
      ),

  DependentContracts
    | { _ | Dyn } -> Dyn
    | doc m%"
      A contract for asserting that if some fields are present then the entire
      record satisfies a contract.

      Specifically, `DependentContracts { foo = C }` is a contract asserting that
      the value is a record, and if the value contains a field named `foo` then
      the entire record satisfies the contract `C`.

      `DependentContracts` calls `std.contract.check` for each of the provided
      contracts. In particular, if `C` is fully eager then so is
      `DependentContracts { foo = C }`.

      # Examples

      ```nickel multiline
      { foo = 1, bar = 2 } | DependentContracts { foo = { bar | Number, .. } }

      { foo = 1, bar = 2 } | DependentContracts { foo = { bar | Number } }
      # => error: extra field `foo`

      # This one doesn't fail: the inner contract never gets triggered because there's no `baz` field
      { foo = 1, bar = 2 } | DependentContracts { baz = { bar | Number } }

      1 | DependentContracts { baz = { bar | Number } }
      # => error: expected a record
      ```
    "%
    = fun deps =>
      std.contract.custom (fun label value =>
        if !std.is_record value then
          'Error { message = "expected a record" }
        else
          deps
          |> std.record.to_array
          # In this fold, the accumulator is `value` wrapped with whatever delayed
          # checks the dependent contracts have applied so far.
          |> std.array.try_fold_left
            (fun acc { field, value = contract } =>
              if std.record.has_field field value then
                std.contract.check contract label acc
              else
                'Ok acc
            )
            value
      ),

  PropertyNames
    | Dyn -> Dyn
    | doc m%"
      `PropertyNames C` is a contract asserting that a value is a record and all of
      its field names satisfy the contract `C`.

      `PropertyNames C` is an eager contract for any `C`. Any lazy parts of `C` will
      be ignored.

      ```nickel multiline
      { foo = 1 } | PropertyNames std.string.NonEmpty

      { "1" = 1 } | PropertyNames std.string.NumberLiteral

      { foo = 1 } | PropertyNames std.string.NumberLiteral
      # => error: invalid number literal
      ```
    "%
    = fun contract =>
      std.contract.custom (fun label value =>
        if !std.is_record value then
          'Error { message = "expected a record" }
        else
          std.record.fields value
          |> std.array.try_fold_left
            (fun acc field_name =>
              std.contract.check contract label field_name
              |> match {
                'Ok _ => 'Ok value,
                'Error e => 'Error e,
              }
            )
            ('Ok value)
      ),
}
