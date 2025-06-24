# DO NOT EDIT
# This file was automatically generated using json-schema-to-nickel
let js2n = {
  Always
    | doc m%"
        A contract that always succeeds. The same as `Dyn`, but with a more JSON-schema name.

        # Examples

        ```nickel multiline
        "foo" | Always

        3 | Always
        ```
      "%
    = Dyn,
  Const
    | Dyn -> Dyn
    | doc m%"
        A more eager version of `std.contract.Equal`.

        # Examples

        ```nickel multiline
        5 | Const 5

        3 | Const 5
        # => error
        ```
      "%
    = fun expected =>
      std.contract.from_validator (fun value =>
        if value == expected then
          'Ok
        else
          'Error { message = "expected %{std.to_string expected}" }
      ),
  Enum
    | Array Dyn -> Dyn
    | doc m%"
        `Enum values x` succeeds if and only if `x` is equal to one of the elements of `values`.

        # Examples

        ```nickel multiline
        "no" | Enum [false, "false", "no", "off"]

        "yes" | Enum [false, "false", "no", "off"]
        # => error
        ```
      "%
    = fun variants =>
      std.contract.from_validator (fun value =>
        let check_equal = fun input variant =>
          (input == variant)
          || (
            (std.is_enum input)
            && (
              ((std.string.from_enum input) == variant)
                | Bool
            )
          )
        in
        if std.array.any (check_equal value) variants then
          'Ok
        else
          'Error {
            message = "expected one of %{std.serialize 'Json variants}"
          }
      ),
  IfThenElse
    | Dyn -> Dyn -> Dyn -> Dyn
    | doc m%"
        `IfThenElse iph zen els` checks the `iph` contract (eagerly). If that
        succeeds, it checks the `zen` contract and otherwise checks the `els`
        contract.

        Only the eager part of the `iph` contract is used to decide which of the
        other contracts is applied. The lazy part of the `iph` contract will be
        applied if the eager part succeeds.

        # Examples

        ```nickel multiline
        6 | IfThenElse Number (number.MultipleOf 2) String

        "hi" | IfThenElse Number (number.MultipleOf 2) String

        true | IfThenElse Number (number.MultipleOf 2) String
        # => error

        # The `zen` branch gets the value that was normalized by TagOrString
        "hi" | IfThenElse std.enum.TagOrString (std.contract.Equal 'hi) Number
        ```
      "%
    = fun iph zen els =>
      std.contract.custom (fun label value =>
        (match {
          'Ok v =>
            std.contract.check zen label v,
          'Error _ =>
            std.contract.check els label value,
        }
        ) (std.contract.check iph label value)
      ),
  Never
    | doc m%"
        A contract that always fails.

        # Examples

        ```nickel multiline
        "foo" | Never
        # => error

        3 | Never
        # => error
      "%
    = std.contract.from_predicate (fun _ => false),
  Null
    | doc m%"
        A contract asserting that a value is null.

        # Examples

        ```nickel multiline
        null | Null

        "foo" | Null
        # => error
      "%
    = std.contract.from_predicate ((fun x0 x1 => x0 == x1) null),
  Nullable
    : Dyn -> Dyn
    | doc m%"
        For a contract `C`, `Nullable C` returns a contract that allows either `C` or a null value.

        # Examples

        ```nickel multiline
        1 | Nullable Number

        null | Nullable Number

        "1" | Nullable Number
        # => error
        ```
      "%
    = fun Contract =>
      std.contract.custom (fun label value =>
        if value == null then
          'Ok value
        else
          std.contract.check Contract label value
      ),
  OneOf
    | Array Dyn -> Dyn
    | doc m%"
        Checks that exactly one of a list of contracts succeeds.

        # Examples

        ```nickel multiline
        "hi" | OneOf [Number, String, Bool]

        "hi" | OneOf [Number, String, std.enum.TagOrString]
        # => error: 2 of the contracts passed

        "hi" | OneOf [Number, std.enum.TagOrString]
        # => 'hi
        ```
      "%
    = fun contracts =>
      std.contract.custom (fun label value =>
        (match {
          'Count n =>
            'Error {
              message = "%{std.to_string
                            n} of the contracts passed, expected precisely one to pass"
            },
          'Ok v =>
            'Ok v,
        }
        ) (
          std.array.fold_right
            (fun ctr acc =>
              (match {
                'Ok val =>
                  (match {
                    'Count 0 =>
                      'Ok val,
                    'Count n =>
                      'Count (n + 1),
                    'Ok _ =>
                      'Count 2,
                  }
                  ) acc,
                'Error e =>
                  acc,
              }
              ) (std.contract.check ctr label value)
            )
            ('Count 0)
            contracts
        )
      ),
  array = {
    ArrayOf
      | Dyn -> Dyn
      | doc m%"
              An eager version of `Array`. `value | ArrayOf C` will succeed if `value` is an array and all of
              its elements satisfy the eager part of the contract `C`.

              # Examples

              ```nickel multiline
              [1, 2, 3] | ArrayOf Number
              # => [1, 2, 3]

              [1, 2, 3] | ArrayOf String
              # => error

              ["a", "b", "c"] | ArrayOf std.enum.TagOrString
              # => ['a, 'b, 'c]
              ```
            "%
      = fun ctr =>
        std.contract.custom (fun label value =>
          if !(std.is_array value) then
            'Error { message = "expected an array" }
          else
            std.array.try_fold_left
              (fun acc elt =>
                (match {
                  'Ok v =>
                    'Ok (acc @ [v]),
                  'Error e =>
                    'Error e,
                }
                ) (std.contract.check ctr label elt)
              )
              []
              value
        ),
    Contains
      | Dyn -> Dyn
      | doc m%"
              A contract checking that an array contains something satisfying another contract.

              For example, `Contains Number` is a contract checking that the value is an array
              and that it contains at least one number.

              This is an eager contract, and it assumes that the inputs are also eager.
              Any lazy parts of the inputs will be ignored.

              # Examples

              ```nickel multiline
              ["hi", true, 1] | Contains Bool
              # => ["hi", true, 1]

              ["hi", true, 1] | Contains (Array Bool)
              # => error

              # Lazy parts of the inner contract are ignored: "hi" does not get transformed
              # to 'hi.
              ["hi", true, 1] | Contains std.enum.TagOrString
              # => ["hi", true, 1]
              ```
            "%
      = fun contract =>
        std.contract.custom (fun label value =>
          if !(std.is_array value) then
            'Error { message = "expected an array" }
          else
            let good_elt = fun elt =>
              (match { 'Ok _ => true, 'Error _ => false, }) (std.contract.check contract label elt)
            in
            if std.array.any good_elt value then
              'Ok value
            else
              'Error { message = "no elements matched" }
        ),
    Items
      | Array Dyn -> Dyn -> Dyn
      | doc m%"
              A contract checking that the first few elements of an array satisfy
              specific contracts, and the remaining elements satisfy another contract.
              For example, `Items [C1 C2] C3` is a contract checking that the value is
              an array, that its first two elements (if present) satisfy `C1` and `C2`
              respectively, and that any other elements satisfy `C3`.

              # Examples

              ```nickel multiline
              [1, 2, "foo", "bar"] | Items [Number, Number] String

              # The value is allowed to be shorter than the list of contracts.
              [1] | Items [Number, Number] String

              [1, "foo", "bar"] | Items [Number, Number] String
              # => error

              # Inner contracts are propagated, but note that the overall `Items` contract
              # is eager if and only if all the contracts you pass in are eager.
              [1, { foo = "str"}, "foo", "bar"] | Items [Number, { foo | Number }] String
              # => error
              ```
            "%
      = fun elt_contracts rest =>
        std.contract.custom (fun label value =>
          if !(std.is_array value) then
            'Error { message = "expected an array" }
          else
            let length_to_check =
              std.number.min
                (std.array.length elt_contracts)
                (std.array.length value)
            in
            let { left, right, } = std.array.split_at length_to_check value
            in
            (match {
              'Error e =>
                'Error e,
              'Ok left_with_contracts =>
                std.array.try_fold_left
                  (fun acc elt =>
                    (match {
                      'Ok v =>
                        'Ok (acc @ [v]),
                      'Error e =>
                        'Error e,
                    }
                    ) (std.contract.check rest label elt)
                  )
                  left_with_contracts
                  right,
            }
            ) (
              std.array.try_fold_left
                (fun acc =>
                  match {
                    'Ok v =>
                      'Ok (acc @ [v]),
                    'Error e =>
                      'Error e,
                  }
                )
                []
                (
                  std.array.zip_with
                    (fun contract elt =>
                      std.contract.check contract label elt
                    )
                    elt_contracts
                    left
                )
            )
        ),
    MaxItems
      : Number -> Dyn
      | doc m%"
              A contract that checks whether an array has at most a certain number of items.

              # Examples

              ```nickel multiline
              [1, 2, 3] | MaxItems 3

              [1, 2, 3] | MaxItems 2
              # => error
            "%
      = fun n =>
        std.contract.from_predicate (fun arr =>
          (std.is_array arr)
          && ((std.array.length (arr | Array Dyn)) <= n)
        ),
    MinItems
      : Number -> Dyn
      | doc m%"
              A contract that checks whether an array has at least a certain number of items.

              # Examples

              ```nickel multiline
              [1, 2, 3] | MinItems 3

              [1, 2, 3] | MinItems 4
              # => error
              ```
            "%
      = fun n =>
        std.contract.from_predicate (fun arr =>
          (std.is_array arr)
          && ((std.array.length (arr | Array Dyn)) >= n)
        ),
    UniqueItems
      | doc m%"
              Checks that the elements of an array are pairwise distinct.

              # Examples

              ```nickel multiline
              [1, 2, "3"] | UniqueItems

              [1, 3, 2, 3] | UniqueItems
              # => error: duplicate found: 3
              ```
            "%
      =
        let find_duplicate
          : Array Dyn -> {
            has_duplicate : Bool,
            duplicate : Dyn,
            seen : { _ : Bool }
          }
          =
            std.array.fold_right
              (fun elt acc =>
                if acc.has_duplicate then
                  acc
                else
                  let index = std.serialize 'Json elt in
                  if std.record.has_field index acc.seen then
                    {
                      duplicate = elt,
                      has_duplicate = true,
                      seen = acc.seen
                    }
                  else
                    {
                      duplicate = null,
                      has_duplicate = false,
                      seen = std.record.insert index true acc.seen
                    }
              )
              { duplicate = null, has_duplicate = false, seen = {} }
        in
        std.contract.from_validator (fun x =>
          if !(std.is_array x) then
            'Error { message = "expected an array" }
          else
            let { has_duplicate, duplicate, .. } = find_duplicate x in
            if has_duplicate then
              'Error {
                message = "duplicate found: %{std.serialize
                                    'Json
                                    duplicate}"
              }
            else
              'Ok
        )
  },
  number =
    let from_predicate
      | (Number -> Bool) -> String -> Dyn
      = fun pred msg =>
        std.contract.from_validator (fun value =>
          if !(std.is_number value) then
            'Error { message = "expected a number" }
          else if pred (value | Number) then
            'Ok
          else
            'Error { message = msg }
        )
    in
    {
      ExclusiveMaximum
        | Number -> Dyn
        | doc m%"
                `ExclusiveMaximum n` is a contract checking that a number is at strictly less than `n`.

                # Examples

                ```nickel multiline
                5 | ExclusiveMaximum 5.1

                5 | ExclusiveMaximum 5
                # => error: expected an exclusive maximum of 5

                "5" | ExclusiveMaximum 5
                # => error: expected a number
                ```
              "%
        = fun limit =>
          from_predicate
            (fun x => x < limit)
            "expected an exclusive maximum of %{std.string.from_number
                      limit}",
      ExclusiveMinimum
        | Number -> Dyn
        | doc m%"
                `ExclusiveMinimum n` is a contract checking that a number is at strictly more than `n`.

                # Examples

                ```nickel multiline
                5 | ExclusiveMinimum 4.9

                5 | ExclusiveMinimum 5
                # => error: expected an exclusive minimum of 5

                "5" | ExclusiveMinimum 5
                # => error: expected a number
                ```
              "%
        = fun limit =>
          from_predicate
            (fun x => x > limit)
            "expected an exclusive minimum of %{std.string.from_number
                      limit}",
      Maximum
        | Number -> Dyn
        | doc m%"
                `Maximum n` is a contract checking that a number is at most `n`.

                # Examples

                ```nickel multiline
                5 | Maximum 5

                5 | Maximum 4
                # => error: expected a maximum of 4

                "5" | Maximum 5
                # => error: expected a number
                ```
              "%
        = fun limit =>
          from_predicate
            (fun x => x <= limit)
            "expected a maximum of %{std.string.from_number limit}",
      Minimum
        | Number -> Dyn
        | doc m%"
                `Minimum n` is a contract checking that a number is at least `n`.

                # Examples

                ```nickel multiline
                5 | Minimum 5

                5 | Minimum 6
                # => error: expected a minimum of 6

                "5" | Minimum 6
                # => error: expected a number
                ```
              "%
        = fun limit =>
          from_predicate
            (fun x => x >= limit)
            "expected a minimum of %{std.string.from_number limit}",
      MultipleOf
        | Number -> Dyn
        | doc m%"
                `MultipleOf n` is a contract checking whether a value is a multiple of `n`.

                # Examples

                ```nickel multiline
                4 | MultipleOf 2

                "4" | MultipleOf 2
                # => error: expected a number

                4 | MultipleOf 3
                # => error: expected a multiple of 3

                4 | MultipleOf 0
                # => error: expected a multiple of 0
                ```
              "%
        = fun n =>
          std.contract.from_validator (fun value =>
            if !(std.is_number value) then
              'Error { message = "expected a number" }
            else if n == 0 then
              'Error { message = "expected a multiple of 0" }
            else if std.number.is_integer (value / n) then
              'Ok
            else
              'Error {
                message = "expected a multiple of %{std.string.from_number
                                      n}"
              }
          )
    },
  record =
    let ErrData = {
      blame_location | Dyn | optional,
      message | String | optional,
      notes | Array String | optional
    }
    in
    {
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
            if !(std.is_record value) then
              'Error { message = "expected a record" }
            else
              std.array.try_fold_left
                (fun acc { field, value = contract, } =>
                  if std.record.has_field field value then
                    std.contract.check contract label acc
                  else
                    'Ok acc
                )
                value
                (std.record.to_array deps)
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
            if !(std.is_record value) then
              'Error { message = "expected a record" }
            else
              let must_have_fields
                : Array String -> { _ : Dyn } -> [| 'Ok, 'Error ErrData |]
                = fun fields x =>
                  std.array.fold_right
                    (fun field acc =>
                      if !(std.record.has_field field x) then
                        'Error (
                          { message = "expected field %{field}" }
                            | ErrData
                        )
                      else
                        acc
                    )
                    'Ok
                    fields
              in
              std.array.fold_right
                (fun { field, value = dependent_fields, } acc =>
                  if std.record.has_field field value then
                    (match {
                      'Ok =>
                        acc,
                      'Error e =>
                        'Error (
                          e
                          & {
                            notes = [
                              "because field %{field} was present"
                            ]
                          }
                        ),
                    }
                    ) (must_have_fields dependent_fields value)
                  else
                    acc
                )
                'Ok
                (std.record.to_array dependencies)
          ),
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
            if !(std.is_record value) then
              'Error { message = "expected a record" }
            else if (std.record.length value) > n then
              'Error {
                message = "expected a record with at most %{std.string.from_number
                                    n} fields"
              }
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
            if !(std.is_record value) then
              'Error { message = "expected a record" }
            else if (std.record.length value) < n then
              'Error {
                message = "expected a record with at least %{std.string.from_number
                                    n} fields"
              }
            else
              'Ok
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
            if !(std.is_record value) then
              'Error { message = "expected a record" }
            else
              std.array.try_fold_left
                (fun acc field_name =>
                  (match {
                    'Ok _ =>
                      'Ok value,
                    'Error e =>
                      'Error e,
                  }
                  ) (std.contract.check contract label field_name)
                )
                ('Ok value)
                (std.record.fields value)
          ),
      Record
        | {
          additional | [| 'None, 'Some Dyn |],
          patterns | { _ | Dyn },
          properties | { _ | Dyn },
          required | { _ | Dyn }
        } -> Dyn
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
        = fun { properties, required, patterns, additional, } =>
          std.contract.custom (fun label x =>
            if !(std.is_record x) then
              'Error { message = "expected a record" }
            else
              let x | { _ | Dyn } = x in
              let required | { _ | Dyn } = required in
              let checked_properties
                : [| 'Ok ({ _ | Dyn }), 'Error ErrData |]
                =
                  std.array.try_fold_left
                    (fun acc { field, value = contract, } =>
                      if !(std.record.has_field field x) then
                        if std.record.has_field field required then
                          'Error (
                            {
                              message = "missing required field `%{field}`"
                            }
                              | ErrData
                          )
                        else
                          'Ok acc
                      else
                        (match {
                          'Ok checked_value =>
                            'Ok (
                              std.record.insert
                                field
                                checked_value
                                acc
                            ),
                          'Error e =>
                            'Error e,
                        }
                        ) (
                          std.contract.check
                            contract
                            label
                            x."%{field}"
                        )
                    )
                    (
                      {}
                        | { _ | Dyn }
                    )
                    (std.record.to_array (properties | { _ : Dyn }))
              in
              let matching_fields
                : String -> { _ : Dyn }
                = fun pattern =>
                  let matcher = std.string.is_match pattern in
                  std.array.fold_left
                    (fun acc { field, value, } =>
                      if matcher field then
                        std.record.insert field value acc
                      else
                        acc
                    )
                    {}
                    (std.record.to_array x)
              in
              let checked_pattern_properties
                : [| 'Ok ({ _ | Dyn }), 'Error ErrData |]
                =
                  std.array.try_fold_left
                    (fun acc { field = pattern, value = contract, } =>
                      (match {
                        'Ok checked_dict =>
                          'Ok (
                            (acc & checked_dict)
                              | { _ | Dyn }
                          ),
                        'Error e =>
                          'Error e,
                      }
                      ) (
                        std.array.try_fold_left
                          (fun acc { field, value, } =>
                            (match {
                              'Ok checked_value =>
                                'Ok (
                                  std.record.insert
                                    field
                                    checked_value
                                    acc
                                ),
                              'Error e =>
                                'Error e,
                            }
                            ) (
                              std.contract.check
                                contract
                                label
                                value
                            )
                          )
                          (
                            {}
                              | { _ | Dyn }
                          )
                          (
                            std.record.to_array (matching_fields pattern)
                          )
                      )
                    )
                    (
                      {}
                        | { _ | Dyn }
                    )
                    (
                      std.record.to_array (
                        patterns
                          | { _ : Dyn }
                      )
                    )
              in
              (match {
                ['Error e, _] =>
                  'Error e,
                [_, 'Error e] =>
                  'Error e,
                [
                  'Ok checked_properties,
                  'Ok checked_pattern_properties
                ] =>
                  let remaining_fields
                    : { _ | Dyn }
                    =
                      std.array.fold_left
                        (fun acc field =>
                          if !(
                            std.record.has_field
                              field
                              acc
                          ) then
                            acc
                          else
                            std.record.remove field acc
                        )
                        x
                        (
                          (
                            std.record.fields (
                              checked_properties
                                | { _ : Dyn }
                            )
                          )
                          @ (
                            std.record.fields (
                              checked_pattern_properties
                                | { _ : Dyn }
                            )
                          )
                        )
                  in
                  let checked_additional
                    : [|
                      'Ok (
                        { _ | Dyn }
                      ),
                      'Error ErrData
                    |]
                    =
                      (match {
                        'None =>
                          if !(
                            std.record.is_empty remaining_fields
                          ) then
                            'Error (
                              {
                                message = "extra fields %{std.serialize
                                                          'Json
                                                          ((std.record.fields
                                                            remaining_fields)
                                                          | Dyn)}"
                              }
                                | ErrData
                            )
                          else
                            'Ok ({} | { _ | Dyn }),
                        'Some contract =>
                          std.array.try_fold_left
                            (fun acc { field, value, } =>
                              (match {
                                'Ok checked_value =>
                                  'Ok (
                                    std.record.insert
                                      field
                                      checked_value
                                      acc
                                  ),
                                'Error e =>
                                  'Error e,
                              }
                              ) (
                                std.contract.check
                                  contract
                                  label
                                  value
                              )
                            )
                            (
                              {}
                                | { _ | Dyn }
                            )
                            (
                              std.record.to_array remaining_fields
                            ),
                      }
                      ) (
                        additional
                          | [| 'None, 'Some Dyn |]
                      )
                  in
                  (match {
                    'Error e =>
                      'Error e,
                    'Ok checked_additional =>
                      'Ok (
                        (
                          checked_properties
                          & checked_pattern_properties
                        )
                        & checked_additional
                      ),
                  }
                  ) checked_additional,
              }
              ) [
                checked_properties,
                checked_pattern_properties
              ]
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
            if !(std.is_record value) then
              'Error { message = "expected a record" }
            else
              (match { 'Error e => 'Error e, 'Ok _ => 'Ok, }) (
                std.array.try_fold_left
                  (fun acc field =>
                    if !(std.record.has_field field value) then
                      'Error {
                        message = "missing required field %{field}"
                      }
                    else
                      'Ok acc
                  )
                  null
                  fields
              )
          )
    },
  string = {
    MaxLength
      | Number -> Dyn
      | doc m%"
              `MaxLength n` is a contract that checks for a string of length at most `n`.

              Here, the "length" of a string is the number of extended grapheme clusters in the string.
              This may not agree with the JSON schema semantics, which counts the number of Unicode
              code points.

              # Examples

              ```nickel multiline
              "Hello" | MaxLength 5

              # 15 bytes, 3 code points, 1 extended grapheme cluster
              "👨‍🦰" | MaxLength 1

              "Hello" | MaxLength 4
              # => error
              ```
            "%
      = fun limit =>
        std.contract.from_validator (fun value =>
          if !(std.is_string value) then
            'Error { message = "expected a string" }
          else if (std.string.length value) <= limit then
            'Ok
          else
            'Error {
              message = "expected a string of length at most %{std.string.from_number
                                  limit}"
            }
        ),
    MinLength
      | Number -> Dyn
      | doc m%"
              `MinLength n` is a contract that checks for a string of length at least `n`.

              Here, the "length" of a string is the number of extended grapheme clusters in the string.
              This may not agree with the JSON schema semantics, which counts the number of Unicode
              code points.

              # Examples

              ```nickel multiline
              "Hello" | MinLength 5

              # 15 bytes, 3 code points, 1 extended grapheme cluster
              "👨‍🦰" | MinLength 3
              # => error

              "Hello" | MinLength 6
              # => error
              ```
            "%
      = fun limit =>
        std.contract.from_validator (fun value =>
          if !(std.is_string value) then
            'Error { message = "expected a string" }
          else if (std.string.length value) >= limit then
            'Ok
          else
            'Error {
              message = "expected a string of length at least %{std.string.from_number
                                  limit}"
            }
        )
  }
}
in
let rec refs = {
  definitions.branch =
    std.contract.Sequence [refs.definitions.globs, Array Dyn ],
  definitions.concurrency = {
    cancel-in-progress
      | std.contract.any_of [
        Bool,
        std.contract.Sequence [refs.definitions.expressionSyntax, String ]
      ]
      | optional,
    group | String
  },
  definitions.configuration =
    std.contract.any_of [ String, Number, Bool, { _ | js2n.Always }, Array js2n.Always],
  definitions.container = {
    credentials
      | {
        password | String | optional,
        username | String | optional,
        ..
      }
      | optional,
    env | refs.definitions.env | optional,
    image | String,
    options | String | optional,
    ports
      | js2n.array.MinItems 1
      | Array (std.contract.any_of [ Number, String ])
      | optional,
    volumes
      | js2n.array.MinItems 1
      | Array (std.string.Matches "^[^:]+:[^:]+$")
      | optional
  },
  definitions.defaults =
    std.contract.Sequence [
      js2n.record.MinProperties 1,
      {
        run
          | js2n.record.MinProperties 1
          | {
            shell | refs.definitions.shell | String | optional,
            working-directory
              | refs.definitions.working-directory
              | String
              | optional
          }
          | optional
      }
    ],
  definitions.env =
    std.contract.any_of [
      { _ | std.contract.any_of [ Bool, Number, String ] },
      std.contract.Sequence [refs.definitions.stringContainingExpressionSyntax, String ]
    ],
  definitions.environment = { name | String, url | String | optional },
  definitions.event =
    std.contract.Sequence [
      std.enum.TagOrString,
      [|
        'repository_dispatch,
        'workflow_run,
        'workflow_dispatch,
        'workflow_call,
        'watch,
        'status,
        'release,
        'registry_package,
        'push,
        'pull_request_target,
        'pull_request_review_comment,
        'pull_request_review,
        'pull_request,
        'public,
        'project_column,
        'project_card,
        'project,
        'page_build,
        'milestone,
        'merge_group,
        'label,
        'issues,
        'issue_comment,
        'gollum,
        'fork,
        'discussion_comment,
        'discussion,
        'deployment_status,
        'deployment,
        'delete,
        'create,
        'check_suite,
        'check_run,
        'branch_protection_rule
      |]
    ],
  definitions.eventObject = js2n.Nullable { .. },
  definitions.expressionSyntax =
    std.string.Matches "^\\$\\{\\{(.|[\r\n])*\\}\\}$",
  definitions.globs =
    std.contract.Sequence [js2n.array.MinItems 1, Array (js2n.string.MinLength 1)],
  definitions.jobNeeds =
    std.contract.any_of [
      std.contract.Sequence [
        js2n.array.MinItems 1,
        Array (
          std.contract.Sequence [refs.definitions.name, String ]
        )
      ],
      std.contract.Sequence [refs.definitions.name, String ]
    ],
  definitions.matrix =
    std.contract.any_of [
      std.contract.Sequence [
        js2n.record.MinProperties 1,
        js2n.record.Record {
          properties = {
            exclude =
              std.contract.any_of [
                std.contract.Sequence [
                  refs.definitions.expressionSyntax,
                  String
                ],
                std.contract.Sequence [
                  js2n.array.MinItems 1,
                  Array (
                    { _ | refs.definitions.configuration }
                  )
                ]
              ],
            include =
              std.contract.any_of [
                std.contract.Sequence [
                  refs.definitions.expressionSyntax,
                  String
                ],
                std.contract.Sequence [
                  js2n.array.MinItems 1,
                  Array (
                    { _ | refs.definitions.configuration }
                  )
                ]
              ]
          },
          required = {},
          patterns = {},
          additional =
            'Some (
              std.contract.any_of [
                std.contract.Sequence [
                  js2n.array.MinItems 1,
                  Array refs.definitions.configuration
                ],
                std.contract.Sequence [refs.definitions.expressionSyntax, String ]
              ]
            )
        }
      ],
      std.contract.Sequence [refs.definitions.expressionSyntax, String ]
    ],
  definitions.name = std.string.Matches "^[_a-zA-Z][a-zA-Z0-9_-]*$",
  definitions.normalJob = {
    concurrency
      | std.contract.any_of [
        String,
        std.contract.Sequence [refs.definitions.concurrency, { _ | Dyn }]
      ]
      | optional,
    container
      | std.contract.any_of [
        String,
        std.contract.Sequence [refs.definitions.container, { _ | Dyn }]
      ]
      | optional,
    continue-on-error
      | std.contract.any_of [
        Bool,
        std.contract.Sequence [refs.definitions.expressionSyntax, String ]
      ]
      | optional,
    defaults | refs.definitions.defaults | { _ | Dyn } | optional,
    env | refs.definitions.env | optional,
    environment
      | std.contract.any_of [
        String,
        std.contract.Sequence [refs.definitions.environment, { _ | Dyn }]
      ]
      | optional,
    "if" | std.contract.any_of [ Bool, Number, String ] | optional,
    name | String | optional,
    needs | refs.definitions.jobNeeds | optional,
    outputs | js2n.record.MinProperties 1 | { _ | String } | optional,
    permissions | refs.definitions.permissions | optional,
    runs-on
      | std.contract.any_of [
        String,
        std.contract.Sequence [
          js2n.array.MinItems 1,
          js2n.array.Items [ String ] js2n.Always
        ],
        js2n.record.Record {
          properties = {
            group = String,
            labels =
              std.contract.any_of [ String, js2n.array.ArrayOf String ]
          },
          required = {},
          patterns = {},
          additional = 'Some Dyn
        },
        std.contract.Sequence [
          refs.definitions.stringContainingExpressionSyntax,
          String
        ],
        std.contract.Sequence [refs.definitions.expressionSyntax, String ]
      ],
    services
      | {
        _ | std.contract.Sequence [refs.definitions.container, { _ | Dyn }]
      }
      | optional,
    steps
      | js2n.array.MinItems 1
      | Array (
        std.contract.Sequence [refs.definitions.step, { _ | Dyn }]
      )
      | optional,
    strategy
      | {
        fail-fast | std.contract.any_of [ Bool, String ] | optional,
        matrix | refs.definitions.matrix,
        max-parallel | std.contract.any_of [ Number, String ] | optional
      }
      | optional,
    timeout-minutes
      | std.contract.any_of [
        Number,
        std.contract.Sequence [refs.definitions.expressionSyntax, String ]
      ]
      | optional
  },
  definitions.path =
    std.contract.Sequence [refs.definitions.globs, Array Dyn ],
  definitions.permissions =
    std.contract.any_of [
      std.contract.Sequence [std.enum.TagOrString, [| 'write-all, 'read-all |]],
      std.contract.Sequence [refs.definitions.permissions-event, { _ | Dyn }]
    ],
  definitions.permissions-event = {
    actions | refs.definitions.permissions-level | String | optional,
    attestations
      | refs.definitions.permissions-level
      | String
      | optional,
    checks | refs.definitions.permissions-level | String | optional,
    contents | refs.definitions.permissions-level | String | optional,
    deployments
      | refs.definitions.permissions-level
      | String
      | optional,
    discussions
      | refs.definitions.permissions-level
      | String
      | optional,
    id-token | refs.definitions.permissions-level | String | optional,
    issues | refs.definitions.permissions-level | String | optional,
    packages | refs.definitions.permissions-level | String | optional,
    pages | refs.definitions.permissions-level | String | optional,
    pull-requests
      | refs.definitions.permissions-level
      | String
      | optional,
    repository-projects
      | refs.definitions.permissions-level
      | String
      | optional,
    security-events
      | refs.definitions.permissions-level
      | String
      | optional,
    statuses | refs.definitions.permissions-level | String | optional
  },
  definitions.permissions-level =
    std.contract.Sequence [std.enum.TagOrString, [| 'none, 'write, 'read |]],
  definitions.ref =
    std.contract.Sequence [
      js2n.Nullable {
        branches | refs.definitions.branch | Array Dyn | optional,
        branches-ignore
          | refs.definitions.branch
          | Array Dyn
          | optional,
        paths | refs.definitions.path | Array Dyn | optional,
        paths-ignore | refs.definitions.path | Array Dyn | optional,
        tags | refs.definitions.branch | Array Dyn | optional,
        tags-ignore | refs.definitions.branch | Array Dyn | optional,
        ..
      },
      js2n.Nullable (
        std.contract.Sequence [
          std.contract.not (
            std.contract.any_of [
              js2n.Null,
              Bool,
              Number,
              String,
              js2n.record.Required ["branches", "branches-ignore"],
              Array Dyn
            ]
          ),
          std.contract.not (
            std.contract.any_of [
              js2n.Null,
              Bool,
              Number,
              String,
              js2n.record.Required ["tags", "tags-ignore"],
              Array Dyn
            ]
          ),
          std.contract.not (
            std.contract.any_of [
              js2n.Null,
              Bool,
              Number,
              String,
              js2n.record.Required ["paths", "paths-ignore"],
              Array Dyn
            ]
          ),
          { _ | Dyn }
        ]
      )
    ],
  definitions.reusableWorkflowCallJob = {
    concurrency
      | std.contract.any_of [
        String,
        std.contract.Sequence [refs.definitions.concurrency, { _ | Dyn }]
      ]
      | optional,
    "if" | std.contract.any_of [ Bool, Number, String ] | optional,
    name | String | optional,
    needs | refs.definitions.jobNeeds | optional,
    permissions | refs.definitions.permissions | optional,
    secrets
      | js2n.OneOf [
        refs.definitions.env,
        std.contract.Sequence [std.enum.TagOrString, [| 'inherit |]]
      ]
      | optional,
    strategy
      | {
        fail-fast | std.contract.any_of [ Bool, String ] | optional,
        matrix | refs.definitions.matrix,
        max-parallel | std.contract.any_of [ Number, String ] | optional
      }
      | optional,
    uses | std.string.Matches "^(.+\\/)+(.+)\\.(ya?ml)(@.+)?$",
    with | refs.definitions.env | optional
  },
  definitions.shell =
    std.contract.any_of [
      String,
      std.contract.Sequence [
        std.enum.TagOrString,
        [| 'powershell, 'cmd, 'sh, 'python, 'pwsh, 'bash |]
      ]
    ],
  definitions.step =
    std.contract.Sequence [
      js2n.record.DependentFields { shell = ["run"], working-directory = ["run"] },
      js2n.OneOf [
        std.contract.any_of [
          js2n.Null,
          Bool,
          Number,
          String,
          js2n.record.Required ["uses"],
          Array Dyn
        ],
        std.contract.any_of [
          js2n.Null,
          Bool,
          Number,
          String,
          js2n.record.Required ["run"],
          Array Dyn
        ]
      ],
      {
        continue-on-error
          | std.contract.any_of [
            Bool,
            std.contract.Sequence [refs.definitions.expressionSyntax, String ]
          ]
          | optional,
        env | refs.definitions.env | optional,
        id | String | optional,
        "if" | std.contract.any_of [ Bool, Number, String ] | optional,
        name | String | optional,
        run | String | optional,
        shell | refs.definitions.shell | String | optional,
        timeout-minutes
          | std.contract.any_of [
            Number,
            std.contract.Sequence [refs.definitions.expressionSyntax, String ]
          ]
          | optional,
        uses | String | optional,
        with
          | std.contract.any_of [
            js2n.Null,
            Bool,
            Number,
            String,
            {
              args | String | optional,
              entrypoint | String | optional,
              ..
            },
            Array Dyn
          ]
          | refs.definitions.env
          | optional,
        working-directory
          | refs.definitions.working-directory
          | String
          | optional
      }
    ],
  definitions.stringContainingExpressionSyntax =
    std.string.Matches "^.*\\$\\{\\{(.|[\r\n])*\\}\\}.*$",
  definitions.types = js2n.array.MinItems 1,
  definitions.workflowDispatchInput =
    std.contract.Sequence [
      js2n.IfThenElse
        (
          std.contract.any_of [
            js2n.Null,
            Bool,
            Number,
            String,
            js2n.record.Record {
              properties = { type = js2n.Const "string" },
              required = { type = true },
              patterns = {},
              additional = 'Some Dyn
            },
            Array Dyn
          ]
        )
        (
          std.contract.any_of [
            js2n.Null,
            Bool,
            Number,
            String,
            { "default" | String | optional, .. },
            Array Dyn
          ]
        )
        js2n.Always,
      js2n.IfThenElse
        (
          std.contract.any_of [
            js2n.Null,
            Bool,
            Number,
            String,
            js2n.record.Record {
              properties = { type = js2n.Const "boolean" },
              required = { type = true },
              patterns = {},
              additional = 'Some Dyn
            },
            Array Dyn
          ]
        )
        (
          std.contract.any_of [
            js2n.Null,
            Bool,
            Number,
            String,
            { "default" | Bool | optional, .. },
            Array Dyn
          ]
        )
        js2n.Always,
      js2n.IfThenElse
        (
          std.contract.any_of [
            js2n.Null,
            Bool,
            Number,
            String,
            js2n.record.Record {
              properties = { type = js2n.Const "number" },
              required = { type = true },
              patterns = {},
              additional = 'Some Dyn
            },
            Array Dyn
          ]
        )
        (
          std.contract.any_of [
            js2n.Null,
            Bool,
            Number,
            String,
            { "default" | Number | optional, .. },
            Array Dyn
          ]
        )
        js2n.Always,
      js2n.IfThenElse
        (
          std.contract.any_of [
            js2n.Null,
            Bool,
            Number,
            String,
            js2n.record.Record {
              properties = { type = js2n.Const "environment" },
              required = { type = true },
              patterns = {},
              additional = 'Some Dyn
            },
            Array Dyn
          ]
        )
        (
          std.contract.any_of [
            js2n.Null,
            Bool,
            Number,
            String,
            { "default" | String | optional, .. },
            Array Dyn
          ]
        )
        js2n.Always,
      js2n.IfThenElse
        (
          std.contract.any_of [
            js2n.Null,
            Bool,
            Number,
            String,
            js2n.record.Record {
              properties = { type = js2n.Const "choice" },
              required = { type = true },
              patterns = {},
              additional = 'Some Dyn
            },
            Array Dyn
          ]
        )
        (
          std.contract.any_of [
            js2n.Null,
            Bool,
            Number,
            String,
            js2n.record.Required ["options"],
            Array Dyn
          ]
        )
        js2n.Always,
      {
        "default"
          | std.contract.any_of [js2n.Null, Bool, Number, String, { _ | Dyn }, Array Dyn ]
          | optional,
        deprecationMessage | String | optional,
        description | String,
        options | js2n.array.MinItems 1 | Array String | optional,
        required | Bool | optional,
        type
          | std.enum.TagOrString
          | [| 'environment, 'number, 'boolean, 'choice, 'string |]
          | optional
      }
    ],
  definitions.working-directory = String
}
in
{
  concurrency
    | std.contract.any_of [ String, refs.definitions.concurrency]
    | optional,
  defaults | refs.definitions.defaults | { _ | Dyn } | optional,
  env | refs.definitions.env | optional,
  jobs
    | js2n.record.MinProperties 1
    | {
      _ | js2n.OneOf [
        refs.definitions.normalJob,
        refs.definitions.reusableWorkflowCallJob
      ]
    }
    | std.record.FieldsMatch "^[_a-zA-Z][a-zA-Z0-9_-]*$",
  name | String | optional,
  on
    | std.contract.any_of [
      refs.definitions.event,
      std.contract.Sequence [js2n.array.MinItems 1, Array refs.definitions.event],
      {
        branch_protection_rule
          | js2n.Nullable {
            types
              | Array (
                std.contract.Sequence [
                  std.enum.TagOrString,
                  [| 'deleted, 'edited, 'created |]
                ]
              )
              | refs.definitions.types
              | optional,
            ..
          }
          | refs.definitions.eventObject
          | optional,
        check_run
          | js2n.Nullable {
            types
              | Array (
                std.contract.Sequence [
                  std.enum.TagOrString,
                  [|
                    'requested_action,
                    'completed,
                    'rerequested,
                    'created
                  |]
                ]
              )
              | refs.definitions.types
              | optional,
            ..
          }
          | refs.definitions.eventObject
          | optional,
        check_suite
          | js2n.Nullable {
            types
              | Array (
                std.contract.Sequence [
                  std.enum.TagOrString,
                  [| 'rerequested, 'requested, 'completed |]
                ]
              )
              | refs.definitions.types
              | optional,
            ..
          }
          | refs.definitions.eventObject
          | optional,
        create | refs.definitions.eventObject | optional,
        delete | refs.definitions.eventObject | optional,
        deployment | refs.definitions.eventObject | optional,
        deployment_status | refs.definitions.eventObject | optional,
        discussion
          | js2n.Nullable {
            types
              | Array (
                std.contract.Sequence [
                  std.enum.TagOrString,
                  [|
                    'unanswered,
                    'answered,
                    'category_changed,
                    'unlocked,
                    'locked,
                    'unlabeled,
                    'labeled,
                    'unpinned,
                    'pinned,
                    'transferred,
                    'deleted,
                    'edited,
                    'created
                  |]
                ]
              )
              | refs.definitions.types
              | optional,
            ..
          }
          | refs.definitions.eventObject
          | optional,
        discussion_comment
          | js2n.Nullable {
            types
              | Array (
                std.contract.Sequence [
                  std.enum.TagOrString,
                  [| 'deleted, 'edited, 'created |]
                ]
              )
              | refs.definitions.types
              | optional,
            ..
          }
          | refs.definitions.eventObject
          | optional,
        fork | refs.definitions.eventObject | optional,
        gollum | refs.definitions.eventObject | optional,
        issue_comment
          | js2n.Nullable {
            types
              | Array (
                std.contract.Sequence [
                  std.enum.TagOrString,
                  [| 'deleted, 'edited, 'created |]
                ]
              )
              | refs.definitions.types
              | optional,
            ..
          }
          | refs.definitions.eventObject
          | optional,
        issues
          | js2n.Nullable {
            types
              | Array (
                std.contract.Sequence [
                  std.enum.TagOrString,
                  [|
                    'demilestoned,
                    'milestoned,
                    'unlocked,
                    'locked,
                    'unlabeled,
                    'labeled,
                    'unassigned,
                    'assigned,
                    'reopened,
                    'closed,
                    'unpinned,
                    'pinned,
                    'transferred,
                    'deleted,
                    'edited,
                    'opened
                  |]
                ]
              )
              | refs.definitions.types
              | optional,
            ..
          }
          | refs.definitions.eventObject
          | optional,
        label
          | js2n.Nullable {
            types
              | Array (
                std.contract.Sequence [
                  std.enum.TagOrString,
                  [| 'deleted, 'edited, 'created |]
                ]
              )
              | refs.definitions.types
              | optional,
            ..
          }
          | refs.definitions.eventObject
          | optional,
        merge_group
          | js2n.Nullable {
            types
              | Array (
                std.contract.Sequence [std.enum.TagOrString, [| 'checks_requested |]]
              )
              | refs.definitions.types
              | optional,
            ..
          }
          | refs.definitions.eventObject
          | optional,
        milestone
          | js2n.Nullable {
            types
              | Array (
                std.contract.Sequence [
                  std.enum.TagOrString,
                  [| 'deleted, 'edited, 'opened, 'closed, 'created |]
                ]
              )
              | refs.definitions.types
              | optional,
            ..
          }
          | refs.definitions.eventObject
          | optional,
        page_build | refs.definitions.eventObject | optional,
        project
          | js2n.Nullable {
            types
              | Array (
                std.contract.Sequence [
                  std.enum.TagOrString,
                  [|
                    'deleted,
                    'edited,
                    'reopened,
                    'closed,
                    'updated,
                    'created
                  |]
                ]
              )
              | refs.definitions.types
              | optional,
            ..
          }
          | refs.definitions.eventObject
          | optional,
        project_card
          | js2n.Nullable {
            types
              | Array (
                std.contract.Sequence [
                  std.enum.TagOrString,
                  [|
                    'deleted,
                    'edited,
                    'converted,
                    'moved,
                    'created
                  |]
                ]
              )
              | refs.definitions.types
              | optional,
            ..
          }
          | refs.definitions.eventObject
          | optional,
        project_column
          | js2n.Nullable {
            types
              | Array (
                std.contract.Sequence [
                  std.enum.TagOrString,
                  [| 'deleted, 'moved, 'updated, 'created |]
                ]
              )
              | refs.definitions.types
              | optional,
            ..
          }
          | refs.definitions.eventObject
          | optional,
        public | refs.definitions.eventObject | optional,
        pull_request
          | js2n.Nullable {
            branches | Array Dyn | optional,
            branches-ignore | Array Dyn | optional,
            paths | Array Dyn | optional,
            paths-ignore | Array Dyn | optional,
            tags | Array Dyn | optional,
            tags-ignore | Array Dyn | optional,
            types
              | Array (
                std.contract.Sequence [
                  std.enum.TagOrString,
                  [|
                    'dequeued,
                    'enqueued,
                    'auto_merge_disabled,
                    'auto_merge_enabled,
                    'review_request_removed,
                    'review_requested,
                    'demilestoned,
                    'milestoned,
                    'unlocked,
                    'locked,
                    'ready_for_review,
                    'converted_to_draft,
                    'synchronize,
                    'reopened,
                    'closed,
                    'edited,
                    'opened,
                    'unlabeled,
                    'labeled,
                    'unassigned,
                    'assigned
                  |]
                ]
              )
              | refs.definitions.types
              | optional
          }
          | refs.definitions.ref
          | optional,
        pull_request_review
          | js2n.Nullable {
            types
              | Array (
                std.contract.Sequence [
                  std.enum.TagOrString,
                  [| 'dismissed, 'edited, 'submitted |]
                ]
              )
              | refs.definitions.types
              | optional,
            ..
          }
          | refs.definitions.eventObject
          | optional,
        pull_request_review_comment
          | js2n.Nullable {
            types
              | Array (
                std.contract.Sequence [
                  std.enum.TagOrString,
                  [| 'deleted, 'edited, 'created |]
                ]
              )
              | refs.definitions.types
              | optional,
            ..
          }
          | refs.definitions.eventObject
          | optional,
        pull_request_target
          | js2n.Nullable {
            branches | js2n.Always | optional,
            branches-ignore | js2n.Always | optional,
            paths | js2n.Always | optional,
            paths-ignore | js2n.Always | optional,
            tags | js2n.Always | optional,
            tags-ignore | js2n.Always | optional,
            types
              | Array (
                std.contract.Sequence [
                  std.enum.TagOrString,
                  [|
                    'auto_merge_disabled,
                    'auto_merge_enabled,
                    'review_request_removed,
                    'review_requested,
                    'unlocked,
                    'locked,
                    'ready_for_review,
                    'converted_to_draft,
                    'synchronize,
                    'reopened,
                    'closed,
                    'edited,
                    'opened,
                    'unlabeled,
                    'labeled,
                    'unassigned,
                    'assigned
                  |]
                ]
              )
              | refs.definitions.types
              | optional
          }
          | refs.definitions.ref
          | optional,
        push
          | js2n.Nullable {
            branches | Array String | optional,
            branches-ignore | Array String | optional,
            paths | Array String | optional,
            paths-ignore | Array String | optional,
            tags | Array String | optional,
            tags-ignore | Array String | optional
          }
          | refs.definitions.ref
          | optional,
        registry_package
          | js2n.Nullable {
            types
              | Array (
                std.contract.Sequence [std.enum.TagOrString, [| 'updated, 'published |]]
              )
              | refs.definitions.types
              | optional,
            ..
          }
          | refs.definitions.eventObject
          | optional,
        release
          | js2n.Nullable {
            types
              | Array (
                std.contract.Sequence [
                  std.enum.TagOrString,
                  [|
                    'released,
                    'prereleased,
                    'deleted,
                    'edited,
                    'created,
                    'unpublished,
                    'published
                  |]
                ]
              )
              | refs.definitions.types
              | optional,
            ..
          }
          | refs.definitions.eventObject
          | optional,
        repository_dispatch | refs.definitions.eventObject | optional,
        schedule
          | js2n.array.MinItems 1
          | Array { cron | String | optional }
          | optional,
        status | refs.definitions.eventObject | optional,
        watch | refs.definitions.eventObject | optional,
        workflow_call
          | std.contract.any_of [
            js2n.Null,
            Bool,
            Number,
            String,
            {
              inputs
                | {
                  _ | {
                    "default"
                      | std.contract.any_of [ Bool, Number, String ]
                      | optional,
                    deprecationMessage | String | optional,
                    description | String | optional,
                    required | Bool | optional,
                    type
                      | std.enum.TagOrString
                      | [| 'string, 'number, 'boolean |]
                  }
                }
                | std.record.FieldsMatch "^[_a-zA-Z][a-zA-Z0-9_-]*$"
                | optional,
              secrets
                | std.contract.any_of [
                  js2n.Null,
                  Bool,
                  Number,
                  String,
                  std.contract.Sequence [
                    {
                      _ | std.contract.any_of [
                        js2n.Null,
                        Bool,
                        Number,
                        String,
                        {
                          description | String | optional,
                          required | Bool
                        },
                        Array Dyn
                      ]
                    },
                    std.record.FieldsMatch "^[_a-zA-Z][a-zA-Z0-9_-]*$"
                  ],
                  Array Dyn
                ]
                | optional,
              ..
            },
            Array Dyn
          ]
          | optional,
        workflow_dispatch
          | std.contract.any_of [
            js2n.Null,
            Bool,
            Number,
            String,
            {
              inputs
                | {
                  _ | std.contract.Sequence [
                    refs.definitions.workflowDispatchInput,
                    { _ | Dyn }
                  ]
                }
                | std.record.FieldsMatch "^[_a-zA-Z][a-zA-Z0-9_-]*$"
                | optional
            },
            Array Dyn
          ]
          | optional,
        workflow_run
          | js2n.Nullable {
            branches | js2n.Always | optional,
            branches-ignore | js2n.Always | optional,
            types
              | Array (
                std.contract.Sequence [
                  std.enum.TagOrString,
                  [| 'in_progress, 'completed, 'requested |]
                ]
              )
              | refs.definitions.types
              | optional,
            workflows | js2n.array.MinItems 1 | Array String | optional,
            ..
          }
          | refs.definitions.eventObject
          | optional
      }
    ],
  permissions | refs.definitions.permissions | optional,
  run-name | String | optional
}
