# DO NOT EDIT
# This file was automatically generated using json-schema-to-nickel
let js2n
  = {
    Always
      | doc m%"
        A contract that always succeeds. The same as `Dyn`, but with a more JSON-schema name.
        
        # Examples
        
        ```nickel multiline
        "foo" | Always
        
        3 | Always
        ```
      "%
      = std.contract.from_predicate (fun x => true),
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
      = fun
          expected
          =>
          std.contract.from_validator
            (fun
              value
              =>
              if value == expected then
                'Ok
              else
                'Error { message = "expected %{std.to_string expected}", }),
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
      = std.contract.from_predicate (fun x => false),
    Null
      | doc m%"
        A contract asserting that a value is null.
        
        # Examples
        
        ```nickel multiline
        null | Null
        
        "foo" | Null
        # => error
      "%
      = std.contract.from_predicate (fun x => x == null),
    Nullable
      : Dyn -> Dyn
      | doc "For a contract `C`, `or_null C` returns a contract that allows either `C` or a null value."
      = fun
          Contract
          =>
          std.contract.custom
            (fun
              label
              value
              =>
              if value == null then
                'Ok value
              else
                std.contract.check Contract label value),
    array =
      {
          ArrayOf
            | Dyn -> Dyn
            | doc m%"
              An eager version of `Array`. `value | ArrayOf C` will succeed if `value` is an array and all of
              its elements satisfy the contract `C`.
              
              Only the eager part of `C` is checked; the lazy part is discarded.
              TODO: actually, we should propagate it. Otherwise TagOrString won't work as expected
              
              # Examples
              
              ```nickel multiline
              [1, 2, 3] | ArrayOf Number
              # => [1, 2, 3]
              
              [1, 2, 3] | ArrayOf String
              # => error
              ```
            "%
            = fun
                ctr
                =>
                std.contract.custom
                  (fun
                    label
                    value
                    =>
                    if !(std.is_array value) then
                      'Error { message = "expected an array", }
                    else
                      std.array.fold_right
                        (fun
                          elt
                          acc
                          =>
                          (match { 'Ok _ => acc, 'Error e => 'Error e, })
                            (std.contract.check ctr label elt)) ('Ok value)
                        value),
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
              ```
            "%
            = fun
                contract
                =>
                std.contract.custom
                  (fun
                    label
                    value
                    =>
                    if !(std.is_array value) then
                      'Error { message = "expected an array", }
                    else
                      let good_elt
                        = fun
                          elt
                          =>
                          (match { 'Ok _ => true, 'Error _ => false, })
                            (std.contract.check contract label elt)
                        in
                      if std.array.any good_elt value then
                          'Ok value
                        else
                          'Error { message = "no elements matched", }),
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
            = fun
                elt_contracts
                rest
                =>
                std.contract.custom
                  (fun
                    label
                    value
                    =>
                    if !(std.is_array value) then
                      'Error { message = "expected an array", }
                    else
                      let length_to_check
                        = std.number.min (std.array.length elt_contracts)
                          (std.array.length value)
                        in
                      let { left, right, }
                          = std.array.split_at length_to_check value
                          in
                        (match {
                            'Error
                            e =>
                              'Error e,
                            'Ok
                            left_with_contracts =>
                              std.array.try_fold_left
                                (fun
                                  acc
                                  elt
                                  =>
                                  (match {
                                    'Ok
                                    v =>
                                      'Ok (acc @ [ v ]),
                                    'Error
                                    e =>
                                      'Error e,
                                  }) (std.contract.check rest label elt))
                                left_with_contracts right,
                          })
                            (std.array.try_fold_left
                              (fun
                                acc
                                =>
                                match {
                                  'Ok
                                  v =>
                                    'Ok (acc @ [ v ]),
                                  'Error
                                  e =>
                                    'Error e,
                                }) [  ]
                              (std.array.zip_with
                                (fun
                                  contract
                                  elt
                                  =>
                                  std.contract.check contract label elt)
                                elt_contracts left))),
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
            = fun
                n
                =>
                std.contract.from_predicate
                  (fun
                    arr
                    =>
                    ((std.typeof arr) == '"Array")
                    && ((std.array.length (arr | Array Dyn)) <= n)),
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
            = fun
                n
                =>
                std.contract.from_predicate
                  (fun
                    arr
                    =>
                    ((std.typeof arr) == '"Array")
                    && ((std.array.length (arr | Array Dyn)) >= n)),
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
            = let find_duplicate
                : Array Dyn
                -> {
                  has_duplicate : Bool,
                  duplicate : Dyn,
                  seen : { _ : Bool }
                }
                = std.array.fold_right
                  (fun
                    elt
                    acc
                    =>
                    if acc.has_duplicate then
                      acc
                    else
                      let index
                        = std.serialize 'Json elt
                        in
                      if std.record.has_field index acc.seen then
                          {
                            duplicate = elt,
                            has_duplicate = true,
                            seen = acc.seen,
                          }
                        else
                          {
                            duplicate = null,
                            has_duplicate = false,
                            seen = std.record.insert index true acc.seen,
                          })
                  { duplicate = null, has_duplicate = false, seen = {}, }
                in
              std.contract.from_validator
                  (fun
                    x
                    =>
                    if !(std.is_array x) then
                      'Error { message = "expected an array", }
                    else
                      let { has_duplicate, duplicate, .. }
                        = find_duplicate x
                        in
                      if has_duplicate then
                          'Error
                            {
                              message =
                                "duplicate found: %{std.serialize 'Json
                                    duplicate}",
                            }
                        else
                          'Ok),
        },
    enum
      | Array Dyn -> Dyn
      | doc m%"
        `enum values x` succeeds if and only if `x` is equal to one of the elements of `values`.
        Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.1.2]
      "%
      = fun
          variants
          =>
          std.contract.from_validator
            (fun
              value
              =>
              let checkEqual
                = fun
                  input
                  variant
                  =>
                  (input == variant)
                  || ((std.is_enum input)
                  && (((std.string.from_enum input) == variant)
                  | Bool))
                in
              if std.array.any (checkEqual value) variants then
                  'Ok
                else
                  'Error
                    {
                      message =
                        "expected one of %{std.serialize 'Json variants}",
                    }),
    if_then_else
      | Dyn -> Dyn -> Dyn -> Dyn
      = fun
          iph
          zen
          els
          =>
          std.contract.custom
            (fun
              label
              value
              =>
              (match {
                'Ok
                _ =>
                  std.contract.check zen label value,
                'Error
                _ =>
                  std.contract.check els label value,
              }) (std.contract.check iph label value)),
    number =
      let from_predicate
          | (Number -> Bool) -> String -> Dyn
          = fun
            pred
            msg
            =>
            std.contract.from_validator
              (fun
                value
                =>
                if !(std.is_number value) then
                  'Error { message = "expected a number", }
                else
                  if pred (value | Number) then
                    'Ok
                  else
                    'Error { message = msg, })
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
              = fun
                  limit
                  =>
                  from_predicate (fun x => x < limit)
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
              = fun
                  limit
                  =>
                  from_predicate (fun x => x > limit)
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
              = fun
                  limit
                  =>
                  from_predicate (fun x => x <= limit)
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
              = fun
                  limit
                  =>
                  from_predicate (fun x => x >= limit)
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
              = fun
                  n
                  =>
                  std.contract.from_validator
                    (fun
                      value
                      =>
                      if !(std.is_number value) then
                        'Error { message = "expected a number", }
                      else
                        if n == 0 then
                          'Error { message = "expected a multiple of 0", }
                        else
                          if std.number.is_integer (value / n) then
                            'Ok
                          else
                            'Error
                              {
                                message =
                                  "expected a multiple of %{std.string.from_number
                                      n}",
                              }),
          },
    one_of
      | Array Dyn -> Dyn
      = fun
          contracts
          =>
          std.contract.custom
            (fun
              label
              value
              =>
              let count_ok
                = std.array.length
                  (std.array.filter
                    (fun
                      ctr
                      =>
                      (match { 'Ok _ => true, 'Error _ => false, })
                        (std.contract.check ctr label value)) contracts)
                in
              if count_ok == 1 then
                  'Ok value
                else
                  'Error
                    {
                      message =
                        "%{std.to_string
                            count_ok} of the contracts passed, expected precisely one to pass",
                    }),
    record =
      let ErrData
          = { message | String | optional, notes | Array String | optional, }
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
              = fun
                  deps
                  =>
                  std.contract.custom
                    (fun
                      label
                      value
                      =>
                      if !(std.is_record value) then
                        'Error { message = "expected a record", }
                      else
                        std.array.try_fold_left
                          (fun
                            acc
                            { field, value = contract, }
                            =>
                            if std.record.has_field field value then
                              std.contract.check contract label acc
                            else
                              'Ok acc) value (std.record.to_array deps)),
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
              = fun
                  dependencies
                  =>
                  std.contract.from_validator
                    (fun
                      value
                      =>
                      if !(std.is_record value) then
                        'Error { message = "expected a record", }
                      else
                        let must_have_fields
                          : Array String
                          -> { _ : Dyn } -> [| 'Ok, 'Error ErrData |]
                          = fun
                            fields
                            x
                            =>
                            std.array.fold_right
                              (fun
                                field
                                acc
                                =>
                                if !(std.record.has_field field x) then
                                  'Error
                                    ({ message = "expected field %{field}", }
                                    | ErrData)
                                else
                                  acc) 'Ok fields
                          in
                        std.array.fold_right
                            (fun
                              { field, value = dependent_fields, }
                              acc
                              =>
                              if std.record.has_field field value then
                                (match {
                                  'Ok =>
                                    acc,
                                  'Error
                                  e =>
                                    'Error
                                      (e
                                      & {
                                        notes =
                                          [
                                              "because field %{field} was present"
                                            ],
                                      }),
                                }) (must_have_fields dependent_fields value)
                              else
                                acc) 'Ok (std.record.to_array dependencies)),
            FieldsMatch
              | String -> Dyn
              = fun
                  regex
                  =>
                  let is_match
                    = std.string.is_match regex
                    in
                  std.contract.from_validator
                      (fun
                        x
                        =>
                        if !(std.is_record x) then
                          'Error { message = "expected a record", }
                        else
                          let bad_field_names
                            = std.array.map (fun k => "\"${k}\"")
                              (std.array.filter (fun k => !(is_match k))
                                (std.record.fields x))
                            in
                          if (std.array.length bad_field_names) > 0 then
                              'Error
                                {
                                  message =
                                    "invalid field names %{std.string.join ","
                                        bad_field_names}",
                                  notes =
                                    [ "expected fields to match %{regex}" ],
                                }
                            else
                              'Ok),
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
              = fun
                  n
                  =>
                  std.contract.from_validator
                    (fun
                      value
                      =>
                      if !(std.is_record value) then
                        'Error { message = "expected a record", }
                      else
                        if (std.record.length value) > n then
                          'Error
                            {
                              message =
                                "expected a record with at most %{std.string.from_number
                                    n} fields",
                            }
                        else
                          'Ok),
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
              = fun
                  n
                  =>
                  std.contract.from_validator
                    (fun
                      value
                      =>
                      if !(std.is_record value) then
                        'Error { message = "expected a record", }
                      else
                        if (std.record.length value) < n then
                          'Error
                            {
                              message =
                                "expected a record with at least %{std.string.from_number
                                    n} fields",
                            }
                        else
                          'Ok),
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
              = fun
                  contract
                  =>
                  std.contract.custom
                    (fun
                      label
                      value
                      =>
                      if !(std.is_record value) then
                        'Error { message = "expected a record", }
                      else
                        std.array.try_fold_left
                          (fun
                            acc
                            field_name
                            =>
                            (match {
                              'Ok
                              _ =>
                                'Ok value,
                              'Error
                              e =>
                                'Error e,
                            }) (std.contract.check contract label field_name))
                          ('Ok value) (std.record.fields value)),
            Record
              | {
                  additional | [| 'None, 'Some Dyn |],
                  patterns | { _ | Dyn },
                  properties | { _ | Dyn },
                  required | { _ | Dyn },
                }
              -> Dyn
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
              = fun
                  { properties, required, patterns, additional, }
                  =>
                  std.contract.custom
                    (fun
                      label
                      x
                      =>
                      if !(std.is_record x) then
                        'Error { message = "expected a record", }
                      else
                        let x
                          | { _ | Dyn }
                          = x
                          in
                        let required
                            | { _ | Dyn }
                            = required
                            in
                          let checked_properties
                              : [| 'Ok ({ _ | Dyn }), 'Error ErrData |]
                              = std.array.try_fold_left
                                (fun
                                  acc
                                  { field, value = contract, }
                                  =>
                                  if !(std.record.has_field field x) then
                                    if std.record.has_field field required then
                                      'Error
                                        ({
                                          message =
                                            "missing required field `%{field}`",
                                        }
                                        | ErrData)
                                    else
                                      'Ok acc
                                  else
                                    (match {
                                      'Ok
                                      checked_value =>
                                        'Ok
                                          (std.record.insert field checked_value
                                            acc),
                                      'Error
                                      e =>
                                        'Error e,
                                    })
                                      (std.contract.check contract label
                                        x."%{field}")) ({} | { _ | Dyn })
                                (std.record.to_array (properties | { _ : Dyn }))
                              in
                            let matching_fields
                                : String -> { _ : Dyn }
                                = fun
                                  pattern
                                  =>
                                  let matcher
                                    = std.string.is_match pattern
                                    in
                                  std.array.fold_left
                                      (fun
                                        acc
                                        { field, value, }
                                        =>
                                        if matcher field then
                                          std.record.insert field value acc
                                        else
                                          acc) {} (std.record.to_array x)
                                in
                              let checked_pattern_properties
                                  : [| 'Ok ({ _ | Dyn }), 'Error ErrData |]
                                  = std.array.try_fold_left
                                    (fun
                                      acc
                                      { field = pattern, value = contract, }
                                      =>
                                      (match {
                                        'Ok
                                        checked_dict =>
                                          'Ok
                                            ((acc & checked_dict)
                                            | { _ | Dyn }),
                                        'Error
                                        e =>
                                          'Error e,
                                      })
                                        (std.array.try_fold_left
                                          (fun
                                            acc
                                            { field, value, }
                                            =>
                                            (match {
                                              'Ok
                                              checked_value =>
                                                'Ok
                                                  (std.record.insert field
                                                    checked_value acc),
                                              'Error
                                              e =>
                                                'Error e,
                                            })
                                              (std.contract.check contract label
                                                value)) ({} | { _ | Dyn })
                                          (std.record.to_array
                                            (matching_fields pattern))))
                                    ({}
                                    | { _ | Dyn })
                                    (std.record.to_array
                                      (patterns
                                      | { _ : Dyn }))
                                  in
                                (match {
                                    ['Error e, _] =>
                                      'Error e,
                                    [_, 'Error e] =>
                                      'Error e,
                                    ['Ok
                                      checked_properties,
                                      'Ok
                                      checked_pattern_properties] =>
                                      let remaining_fields
                                        : { _ | Dyn }
                                        = std.array.fold_left
                                          (fun
                                            acc
                                            field
                                            =>
                                            if !(std.record.has_field field
                                              acc) then
                                              acc
                                            else
                                              std.record.remove field acc) x
                                          ((std.record.fields
                                            (checked_properties
                                            | { _ : Dyn }))
                                          @ (std.record.fields
                                            (checked_pattern_properties
                                            | { _ : Dyn })))
                                        in
                                      let checked_additional
                                          : [|
                                            'Ok (
                                            { _ | Dyn }
                                            ),
                                            'Error ErrData
                                          |]
                                          = (match {
                                            'None =>
                                              if !(std.record.is_empty
                                                remaining_fields) then
                                                'Error
                                                  ({
                                                    message =
                                                      "extra fields %{std.serialize
                                                          'Json
                                                          ((std.record.fields
                                                            remaining_fields)
                                                          | Dyn)}",
                                                  }
                                                  | ErrData)
                                              else
                                                'Ok ({} | { _ | Dyn }),
                                            'Some
                                            contract =>
                                              std.array.try_fold_left
                                                (fun
                                                  acc
                                                  { field, value, }
                                                  =>
                                                  (match {
                                                    'Ok
                                                    checked_value =>
                                                      'Ok
                                                        (std.record.insert field
                                                          checked_value acc),
                                                    'Error
                                                    e =>
                                                      'Error e,
                                                  })
                                                    (std.contract.check contract
                                                      label value))
                                                ({}
                                                | { _ | Dyn })
                                                (std.record.to_array
                                                  remaining_fields),
                                          })
                                            (additional
                                            | [| 'None, 'Some Dyn |])
                                          in
                                        (match {
                                            'Error
                                            e =>
                                              'Error e,
                                            'Ok
                                            checked_additional =>
                                              'Ok
                                                ((checked_properties
                                                & checked_pattern_properties)
                                                & checked_additional),
                                          }) checked_additional,
                                  })
                                    [
                                      checked_properties,
                                      checked_pattern_properties
                                    ]),
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
              = fun
                  fields
                  =>
                  std.contract.from_validator
                    (fun
                      value
                      =>
                      if !(std.is_record value) then
                        'Error { message = "expected a record", }
                      else
                        (match { 'Error e => 'Error e, 'Ok _ => 'Ok, })
                          (std.array.try_fold_left
                            (fun
                              acc
                              field
                              =>
                              if !(std.record.has_field field value) then
                                'Error
                                  {
                                    message = "missing required field %{field}",
                                  }
                              else
                                'Ok acc) null fields)),
          },
    string =
      {
          Matches
            | String -> Dyn
            | doc m%"
              Enforces that the value is a string matching the supplied regular expression.
              
              # Examples
              
              ```nickel multiline
              "hello" | std.string.Matches "^[a-z]+$"
              # => "hello"
              
              "42" | std.string.Matches "^[a-z]+$"
              # => error
              ```
            "%
            = fun
                regex
                =>
                let is_match
                  = std.string.is_match regex
                  in
                std.contract.custom
                    (fun
                      _label
                      value
                      =>
                      if !((std.typeof value) == '"String") then
                        'Error { message = "expected a string", }
                      else
                        if is_match value then
                          'Ok value
                        else
                          'Error
                            {
                              message = "expected a string matching `%{regex}`",
                            }),
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
            = fun
                limit
                =>
                std.contract.from_validator
                  (fun
                    value
                    =>
                    if !(std.is_string value) then
                      'Error { message = "expected a string", }
                    else
                      if (std.string.length value) <= limit then
                        'Ok
                      else
                        'Error
                          {
                            message =
                              "expected a string of length at most %{std.string.from_number
                                  limit}",
                          }),
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
            = fun
                limit
                =>
                std.contract.from_validator
                  (fun
                    value
                    =>
                    if !(std.is_string value) then
                      'Error { message = "expected a string", }
                    else
                      if (std.string.length value) >= limit then
                        'Ok
                      else
                        'Error
                          {
                            message =
                              "expected a string of length at least %{std.string.from_number
                                  limit}",
                          }),
        },
  }
  in
let rec refs
    = {
      "definitions.branch" =
        std.contract.Sequence [ refs."definitions.globs", Array Dyn ],
      "definitions.concurrency" =
        {
            cancel-in-progress
              | std.contract.any_of
                [
                  Bool,
                  std.contract.Sequence
                    [ refs."definitions.expressionSyntax", String ]
                ]
              | doc "To cancel any currently running job or workflow in the same concurrency group, specify cancel-in-progress: true."
              | optional,
            group
              | String
              | doc "When a concurrent job or workflow is queued, if another job or workflow using the same concurrency group in the repository is in progress, the queued job or workflow will be pending. Any previously pending job or workflow in the concurrency group will be canceled.",
          },
      "definitions.configuration" =
        std.contract.any_of
            [ String, Number, Bool, { _ | js2n.Always }, Array js2n.Always ],
      "definitions.container" =
        {
            credentials
              | {
                password | String | optional,
                username | String | optional,
                ..
              }
              | doc "If the image's container registry requires authentication to pull the image, you can use credentials to set a map of the username and password. The credentials are the same values that you would provide to the `docker login` command."
              | optional,
            env
              | refs."definitions.env"
              | doc "Sets an array of environment variables in the container."
              | optional,
            image
              | String
              | doc "The Docker image to use as the container to run the action. The value can be the Docker Hub image name or a registry name.",
            options
              | String
              | doc "Additional Docker container resource options. For a list of options, see https://docs.docker.com/engine/reference/commandline/create/#options."
              | optional,
            ports
              | js2n.array.MinItems 1
              | Array (std.contract.any_of [ Number, String ])
              | doc "Sets an array of ports to expose on the container."
              | optional,
            volumes
              | js2n.array.MinItems 1
              | Array (js2n.string.Matches "^[^:]+:[^:]+$")
              | doc m%"
                Sets an array of volumes for the container to use. You can use volumes to share data between services or other steps in a job. You can specify named Docker volumes, anonymous Docker volumes, or bind mounts on the host.
                To specify a volume, you specify the source and destination path: <source>:<destinationPath>
                The <source> is a volume name or an absolute path on the host machine, and <destinationPath> is an absolute path in the container.
              "%
              | optional,
          },
      "definitions.defaults" =
        std.contract.Sequence
            [
              js2n.record.MinProperties 1,
              {
                run
                  | js2n.record.MinProperties 1
                  | {
                    shell | refs."definitions.shell" | String | optional,
                    working-directory
                      | refs."definitions.working-directory"
                      | String
                      | optional,
                  }
                  | optional,
              }
            ],
      "definitions.env" =
        std.contract.any_of
            [
              { _ | std.contract.any_of [ Bool, Number, String ] },
              std.contract.Sequence
                [ refs."definitions.stringContainingExpressionSyntax", String ]
            ],
      "definitions.environment" =
        {
            name
              | String
              | doc "The name of the environment configured in the repo.",
            url | String | doc "A deployment URL" | optional,
          },
      "definitions.event" =
        std.contract.Sequence
            [
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
      "definitions.eventObject" = js2n.Nullable {  .. },
      "definitions.expressionSyntax" =
        js2n.string.Matches "^\\$\\{\\{(.|[\r\n])*\\}\\}$",
      "definitions.globs" =
        std.contract.Sequence
            [ js2n.array.MinItems 1, Array (js2n.string.MinLength 1) ],
      "definitions.jobNeeds" =
        std.contract.any_of
            [
              std.contract.Sequence
                [
                  js2n.array.MinItems 1,
                  Array (
                    std.contract.Sequence [ refs."definitions.name", String ]
                  )
                ],
              std.contract.Sequence [ refs."definitions.name", String ]
            ],
      "definitions.matrix" =
        std.contract.any_of
            [
              std.contract.Sequence
                [
                  js2n.record.MinProperties 1,
                  js2n.record.Record
                    {
                      additional =
                        'Some
                            (std.contract.any_of
                              [
                                std.contract.Sequence
                                  [
                                    js2n.array.MinItems 1,
                                    Array refs."definitions.configuration"
                                  ],
                                std.contract.Sequence
                                  [
                                    refs."definitions.expressionSyntax",
                                    String
                                  ]
                              ]),
                      patterns = {},
                      properties =
                        {
                            exclude =
                              std.contract.any_of
                                  [
                                    std.contract.Sequence
                                      [
                                        refs."definitions.expressionSyntax",
                                        String
                                      ],
                                    std.contract.Sequence
                                      [
                                        js2n.array.MinItems 1,
                                        Array (
                                          {
                                            _ | refs."definitions.configuration"
                                          }
                                        )
                                      ]
                                  ],
                            include =
                              std.contract.any_of
                                  [
                                    std.contract.Sequence
                                      [
                                        refs."definitions.expressionSyntax",
                                        String
                                      ],
                                    std.contract.Sequence
                                      [
                                        js2n.array.MinItems 1,
                                        Array (
                                          {
                                            _ | refs."definitions.configuration"
                                          }
                                        )
                                      ]
                                  ],
                          },
                      required = {},
                    }
                ],
              std.contract.Sequence
                [ refs."definitions.expressionSyntax", String ]
            ],
      "definitions.name" = js2n.string.Matches "^[_a-zA-Z][a-zA-Z0-9_-]*$",
      "definitions.normalJob" =
        {
            concurrency
              | std.contract.any_of
                [
                  String,
                  std.contract.Sequence
                    [ refs."definitions.concurrency", { _ | Dyn } ]
                ]
              | doc m%"
                Concurrency ensures that only a single job or workflow using the same concurrency group will run at a time. A concurrency group can be any string or expression. The expression can use any context except for the secrets context. 
                You can also specify concurrency at the workflow level. 
                When a concurrent job or workflow is queued, if another job or workflow using the same concurrency group in the repository is in progress, the queued job or workflow will be pending. Any previously pending job or workflow in the concurrency group will be canceled. To also cancel any currently running job or workflow in the same concurrency group, specify cancel-in-progress: true.
              "%
              | optional,
            container
              | std.contract.any_of
                [
                  String,
                  std.contract.Sequence
                    [ refs."definitions.container", { _ | Dyn } ]
                ]
              | doc m%"
                A container to run any steps in a job that don't already specify a container. If you have steps that use both script and container actions, the container actions will run as sibling containers on the same network with the same volume mounts.
                If you do not set a container, all steps will run directly on the host specified by runs-on unless a step refers to an action configured to run in a container.
              "%
              | optional,
            continue-on-error
              | std.contract.any_of
                [
                  Bool,
                  std.contract.Sequence
                    [ refs."definitions.expressionSyntax", String ]
                ]
              | doc "Prevents a workflow run from failing when a job fails. Set to true to allow a workflow run to pass when this job fails."
              | optional,
            defaults
              | refs."definitions.defaults"
              | { _ | Dyn }
              | doc "A map of default settings that will apply to all steps in the job."
              | optional,
            env
              | refs."definitions.env"
              | doc "A map of environment variables that are available to all steps in the job."
              | optional,
            environment
              | std.contract.any_of
                [
                  String,
                  std.contract.Sequence
                    [ refs."definitions.environment", { _ | Dyn } ]
                ]
              | doc "The environment that the job references."
              | optional,
            "if"
              | std.contract.any_of [ Bool, Number, String ]
              | doc m%"
                You can use the if conditional to prevent a job from running unless a condition is met. You can use any supported context and expression to create a conditional.
                Expressions in an if conditional do not require the ${{ }} syntax. For more information, see https://help.github.com/en/articles/contexts-and-expression-syntax-for-github-actions.
              "%
              | optional,
            name
              | String
              | doc "The name of the job displayed on GitHub."
              | optional,
            needs | refs."definitions.jobNeeds" | optional,
            outputs
              | js2n.record.MinProperties 1
              | { _ | String }
              | doc "A map of outputs for a job. Job outputs are available to all downstream jobs that depend on this job."
              | optional,
            permissions | refs."definitions.permissions" | optional,
            runs-on
              | std.contract.any_of
                [
                  String,
                  std.contract.Sequence
                    [
                      js2n.array.MinItems 1,
                      js2n.array.Items [ String ] js2n.Always
                    ],
                  js2n.record.Record
                    {
                      additional = 'Some Dyn,
                      patterns = {},
                      properties =
                        {
                            group = String,
                            labels =
                              std.contract.any_of
                                  [ String, js2n.array.ArrayOf String ],
                          },
                      required = {},
                    },
                  std.contract.Sequence
                    [
                      refs."definitions.stringContainingExpressionSyntax",
                      String
                    ],
                  std.contract.Sequence
                    [ refs."definitions.expressionSyntax", String ]
                ]
              | doc "The type of machine to run the job on. The machine can be either a GitHub-hosted runner, or a self-hosted runner.",
            services
              | {
                _ | std.contract.Sequence
                  [ refs."definitions.container", { _ | Dyn } ]
              }
              | doc m%"
                Additional containers to host services for a job in a workflow. These are useful for creating databases or cache services like redis. The runner on the virtual machine will automatically create a network and manage the life cycle of the service containers.
                When you use a service container for a job or your step uses container actions, you don't need to set port information to access the service. Docker automatically exposes all ports between containers on the same network.
                When both the job and the action run in a container, you can directly reference the container by its hostname. The hostname is automatically mapped to the service name.
                When a step does not use a container action, you must access the service using localhost and bind the ports.
              "%
              | optional,
            steps
              | js2n.array.MinItems 1
              | Array (
                std.contract.Sequence [ refs."definitions.step", { _ | Dyn } ]
              )
              | doc m%"
                A job contains a sequence of tasks called steps. Steps can run commands, run setup tasks, or run an action in your repository, a public repository, or an action published in a Docker registry. Not all steps run actions, but all actions run as a step. Each step runs in its own process in the virtual environment and has access to the workspace and filesystem. Because steps run in their own process, changes to environment variables are not preserved between steps. GitHub provides built-in steps to set up and complete a job.
                Must contain either `uses` or `run`
                
              "%
              | optional,
            strategy
              | {
                fail-fast
                  | std.contract.any_of [ Bool, String ]
                  | doc "When set to true, GitHub cancels all in-progress jobs if any matrix job fails. Default: true"
                  | optional,
                matrix | refs."definitions.matrix",
                max-parallel
                  | std.contract.any_of [ Number, String ]
                  | doc "The maximum number of jobs that can run simultaneously when using a matrix job strategy. By default, GitHub will maximize the number of jobs run in parallel depending on the available runners on GitHub-hosted virtual machines."
                  | optional,
              }
              | doc "A strategy creates a build matrix for your jobs. You can define different variations of an environment to run each job in."
              | optional,
            timeout-minutes
              | std.contract.any_of
                [
                  Number,
                  std.contract.Sequence
                    [ refs."definitions.expressionSyntax", String ]
                ]
              | doc "The maximum number of minutes to let a workflow run before GitHub automatically cancels it. Default: 360"
              | optional,
          },
      "definitions.path" =
        std.contract.Sequence [ refs."definitions.globs", Array Dyn ],
      "definitions.permissions" =
        std.contract.any_of
            [
              std.contract.Sequence
                [ std.enum.TagOrString, [| 'write-all, 'read-all |] ],
              std.contract.Sequence
                [ refs."definitions.permissions-event", { _ | Dyn } ]
            ],
      "definitions.permissions-event" =
        {
            actions | refs."definitions.permissions-level" | String | optional,
            attestations
              | refs."definitions.permissions-level"
              | String
              | optional,
            checks | refs."definitions.permissions-level" | String | optional,
            contents | refs."definitions.permissions-level" | String | optional,
            deployments
              | refs."definitions.permissions-level"
              | String
              | optional,
            discussions
              | refs."definitions.permissions-level"
              | String
              | optional,
            id-token | refs."definitions.permissions-level" | String | optional,
            issues | refs."definitions.permissions-level" | String | optional,
            packages | refs."definitions.permissions-level" | String | optional,
            pages | refs."definitions.permissions-level" | String | optional,
            pull-requests
              | refs."definitions.permissions-level"
              | String
              | optional,
            repository-projects
              | refs."definitions.permissions-level"
              | String
              | optional,
            security-events
              | refs."definitions.permissions-level"
              | String
              | optional,
            statuses | refs."definitions.permissions-level" | String | optional,
          },
      "definitions.permissions-level" =
        std.contract.Sequence
            [ std.enum.TagOrString, [| 'none, 'write, 'read |] ],
      "definitions.ref" =
        std.contract.Sequence
            [
              js2n.Nullable
                {
                  branches | refs."definitions.branch" | Array Dyn | optional,
                  branches-ignore
                    | refs."definitions.branch"
                    | Array Dyn
                    | optional,
                  paths | refs."definitions.path" | Array Dyn | optional,
                  paths-ignore | refs."definitions.path" | Array Dyn | optional,
                  tags | refs."definitions.branch" | Array Dyn | optional,
                  tags-ignore
                    | refs."definitions.branch"
                    | Array Dyn
                    | optional,
                  ..
                },
              js2n.Nullable
                (std.contract.Sequence
                  [
                    std.contract.not
                      (std.contract.any_of
                        [
                          js2n.Null,
                          Bool,
                          Number,
                          String,
                          js2n.record.Required
                            [ "branches", "branches-ignore" ],
                          Array Dyn
                        ]),
                    std.contract.not
                      (std.contract.any_of
                        [
                          js2n.Null,
                          Bool,
                          Number,
                          String,
                          js2n.record.Required [ "tags", "tags-ignore" ],
                          Array Dyn
                        ]),
                    std.contract.not
                      (std.contract.any_of
                        [
                          js2n.Null,
                          Bool,
                          Number,
                          String,
                          js2n.record.Required [ "paths", "paths-ignore" ],
                          Array Dyn
                        ]),
                    { _ | Dyn }
                  ])
            ],
      "definitions.reusableWorkflowCallJob" =
        {
            concurrency
              | std.contract.any_of
                [
                  String,
                  std.contract.Sequence
                    [ refs."definitions.concurrency", { _ | Dyn } ]
                ]
              | doc m%"
                Concurrency ensures that only a single job or workflow using the same concurrency group will run at a time. A concurrency group can be any string or expression. The expression can use any context except for the secrets context. 
                You can also specify concurrency at the workflow level. 
                When a concurrent job or workflow is queued, if another job or workflow using the same concurrency group in the repository is in progress, the queued job or workflow will be pending. Any previously pending job or workflow in the concurrency group will be canceled. To also cancel any currently running job or workflow in the same concurrency group, specify cancel-in-progress: true.
              "%
              | optional,
            "if"
              | std.contract.any_of [ Bool, Number, String ]
              | doc m%"
                You can use the if conditional to prevent a job from running unless a condition is met. You can use any supported context and expression to create a conditional.
                Expressions in an if conditional do not require the ${{ }} syntax. For more information, see https://help.github.com/en/articles/contexts-and-expression-syntax-for-github-actions.
              "%
              | optional,
            name
              | String
              | doc "The name of the job displayed on GitHub."
              | optional,
            needs | refs."definitions.jobNeeds" | optional,
            permissions | refs."definitions.permissions" | optional,
            secrets
              | js2n.one_of
                [
                  refs."definitions.env",
                  std.contract.Sequence [ std.enum.TagOrString, [| 'inherit |] ]
                ]
              | doc "When a job is used to call a reusable workflow, you can use 'secrets' to provide a map of secrets that are passed to the called workflow. Any secrets that you pass must match the names defined in the called workflow."
              | optional,
            strategy
              | {
                fail-fast
                  | std.contract.any_of [ Bool, String ]
                  | doc "When set to true, GitHub cancels all in-progress jobs if any matrix job fails. Default: true"
                  | optional,
                matrix | refs."definitions.matrix",
                max-parallel
                  | std.contract.any_of [ Number, String ]
                  | doc "The maximum number of jobs that can run simultaneously when using a matrix job strategy. By default, GitHub will maximize the number of jobs run in parallel depending on the available runners on GitHub-hosted virtual machines."
                  | optional,
              }
              | doc "A strategy creates a build matrix for your jobs. You can define different variations of an environment to run each job in."
              | optional,
            uses
              | js2n.string.Matches "^(.+\\/)+(.+)\\.(ya?ml)(@.+)?$"
              | doc "The location and version of a reusable workflow file to run as a job, of the form './{path/to}/{localfile}.yml' or '{owner}/{repo}/{path}/{filename}@{ref}'. {ref} can be a SHA, a release tag, or a branch name. Using the commit SHA is the safest for stability and security.",
            with
              | refs."definitions.env"
              | doc "A map of inputs that are passed to the called workflow. Any inputs that you pass must match the input specifications defined in the called workflow. Unlike 'jobs.<job_id>.steps[*].with', the inputs you pass with 'jobs.<job_id>.with' are not be available as environment variables in the called workflow. Instead, you can reference the inputs by using the inputs context."
              | optional,
          },
      "definitions.shell" =
        std.contract.any_of
            [
              String,
              std.contract.Sequence
                [
                  std.enum.TagOrString,
                  [| 'powershell, 'cmd, 'sh, 'python, 'pwsh, 'bash |]
                ]
            ],
      "definitions.step" =
        std.contract.Sequence
            [
              js2n.record.DependentFields
                { shell = [ "run" ], working-directory = [ "run" ], },
              js2n.one_of
                [
                  std.contract.any_of
                    [
                      js2n.Null,
                      Bool,
                      Number,
                      String,
                      js2n.record.Required [ "uses" ],
                      Array Dyn
                    ],
                  std.contract.any_of
                    [
                      js2n.Null,
                      Bool,
                      Number,
                      String,
                      js2n.record.Required [ "run" ],
                      Array Dyn
                    ]
                ],
              {
                continue-on-error
                  | std.contract.any_of
                    [
                      Bool,
                      std.contract.Sequence
                        [ refs."definitions.expressionSyntax", String ]
                    ]
                  | doc "Prevents a job from failing when a step fails. Set to true to allow a job to pass when this step fails."
                  | optional,
                env
                  | refs."definitions.env"
                  | doc "Sets environment variables for steps to use in the virtual environment. You can also set environment variables for the entire workflow or a job."
                  | optional,
                id
                  | String
                  | doc "A unique identifier for the step. You can use the id to reference the step in contexts. For more information, see https://help.github.com/en/articles/contexts-and-expression-syntax-for-github-actions."
                  | optional,
                "if"
                  | std.contract.any_of [ Bool, Number, String ]
                  | doc m%"
                    You can use the if conditional to prevent a step from running unless a condition is met. You can use any supported context and expression to create a conditional.
                    Expressions in an if conditional do not require the ${{ }} syntax. For more information, see https://help.github.com/en/articles/contexts-and-expression-syntax-for-github-actions.
                  "%
                  | optional,
                name
                  | String
                  | doc "A name for your step to display on GitHub."
                  | optional,
                run
                  | String
                  | doc m%"
                    Runs command-line programs using the operating system's shell. If you do not provide a name, the step name will default to the text specified in the run command.
                    Commands run using non-login shells by default. You can choose a different shell and customize the shell used to run commands. For more information, see https://help.github.com/en/actions/automating-your-workflow-with-github-actions/workflow-syntax-for-github-actions#using-a-specific-shell.
                    Each run keyword represents a new process and shell in the virtual environment. When you provide multi-line commands, each line runs in the same shell.
                  "%
                  | optional,
                shell | refs."definitions.shell" | String | optional,
                timeout-minutes
                  | std.contract.any_of
                    [
                      Number,
                      std.contract.Sequence
                        [ refs."definitions.expressionSyntax", String ]
                    ]
                  | doc "The maximum number of minutes to run the step before killing the process."
                  | optional,
                uses
                  | String
                  | doc m%"
                    Selects an action to run as part of a step in your job. An action is a reusable unit of code. You can use an action defined in the same repository as the workflow, a public repository, or in a published Docker container image (https://hub.docker.com/).
                    We strongly recommend that you include the version of the action you are using by specifying a Git ref, SHA, or Docker tag number. If you don't specify a version, it could break your workflows or cause unexpected behavior when the action owner publishes an update.
                    - Using the commit SHA of a released action version is the safest for stability and security.
                    - Using the specific major action version allows you to receive critical fixes and security patches while still maintaining compatibility. It also assures that your workflow should still work.
                    - Using the master branch of an action may be convenient, but if someone releases a new major version with a breaking change, your workflow could break.
                    Some actions require inputs that you must set using the with keyword. Review the action's README file to determine the inputs required.
                    Actions are either JavaScript files or Docker containers. If the action you're using is a Docker container you must run the job in a Linux virtual environment. For more details, see https://help.github.com/en/articles/virtual-environments-for-github-actions.
                  "%
                  | optional,
                with
                  | std.contract.any_of
                    [
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
                  | refs."definitions.env"
                  | doc "A map of the input parameters defined by the action. Each input parameter is a key/value pair. Input parameters are set as environment variables. The variable is prefixed with INPUT_ and converted to upper case."
                  | optional,
                working-directory
                  | refs."definitions.working-directory"
                  | String
                  | optional,
              }
            ],
      "definitions.stringContainingExpressionSyntax" =
        js2n.string.Matches "^.*\\$\\{\\{(.|[\r\n])*\\}\\}.*$",
      "definitions.types" = js2n.array.MinItems 1,
      "definitions.workflowDispatchInput" =
        std.contract.Sequence
            [
              js2n.if_then_else
                (std.contract.any_of
                  [
                    js2n.Null,
                    Bool,
                    Number,
                    String,
                    js2n.record.Record
                      {
                        additional = 'Some Dyn,
                        patterns = {},
                        properties = { type = js2n.Const "string", },
                        required = { type = true, },
                      },
                    Array Dyn
                  ])
                (std.contract.any_of
                  [
                    js2n.Null,
                    Bool,
                    Number,
                    String,
                    { "default" | String | optional, .. },
                    Array Dyn
                  ]) js2n.Always,
              js2n.if_then_else
                (std.contract.any_of
                  [
                    js2n.Null,
                    Bool,
                    Number,
                    String,
                    js2n.record.Record
                      {
                        additional = 'Some Dyn,
                        patterns = {},
                        properties = { type = js2n.Const "boolean", },
                        required = { type = true, },
                      },
                    Array Dyn
                  ])
                (std.contract.any_of
                  [
                    js2n.Null,
                    Bool,
                    Number,
                    String,
                    { "default" | Bool | optional, .. },
                    Array Dyn
                  ]) js2n.Always,
              js2n.if_then_else
                (std.contract.any_of
                  [
                    js2n.Null,
                    Bool,
                    Number,
                    String,
                    js2n.record.Record
                      {
                        additional = 'Some Dyn,
                        patterns = {},
                        properties = { type = js2n.Const "number", },
                        required = { type = true, },
                      },
                    Array Dyn
                  ])
                (std.contract.any_of
                  [
                    js2n.Null,
                    Bool,
                    Number,
                    String,
                    { "default" | Number | optional, .. },
                    Array Dyn
                  ]) js2n.Always,
              js2n.if_then_else
                (std.contract.any_of
                  [
                    js2n.Null,
                    Bool,
                    Number,
                    String,
                    js2n.record.Record
                      {
                        additional = 'Some Dyn,
                        patterns = {},
                        properties = { type = js2n.Const "environment", },
                        required = { type = true, },
                      },
                    Array Dyn
                  ])
                (std.contract.any_of
                  [
                    js2n.Null,
                    Bool,
                    Number,
                    String,
                    { "default" | String | optional, .. },
                    Array Dyn
                  ]) js2n.Always,
              js2n.if_then_else
                (std.contract.any_of
                  [
                    js2n.Null,
                    Bool,
                    Number,
                    String,
                    js2n.record.Record
                      {
                        additional = 'Some Dyn,
                        patterns = {},
                        properties = { type = js2n.Const "choice", },
                        required = { type = true, },
                      },
                    Array Dyn
                  ])
                (std.contract.any_of
                  [
                    js2n.Null,
                    Bool,
                    Number,
                    String,
                    js2n.record.Required [ "options" ],
                    Array Dyn
                  ]) js2n.Always,
              {
                "default"
                  | std.contract.any_of
                    [ js2n.Null, Bool, Number, String, { _ | Dyn }, Array Dyn ]
                  | doc "A string representing the default value. The default value is used when an input parameter isn't specified in a workflow file."
                  | optional,
                deprecationMessage
                  | String
                  | doc "A string shown to users using the deprecated input."
                  | optional,
                description
                  | String
                  | doc "A string description of the input parameter.",
                options
                  | js2n.array.MinItems 1
                  | Array String
                  | doc "The options of the dropdown list, if the type is a choice."
                  | optional,
                required
                  | Bool
                  | doc "A boolean to indicate whether the action requires the input parameter. Set to true when the parameter is required."
                  | optional,
                type
                  | std.enum.TagOrString
                  | [| 'environment, 'number, 'boolean, 'choice, 'string |]
                  | doc "A string representing the type of the input."
                  | optional,
              }
            ],
      "definitions.working-directory" = String,
    }
    in
  {
      concurrency
        | std.contract.any_of [ String, refs."definitions.concurrency" ]
        | doc m%"
          Concurrency ensures that only a single job or workflow using the same concurrency group will run at a time. A concurrency group can be any string or expression. The expression can use any context except for the secrets context. 
          You can also specify concurrency at the workflow level. 
          When a concurrent job or workflow is queued, if another job or workflow using the same concurrency group in the repository is in progress, the queued job or workflow will be pending. Any previously pending job or workflow in the concurrency group will be canceled. To also cancel any currently running job or workflow in the same concurrency group, specify cancel-in-progress: true.
        "%
        | optional,
      defaults
        | refs."definitions.defaults"
        | { _ | Dyn }
        | doc "A map of default settings that will apply to all jobs in the workflow."
        | optional,
      env
        | refs."definitions.env"
        | doc "A map of environment variables that are available to all jobs and steps in the workflow."
        | optional,
      jobs
        | js2n.record.MinProperties 1
        | {
          _ | js2n.one_of
            [
              refs."definitions.normalJob",
              refs."definitions.reusableWorkflowCallJob"
            ]
        }
        | js2n.record.FieldsMatch "^[_a-zA-Z][a-zA-Z0-9_-]*$"
        | doc m%"
          A workflow run is made up of one or more jobs. Jobs run in parallel by default. To run jobs sequentially, you can define dependencies on other jobs using the jobs.<job_id>.needs keyword.
          Each job runs in a fresh instance of the virtual environment specified by runs-on.
          You can run an unlimited number of jobs as long as you are within the workflow usage limits. For more information, see https://help.github.com/en/github/automating-your-workflow-with-github-actions/workflow-syntax-for-github-actions#usage-limits.
        "%,
      name
        | String
        | doc "The name of your workflow. GitHub displays the names of your workflows on your repository's actions page. If you omit this field, GitHub sets the name to the workflow's filename."
        | optional,
      on
        | std.contract.any_of
          [
            refs."definitions.event",
            std.contract.Sequence
              [ js2n.array.MinItems 1, Array refs."definitions.event" ],
            {
              branch_protection_rule
                | js2n.Nullable
                  {
                    types
                      | Array (
                        std.contract.Sequence
                          [
                            std.enum.TagOrString,
                            [| 'deleted, 'edited, 'created |]
                          ]
                      )
                      | refs."definitions.types"
                      | optional,
                    ..
                  }
                | refs."definitions.eventObject"
                | doc "Runs your workflow anytime the branch_protection_rule event occurs. More than one activity type triggers this event."
                | optional,
              check_run
                | js2n.Nullable
                  {
                    types
                      | Array (
                        std.contract.Sequence
                          [
                            std.enum.TagOrString,
                            [|
                              'requested_action,
                              'completed,
                              'rerequested,
                              'created
                            |]
                          ]
                      )
                      | refs."definitions.types"
                      | optional,
                    ..
                  }
                | refs."definitions.eventObject"
                | doc "Runs your workflow anytime the check_run event occurs. More than one activity type triggers this event. For information about the REST API, see https://developer.github.com/v3/checks/runs."
                | optional,
              check_suite
                | js2n.Nullable
                  {
                    types
                      | Array (
                        std.contract.Sequence
                          [
                            std.enum.TagOrString,
                            [| 'rerequested, 'requested, 'completed |]
                          ]
                      )
                      | refs."definitions.types"
                      | optional,
                    ..
                  }
                | refs."definitions.eventObject"
                | doc "Runs your workflow anytime the check_suite event occurs. More than one activity type triggers this event. For information about the REST API, see https://developer.github.com/v3/checks/suites/."
                | optional,
              create
                | refs."definitions.eventObject"
                | doc "Runs your workflow anytime someone creates a branch or tag, which triggers the create event. For information about the REST API, see https://developer.github.com/v3/git/refs/#create-a-reference."
                | optional,
              delete
                | refs."definitions.eventObject"
                | doc "Runs your workflow anytime someone deletes a branch or tag, which triggers the delete event. For information about the REST API, see https://developer.github.com/v3/git/refs/#delete-a-reference."
                | optional,
              deployment
                | refs."definitions.eventObject"
                | doc "Runs your workflow anytime someone creates a deployment, which triggers the deployment event. Deployments created with a commit SHA may not have a Git ref. For information about the REST API, see https://developer.github.com/v3/repos/deployments/."
                | optional,
              deployment_status
                | refs."definitions.eventObject"
                | doc "Runs your workflow anytime a third party provides a deployment status, which triggers the deployment_status event. Deployments created with a commit SHA may not have a Git ref. For information about the REST API, see https://developer.github.com/v3/repos/deployments/#create-a-deployment-status."
                | optional,
              discussion
                | js2n.Nullable
                  {
                    types
                      | Array (
                        std.contract.Sequence
                          [
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
                      | refs."definitions.types"
                      | optional,
                    ..
                  }
                | refs."definitions.eventObject"
                | doc "Runs your workflow anytime the discussion event occurs. More than one activity type triggers this event. For information about the GraphQL API, see https://docs.github.com/en/graphql/guides/using-the-graphql-api-for-discussions"
                | optional,
              discussion_comment
                | js2n.Nullable
                  {
                    types
                      | Array (
                        std.contract.Sequence
                          [
                            std.enum.TagOrString,
                            [| 'deleted, 'edited, 'created |]
                          ]
                      )
                      | refs."definitions.types"
                      | optional,
                    ..
                  }
                | refs."definitions.eventObject"
                | doc "Runs your workflow anytime the discussion_comment event occurs. More than one activity type triggers this event. For information about the GraphQL API, see https://docs.github.com/en/graphql/guides/using-the-graphql-api-for-discussions"
                | optional,
              fork
                | refs."definitions.eventObject"
                | doc "Runs your workflow anytime when someone forks a repository, which triggers the fork event. For information about the REST API, see https://developer.github.com/v3/repos/forks/#create-a-fork."
                | optional,
              gollum
                | refs."definitions.eventObject"
                | doc "Runs your workflow when someone creates or updates a Wiki page, which triggers the gollum event."
                | optional,
              issue_comment
                | js2n.Nullable
                  {
                    types
                      | Array (
                        std.contract.Sequence
                          [
                            std.enum.TagOrString,
                            [| 'deleted, 'edited, 'created |]
                          ]
                      )
                      | refs."definitions.types"
                      | optional,
                    ..
                  }
                | refs."definitions.eventObject"
                | doc "Runs your workflow anytime the issue_comment event occurs. More than one activity type triggers this event. For information about the REST API, see https://developer.github.com/v3/issues/comments/."
                | optional,
              issues
                | js2n.Nullable
                  {
                    types
                      | Array (
                        std.contract.Sequence
                          [
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
                      | refs."definitions.types"
                      | optional,
                    ..
                  }
                | refs."definitions.eventObject"
                | doc "Runs your workflow anytime the issues event occurs. More than one activity type triggers this event. For information about the REST API, see https://developer.github.com/v3/issues."
                | optional,
              label
                | js2n.Nullable
                  {
                    types
                      | Array (
                        std.contract.Sequence
                          [
                            std.enum.TagOrString,
                            [| 'deleted, 'edited, 'created |]
                          ]
                      )
                      | refs."definitions.types"
                      | optional,
                    ..
                  }
                | refs."definitions.eventObject"
                | doc "Runs your workflow anytime the label event occurs. More than one activity type triggers this event. For information about the REST API, see https://developer.github.com/v3/issues/labels/."
                | optional,
              merge_group
                | js2n.Nullable
                  {
                    types
                      | Array (
                        std.contract.Sequence
                          [ std.enum.TagOrString, [| 'checks_requested |] ]
                      )
                      | refs."definitions.types"
                      | optional,
                    ..
                  }
                | refs."definitions.eventObject"
                | doc "Runs your workflow when a pull request is added to a merge queue, which adds the pull request to a merge group. For information about the merge queue, see https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/incorporating-changes-from-a-pull-request/merging-a-pull-request-with-a-merge-queue ."
                | optional,
              milestone
                | js2n.Nullable
                  {
                    types
                      | Array (
                        std.contract.Sequence
                          [
                            std.enum.TagOrString,
                            [| 'deleted, 'edited, 'opened, 'closed, 'created |]
                          ]
                      )
                      | refs."definitions.types"
                      | optional,
                    ..
                  }
                | refs."definitions.eventObject"
                | doc "Runs your workflow anytime the milestone event occurs. More than one activity type triggers this event. For information about the REST API, see https://developer.github.com/v3/issues/milestones/."
                | optional,
              page_build
                | refs."definitions.eventObject"
                | doc "Runs your workflow anytime someone pushes to a GitHub Pages-enabled branch, which triggers the page_build event. For information about the REST API, see https://developer.github.com/v3/repos/pages/."
                | optional,
              project
                | js2n.Nullable
                  {
                    types
                      | Array (
                        std.contract.Sequence
                          [
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
                      | refs."definitions.types"
                      | optional,
                    ..
                  }
                | refs."definitions.eventObject"
                | doc "Runs your workflow anytime the project event occurs. More than one activity type triggers this event. For information about the REST API, see https://developer.github.com/v3/projects/."
                | optional,
              project_card
                | js2n.Nullable
                  {
                    types
                      | Array (
                        std.contract.Sequence
                          [
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
                      | refs."definitions.types"
                      | optional,
                    ..
                  }
                | refs."definitions.eventObject"
                | doc "Runs your workflow anytime the project_card event occurs. More than one activity type triggers this event. For information about the REST API, see https://developer.github.com/v3/projects/cards."
                | optional,
              project_column
                | js2n.Nullable
                  {
                    types
                      | Array (
                        std.contract.Sequence
                          [
                            std.enum.TagOrString,
                            [| 'deleted, 'moved, 'updated, 'created |]
                          ]
                      )
                      | refs."definitions.types"
                      | optional,
                    ..
                  }
                | refs."definitions.eventObject"
                | doc "Runs your workflow anytime the project_column event occurs. More than one activity type triggers this event. For information about the REST API, see https://developer.github.com/v3/projects/columns."
                | optional,
              public
                | refs."definitions.eventObject"
                | doc "Runs your workflow anytime someone makes a private repository public, which triggers the public event. For information about the REST API, see https://developer.github.com/v3/repos/#edit."
                | optional,
              pull_request
                | js2n.Nullable
                  {
                    branches | Array Dyn | optional,
                    branches-ignore | Array Dyn | optional,
                    paths | Array Dyn | optional,
                    paths-ignore | Array Dyn | optional,
                    tags | Array Dyn | optional,
                    tags-ignore | Array Dyn | optional,
                    types
                      | Array (
                        std.contract.Sequence
                          [
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
                      | refs."definitions.types"
                      | optional,
                  }
                | refs."definitions.ref"
                | doc m%"
                  Runs your workflow anytime the pull_request event occurs. More than one activity type triggers this event. For information about the REST API, see https://developer.github.com/v3/pulls.
                  Note: Workflows do not run on private base repositories when you open a pull request from a forked repository.
                  When you create a pull request from a forked repository to the base repository, GitHub sends the pull_request event to the base repository and no pull request events occur on the forked repository.
                  Workflows don't run on forked repositories by default. You must enable GitHub Actions in the Actions tab of the forked repository.
                  The permissions for the GITHUB_TOKEN in forked repositories is read-only. For more information about the GITHUB_TOKEN, see https://help.github.com/en/articles/virtual-environments-for-github-actions.
                "%
                | optional,
              pull_request_review
                | js2n.Nullable
                  {
                    types
                      | Array (
                        std.contract.Sequence
                          [
                            std.enum.TagOrString,
                            [| 'dismissed, 'edited, 'submitted |]
                          ]
                      )
                      | refs."definitions.types"
                      | optional,
                    ..
                  }
                | refs."definitions.eventObject"
                | doc m%"
                  Runs your workflow anytime the pull_request_review event occurs. More than one activity type triggers this event. For information about the REST API, see https://developer.github.com/v3/pulls/reviews.
                  Note: Workflows do not run on private base repositories when you open a pull request from a forked repository.
                  When you create a pull request from a forked repository to the base repository, GitHub sends the pull_request event to the base repository and no pull request events occur on the forked repository.
                  Workflows don't run on forked repositories by default. You must enable GitHub Actions in the Actions tab of the forked repository.
                  The permissions for the GITHUB_TOKEN in forked repositories is read-only. For more information about the GITHUB_TOKEN, see https://help.github.com/en/articles/virtual-environments-for-github-actions.
                "%
                | optional,
              pull_request_review_comment
                | js2n.Nullable
                  {
                    types
                      | Array (
                        std.contract.Sequence
                          [
                            std.enum.TagOrString,
                            [| 'deleted, 'edited, 'created |]
                          ]
                      )
                      | refs."definitions.types"
                      | optional,
                    ..
                  }
                | refs."definitions.eventObject"
                | doc m%"
                  Runs your workflow anytime a comment on a pull request's unified diff is modified, which triggers the pull_request_review_comment event. More than one activity type triggers this event. For information about the REST API, see https://developer.github.com/v3/pulls/comments.
                  Note: Workflows do not run on private base repositories when you open a pull request from a forked repository.
                  When you create a pull request from a forked repository to the base repository, GitHub sends the pull_request event to the base repository and no pull request events occur on the forked repository.
                  Workflows don't run on forked repositories by default. You must enable GitHub Actions in the Actions tab of the forked repository.
                  The permissions for the GITHUB_TOKEN in forked repositories is read-only. For more information about the GITHUB_TOKEN, see https://help.github.com/en/articles/virtual-environments-for-github-actions.
                "%
                | optional,
              pull_request_target
                | js2n.Nullable
                  {
                    branches | js2n.Always | optional,
                    branches-ignore | js2n.Always | optional,
                    paths | js2n.Always | optional,
                    paths-ignore | js2n.Always | optional,
                    tags | js2n.Always | optional,
                    tags-ignore | js2n.Always | optional,
                    types
                      | Array (
                        std.contract.Sequence
                          [
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
                      | refs."definitions.types"
                      | optional,
                  }
                | refs."definitions.ref"
                | doc "This event is similar to pull_request, except that it runs in the context of the base repository of the pull request, rather than in the merge commit. This means that you can more safely make your secrets available to the workflows triggered by the pull request, because only workflows defined in the commit on the base repository are run. For example, this event allows you to create workflows that label and comment on pull requests, based on the contents of the event payload."
                | optional,
              push
                | js2n.Nullable
                  {
                    branches | Array String | optional,
                    branches-ignore | Array String | optional,
                    paths | Array String | optional,
                    paths-ignore | Array String | optional,
                    tags | Array String | optional,
                    tags-ignore | Array String | optional,
                  }
                | refs."definitions.ref"
                | doc m%"
                  Runs your workflow when someone pushes to a repository branch, which triggers the push event.
                  Note: The webhook payload available to GitHub Actions does not include the added, removed, and modified attributes in the commit object. You can retrieve the full commit object using the REST API. For more information, see https://developer.github.com/v3/repos/commits/#get-a-single-commit.
                "%
                | optional,
              registry_package
                | js2n.Nullable
                  {
                    types
                      | Array (
                        std.contract.Sequence
                          [ std.enum.TagOrString, [| 'updated, 'published |] ]
                      )
                      | refs."definitions.types"
                      | optional,
                    ..
                  }
                | refs."definitions.eventObject"
                | doc "Runs your workflow anytime a package is published or updated. For more information, see https://help.github.com/en/github/managing-packages-with-github-packages."
                | optional,
              release
                | js2n.Nullable
                  {
                    types
                      | Array (
                        std.contract.Sequence
                          [
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
                      | refs."definitions.types"
                      | optional,
                    ..
                  }
                | refs."definitions.eventObject"
                | doc "Runs your workflow anytime the release event occurs. More than one activity type triggers this event. For information about the REST API, see https://developer.github.com/v3/repos/releases/ in the GitHub Developer documentation."
                | optional,
              repository_dispatch
                | refs."definitions.eventObject"
                | doc m%"
                  You can use the GitHub API to trigger a webhook event called repository_dispatch when you want to trigger a workflow for activity that happens outside of GitHub. For more information, see https://developer.github.com/v3/repos/#create-a-repository-dispatch-event.
                  To trigger the custom repository_dispatch webhook event, you must send a POST request to a GitHub API endpoint and provide an event_type name to describe the activity type. To trigger a workflow run, you must also configure your workflow to use the repository_dispatch event.
                "%
                | optional,
              schedule
                | js2n.array.MinItems 1
                | Array { cron | String | optional, }
                | doc m%"
                  You can schedule a workflow to run at specific UTC times using POSIX cron syntax (https://pubs.opengroup.org/onlinepubs/9699919799/utilities/crontab.html#tag_20_25_07). Scheduled workflows run on the latest commit on the default or base branch. The shortest interval you can run scheduled workflows is once every 5 minutes.
                  Note: GitHub Actions does not support the non-standard syntax @yearly, @monthly, @weekly, @daily, @hourly, and @reboot.
                  You can use crontab guru (https://crontab.guru/). to help generate your cron syntax and confirm what time it will run. To help you get started, there is also a list of crontab guru examples (https://crontab.guru/examples.html).
                "%
                | optional,
              status
                | refs."definitions.eventObject"
                | doc "Runs your workflow anytime the status of a Git commit changes, which triggers the status event. For information about the REST API, see https://developer.github.com/v3/repos/statuses/."
                | optional,
              watch
                | refs."definitions.eventObject"
                | doc "Runs your workflow anytime the watch event occurs. More than one activity type triggers this event. For information about the REST API, see https://developer.github.com/v3/activity/starring/."
                | optional,
              workflow_call
                | std.contract.any_of
                  [
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
                              | doc "The default value is used when an input parameter isn't specified in a workflow file."
                              | optional,
                            deprecationMessage
                              | String
                              | doc "A string shown to users using the deprecated input."
                              | optional,
                            description
                              | String
                              | doc "A string description of the input parameter."
                              | optional,
                            required
                              | Bool
                              | doc "A boolean to indicate whether the action requires the input parameter. Set to true when the parameter is required."
                              | optional,
                            type
                              | std.enum.TagOrString
                              | [| 'string, 'number, 'boolean |]
                              | doc "Required if input is defined for the on.workflow_call keyword. The value of this parameter is a string specifying the data type of the input. This must be one of: boolean, number, or string.",
                          }
                        }
                        | js2n.record.FieldsMatch "^[_a-zA-Z][a-zA-Z0-9_-]*$"
                        | doc "When using the workflow_call keyword, you can optionally specify inputs that are passed to the called workflow from the caller workflow."
                        | optional,
                      secrets
                        | std.contract.any_of
                          [
                            js2n.Null,
                            Bool,
                            Number,
                            String,
                            std.contract.Sequence
                              [
                                {
                                  _ | std.contract.any_of
                                    [
                                      js2n.Null,
                                      Bool,
                                      Number,
                                      String,
                                      {
                                        description
                                          | String
                                          | doc "A string description of the secret parameter."
                                          | optional,
                                        required
                                          | Bool
                                          | doc "A boolean specifying whether the secret must be supplied.",
                                      },
                                      Array Dyn
                                    ]
                                },
                                js2n.record.FieldsMatch
                                  "^[_a-zA-Z][a-zA-Z0-9_-]*$"
                              ],
                            Array Dyn
                          ]
                        | doc "A map of the secrets that can be used in the called workflow. Within the called workflow, you can use the secrets context to refer to a secret."
                        | optional,
                      ..
                    },
                    Array Dyn
                  ]
                | doc "Allows workflows to be reused by other workflows."
                | optional,
              workflow_dispatch
                | std.contract.any_of
                  [
                    js2n.Null,
                    Bool,
                    Number,
                    String,
                    {
                      inputs
                        | {
                          _ | std.contract.Sequence
                            [
                              refs."definitions.workflowDispatchInput",
                              { _ | Dyn }
                            ]
                        }
                        | js2n.record.FieldsMatch "^[_a-zA-Z][a-zA-Z0-9_-]*$"
                        | doc "Input parameters allow you to specify data that the action expects to use during runtime. GitHub stores input parameters as environment variables. Input ids with uppercase letters are converted to lowercase during runtime. We recommended using lowercase input ids."
                        | optional,
                    },
                    Array Dyn
                  ]
                | doc "You can now create workflows that are manually triggered with the new workflow_dispatch event. You will then see a 'Run workflow' button on the Actions tab, enabling you to easily trigger a run."
                | optional,
              workflow_run
                | js2n.Nullable
                  {
                    branches | js2n.Always | optional,
                    branches-ignore | js2n.Always | optional,
                    types
                      | Array (
                        std.contract.Sequence
                          [
                            std.enum.TagOrString,
                            [| 'in_progress, 'completed, 'requested |]
                          ]
                      )
                      | refs."definitions.types"
                      | optional,
                    workflows | js2n.array.MinItems 1 | Array String | optional,
                    ..
                  }
                | refs."definitions.eventObject"
                | doc "This event occurs when a workflow run is requested or completed, and allows you to execute a workflow based on the finished result of another workflow. For example, if your pull_request workflow generates build artifacts, you can create a new workflow that uses workflow_run to analyze the results and add a comment to the original pull request."
                | optional,
            }
          ]
        | doc "The name of the GitHub event that triggers the workflow. You can provide a single event string, array of events, array of event types, or an event configuration map that schedules a workflow or restricts the execution of a workflow to specific files, tags, or branch changes. For a list of available events, see https://help.github.com/en/github/automating-your-workflow-with-github-actions/events-that-trigger-workflows.",
      permissions | refs."definitions.permissions" | optional,
      run-name
        | String
        | doc "The name for workflow runs generated from the workflow. GitHub displays the workflow run name in the list of workflow runs on your repository's 'Actions' tab."
        | optional,
    }