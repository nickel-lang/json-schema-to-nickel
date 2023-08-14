# DO NOT EDIT
# This file was automatically generated using json-schema-to-nickel
let predicates = {
  allOf
    : Array (Dyn -> { success : Bool, error : String }) -> Dyn -> {
      success : Bool,
      error : String
    }
    | doc m%"
      `allOf preds` succeeds if all of the predicates in `preds` succeed
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.7.1]
      "%
    = fun preds x =>
      std.array.fold_right
        (
          fun pred acc =>
            let result = pred x in
            if !result.success then result else acc
        )
        { error = "", success = true, }
        preds,
  always : Dyn -> { success : Bool, error : String }
    = std.function.const { error = "", success = true, },
  anyOf
    : Array (Dyn -> { success : Bool, error : String }) -> Dyn -> {
      success : Bool,
      error : String
    }
    | doc m%"
      `anyOf preds` succeeds if at least one of the predicates in `preds` succeeds
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.7.2]
      "%
    = fun preds x =>
      (
        fun result =>
          if result.success then
            { error = "", success = true, }
          else
            {
              error = m%"
                  anyOf: none of the options matched%{std.string.join "\n"
                  result.errors}
                  "%,
              success = false,
            }
      )
        (
          std.array.fold_right
            (
              fun pred acc =>
                let result = pred x in
                if result.success || acc.success then
                  { errors = [], success = true, }
                else
                  {
                    errors =
                      [
                        m%"
                      - %{result.error}
                      "%
                      ]
                      @ acc.errors,
                    success = false,
                  }
            )
            { errors = [], success = false, }
            preds
        ),
  arrays = {
    additionalItems
      : (Dyn -> { success : Bool, error : String }) -> Number -> Dyn -> {
        success : Bool,
        error : String
      }
      | doc m%"
            Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.4.2]
            "%
      = fun pred start x =>
        if !(std.is_array x) then
          { error = "", success = true, }
        else
          (
            let x | Array Dyn = x in
            let value_length = std.array.length x in
            if start >= value_length then
              { error = "", success = true, }
            else
              (arrayOf pred ((std.array.slice start value_length x) | Dyn))
          ),
    arrayOf
      : (Dyn -> { success : Bool, error : String }) -> Dyn -> {
        success : Bool,
        error : String
      }
      | doc m%"
            Apply a predicate to all elements of an array, succeeding if all
            applications succeed. If the value isn't an array, fail.
            "%
      = fun pred x =>
        if !(std.is_array x) then
          { error = "", success = true, }
        else
          (
            let x | Array Dyn = x in
            std.array.fold_right
              (
                fun x acc =>
                  let result = pred x in
                  if !result.success then result else acc
              )
              { error = "", success = true, }
              x
          ),
    contains
      : (Dyn -> { success : Bool, error : String }) -> Dyn -> {
        success : Bool,
        error : String
      }
      | doc m%"
            Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.4.6]
            "%
      = fun pred x =>
        if !(std.is_array x) then
          { error = "", success = true, }
        else
          (
            let x | Array Dyn = x in
            (
              fun result =>
                if result.success then
                  { error = "", success = true, }
                else
                  {
                    error = m%"
                        contains: no elements matched%{std.string.join "\n"
                        result.errors}
                        "%,
                    success = false,
                  }
            )
              (
                std.array.fold_right
                  (
                    fun x acc =>
                      let result = pred x in
                      if result.success || acc.success then
                        { errors = [], success = true, }
                      else
                        {
                          errors =
                            [
                              m%"
                            - %{result.error}
                            "%
                            ]
                            @ acc.errors,
                          success = false,
                        }
                  )
                  { errors = [], success = false, }
                  x
              )
          ),
    items
      : Array (Dyn -> { success : Bool, error : String }) -> Dyn -> {
        success : Bool,
        error : String
      }
      | doc m%"
            Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.4.1]
            "%
      = fun preds x =>
        if !(std.is_array x) then
          { error = "", success = true, }
        else
          (
            let x | Array Dyn = x in
            let length_to_check =
              std.number.min (std.array.length preds) (std.array.length x)
            in

            std.array.fold_right
              (
                fun i acc =>
                  let result =
                    std.array.at i preds (std.array.at i x)
                  in

                  if !result.success then result else acc
              )
              { error = "", success = true, }
              (std.array.range 0 length_to_check)
          ),
    maxItems
      : Number -> Dyn -> { success : Bool, error : String }
      | doc m%"
            `maxItems n x` fails if `x` is an array of length strictly greater than `n` and succeeds otherwise.
            Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.4.3]
            "%
      = fun n x =>
        if !(std.is_array x) then
          { error = "", success = true, }
        else
          (
            if (std.array.length (x | Array Dyn)) > n then
              {
                error = m%"
                        array is longer than %{std.string.from_number n}
                        "%,
                success = false,
              }
            else
              { error = "", success = true, }
          ),
    minItems
      : Number -> Dyn -> { success : Bool, error : String }
      | doc m%"
            `minItems n x` fails if `x` is an array of length strictly smaller than `n` and succeeds otherwise.
            Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.4.4]
            "%
      = fun n x =>
        if !(std.is_array x) then
          { error = "", success = true, }
        else
          (
            if (std.array.length (x | Array Dyn)) < n then
              {
                error = m%"
                        array is longer than %{std.string.from_number n}
                        "%,
                success = false,
              }
            else
              { error = "", success = true, }
          ),
    uniqueItems
      : Dyn -> { success : Bool, error : String }
      | doc m%"
            Succeeds for any array if its elements are pairwise distinct.
            Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.4.5]
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
              (
                fun elt acc =>
                  if acc.has_duplicate then
                    acc
                  else
                    (
                      let index = std.serialize 'Json elt in
                      if std.record.has_field index acc.seen then
                        { duplicate = elt, has_duplicate = true, seen = acc.seen, }
                      else
                        {
                          duplicate = null,
                          has_duplicate = false,
                          seen = std.record.insert index true acc.seen,
                        }
                    )
              )
              { duplicate = null, has_duplicate = false, seen = {}, }
          in

        fun x =>
          if !(std.is_array x) then
            { error = "", success = true, }
          else
            (
              let { has_duplicate | Dyn, duplicate | Dyn, .. } =
                find_duplicate (x | Array Dyn)
              in

              if has_duplicate then
                {
                  error = m%"
                        duplicate found: %{std.serialize 'Json duplicate}
                        "%,
                  success = false,
                }
              else
                { error = "", success = true, }
            ),
  },
  const
    : Dyn -> Dyn -> { success : Bool, error : String }
    | doc m%"
      `const value x` succeeds if and only if `x` is equalt to `value`
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.1.3]
      "%
    = fun value x =>
      if x == value then
        { error = "", success = true, }
      else
        {
          error = m%"
                expected %{std.serialize 'Json value}
                "%,
          success = false,
        },
  contract_from_predicate
    : (Dyn -> { success : Bool, error : String }) -> Dyn -> Dyn -> Dyn
    = fun predicate label value =>
      let { success | Dyn, error | Dyn } =
        predicate value
      in

      if success then
        value
      else
        ((std.contract.blame_with_message error label) | Dyn),
  enum
    : Array Dyn -> Dyn -> { success : Bool, error : String }
    | doc m%"
      `enum values x` succeeds if and only if `x` is equal to one of the elements of `values`.
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.1.2]
      "%
    =
      let checkEqual = fun input variant =>
        (input == variant)
        || (
          (std.is_enum input)
          && (((std.string.from_enum input) == variant) | Bool)
        )
      in

      fun values x =>
        std.array.fold_right
          (
            fun value acc =>
              if checkEqual x value then
                { error = "", success = true, }
              else
                acc
          )
          {
            error = m%"
                expected one of %{std.serialize 'Json (values | Dyn)}
                "%,
            success = false,
          }
          values,
  from_simple_predicate
    : String -> (Dyn -> Bool) -> Dyn -> { success : Bool, error : String }
    | doc m%"
      Convert a simple boolean predicate into a predicate supporting error messages
      "%
    = fun error' pred x => { error = error', success = pred x, },
  ifThenElse
    : (Dyn -> { success : Bool, error : String }) -> (
      Dyn -> {
        success : Bool,
        error : String
      }
    ) -> (Dyn -> { success : Bool, error : String }) -> Dyn -> {
      success : Bool,
      error : String
    }
    | doc m%"
      `ifThenElse i t e` first checks if the predicate `i` succeeds. If it does, it's equivalent to `t`, otherwise it's equivalent to `e`.
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.6]
      "%
    = fun i t e x =>
      let { success | Dyn, .. } = i x in
      if success then t x else (e x),
  isType
    : [| '"Array", '"Bool", 'Integer, 'Null, '"Number", 'Record, '"String" |] -> Dyn -> { success : Bool, error : String }
    = fun t =>
      (
        match {
          'Integer =>
            from_simple_predicate
              "expected an integer"
              (
                fun x =>
                  (std.is_number x)
                  && (std.number.is_integer (x | Number))
              ),
          'Null =>
            from_simple_predicate
              "expected `null`"
              (fun x => x == null),
          _ =>
            from_simple_predicate
              m%"
            value is not of type %{std.string.from_enum t}
            "%
              (fun x => (std.typeof x) == t)
        }
      )
        t,
  never : Dyn -> { success : Bool, error : String }
    = std.function.const { error = "never", success = false, },
  not
    : (Dyn -> { success : Bool, error : String }) -> Dyn -> {
      success : Bool,
      error : String
    }
    | doc m%"
      `not pred` succeeds if and only if `pred` fails
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.7.4]
      "%
    = fun pred x =>
      let result = pred x in
      if result.success then
        {
          error = "Inverted predicate succeeded unexpectedly",
          success = false,
        }
      else
        { error = "", success = true, },
  numbers = {
    exclusiveMaximum
      : Number -> Dyn -> { success : Bool, error : String }
      | doc m%"
            Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.2.3]
            "%
      = fun limit x =>
        if !(std.is_number x) then
          { error = "", success = true, }
        else
          (
            if (x | Number) < limit then
              { error = "", success = true, }
            else
              {
                error = m%"
                      expected an exclusive maximum of %{std.string.from_number
                      limit}
                      "%,
                success = false,
              }
          ),
    exclusiveMinimum
      : Number -> Dyn -> { success : Bool, error : String }
      | doc m%"
            Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.2.5]
            "%
      = fun limit x =>
        if !(std.is_number x) then
          { error = "", success = true, }
        else
          (
            if (x | Number) > limit then
              { error = "", success = true, }
            else
              {
                error = m%"
                      expected an exclusive minimum of %{std.string.from_number
                      limit}
                      "%,
                success = false,
              }
          ),
    maximum
      : Number -> Dyn -> { success : Bool, error : String }
      | doc m%"
            Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.2.2]
            "%
      = fun limit x =>
        if !(std.is_number x) then
          { error = "", success = true, }
        else
          (
            if (x | Number) <= limit then
              { error = "", success = true, }
            else
              {
                error = m%"
                      expected a maximum of %{std.string.from_number limit}
                      "%,
                success = false,
              }
          ),
    minimum
      : Number -> Dyn -> { success : Bool, error : String }
      | doc m%"
            Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.2.4]
            "%
      = fun limit x =>
        if !(std.is_number x) then
          { error = "", success = true, }
        else
          (
            if (x | Number) >= limit then
              { error = "", success = true, }
            else
              {
                error = m%"
                      expected a minimum of %{std.string.from_number limit}
                      "%,
                success = false,
              }
          ),
    multipleOf
      : Number -> Dyn -> { success : Bool, error : String }
      | doc m%"
            Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.2.1]
            "%
      = fun mult x =>
        if !(std.is_number x) then
          { error = "", success = true, }
        else
          (
            if std.number.is_integer ((x | Number) / mult) then
              { error = "", success = true, }
            else
              {
                error = m%"
                      expected a multiple of %{std.string.from_number mult}
                      "%,
                success = false,
              }
          ),
  },
  oneOf
    : Array (Dyn -> { success : Bool, error : String }) -> Dyn -> {
      success : Bool,
      error : String
    }
    | doc m%"
      `oneOf preds` succeeds if precisely one of the predicates in `preds` succeeds
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.7.3]
      "%
    = fun preds x =>
      let count_true
        : Array { success : Bool, error : String } -> Number = fun results =>
          std.array.fold_left
            (fun n b => if b.success then n + 1 else n)
            0
            results
        in

      let results = std.array.map (fun pred => pred x) preds in
      let count = count_true results in
      if count == 0 then
        let errors =
          std.array.map
            (
              fun result =>
                m%"
                - %{result.error}
                "%
            )
            results
        in

        {
          error = m%"
                  oneOf: none of the options matched%{std.string.join "\n"
                  errors}
                  "%,
          success = false,
        }
      else
        (
          if count > 1 then
            {
              error = "oneOf: more than one of the options matched",
              success = false,
            }
          else
            { error = "", success = true, }
        ),
  records = {
    dependencies
      : { _ : Dyn } -> Dyn -> { success : Bool, error : String }
      | doc m%"
            Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.5.7]
            "%
      =
        let mustHaveFields
          : Array String -> { _ : Dyn } -> {
            success : Bool,
            error : String
          }
          = fun fields x =>
            std.array.fold_right
              (
                fun field acc =>
                  if !(std.record.has_field field x) then
                    {
                      error = m%"
                            expected field `%{field}`
                            "%,
                      success = false,
                    }
                  else
                    acc
              )
              { error = "", success = true, }
              fields
          in

        fun deps x =>
          if !(std.is_record x) then
            { error = "", success = true, }
          else
            (
              let x | { _ : Dyn } = x in
              std.array.fold_right
                (
                  fun { field | Dyn, value | Dyn } =>
                    fun acc =>
                      if !(std.record.has_field field x) then
                        acc
                      else
                        (
                          let result =
                            if std.is_array value then
                              mustHaveFields (value | Array String) x
                            else
                              (
                                let pred | Dyn -> { success : Bool, error : String } =
                                    value
                                  in

                                pred (x | Dyn)
                              )
                          in

                          if !result.success then
                            {
                              error = m%"
                            dependency of `%{field}` failed: %{result.error}
                            "%,
                              success = false,
                            }
                          else
                            acc
                        )
                )
                { error = "", success = true, }
                (std.record.to_array deps)
            ),
    maxProperties
      : Number -> Dyn -> { success : Bool, error : String }
      | doc m%"
            `maxProperties n x` fails if `x` is a record containing stricly more than `n` fields.
            Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.5.1]
            "%
      = fun n x =>
        if !(std.is_record x) then
          { error = "", success = true, }
        else
          (
            if (std.record.length (x | { _ : Dyn })) > n then
              {
                error = m%"
                        record contains more than %{std.string.from_number
                        n} fields
                        "%,
                success = false,
              }
            else
              { error = "", success = true, }
          ),
    minProperties
      : Number -> Dyn -> { success : Bool, error : String }
      | doc m%"
            `minProperties n x` fails if `x` is a record containing stricly less than `n` fields.
            Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.5.1]
            "%
      = fun n x =>
        if !(std.is_record x) then
          { error = "", success = true, }
        else
          (
            if (std.record.length (x | { _ : Dyn })) < n then
              {
                error = m%"
                        record contains fewer than %{std.string.from_number
                        n} fields
                        "%,
                success = false,
              }
            else
              { error = "", success = true, }
          ),
    propertyNames
      : (Dyn -> { success : Bool, error : String }) -> Dyn -> {
        success : Bool,
        error : String
      }
      | doc m%"
            Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.5.8]
            "%
      = fun pred x =>
        if !(std.is_record x) then
          { error = "", success = true, }
        else
          (
            std.array.fold_right
              (
                fun field acc =>
                  let result = pred (field | Dyn) in
                  if !result.success then
                    {
                      error = m%"
                          field `%{field}` did not validate against `propertyNames` schema
                          "%,
                      success = false,
                    }
                  else
                    acc
              )
              { error = "", success = true, }
              (std.record.fields (x | { _ : Dyn }))
          ),
    record
      : { _ : Dyn -> { success : Bool, error : String } } -> {
        _ : Dyn -> {
          success : Bool,
          error : String
        }
      } -> Bool -> (Dyn -> { success : Bool, error : String }) -> Dyn -> {
        success : Bool,
        error : String
      }
      | doc m%"
            `record properties patternProperties additionalAllowed
            additionalProperties x` is a combination of the `properties`,
            `patternProperties` and `additionalProperties` validation keywords in
            JSON schema.

            Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.5]
            "%
      = fun properties patternProperties additionalAllowed additionalProperties x =>
        if !(std.is_record x) then
          { error = "", success = true, }
        else
          (
            let x | { _ : Dyn } = x in
            let check_properties
              : { success : Bool, error : String, checked : { _ : Bool } } =
                std.array.fold_right
                  (
                    fun { field | Dyn, value | Dyn } =>
                      fun acc =>
                        if !(std.record.has_field field x) then
                          acc
                        else
                          (
                            let result = value x."%{field}" in
                            if !result.success then
                              {
                                checked : { _ : Bool }
                                  = {},
                                error = m%"
                              field `%{field}` didn't validate: %{result.error}
                              "%,
                                success = false,
                              }
                            else
                              {
                                checked = std.record.insert field true acc.checked,
                                error = acc.error,
                                success = acc.success,
                              }
                          )
                  )
                  { checked = {}, error = "", success = true, }
                  (std.record.to_array properties)
              in

            let matching_fields : String -> { _ : Dyn } = fun pattern =>
                let matcher = std.string.is_match pattern in
                std.array.fold_left
                  (
                    fun acc =>
                      fun { field | Dyn, value | Dyn } =>
                        if matcher
                          field then
                          std.record.insert field value acc
                        else
                          acc
                  )
                  {}
                  (std.record.to_array x)
              in

            let check_pattern_properties
              : { success : Bool, error : String, checked : { _ : Bool } } =
                std.array.fold_right
                  (
                    fun { field | Dyn = pattern, value | Dyn = pred } =>
                      fun acc =>
                        let result =
                          std.array.fold_right
                            (
                              fun { field | Dyn, value | Dyn } =>
                                fun acc =>
                                  let result = pred value in
                                  if !result.success then
                                    {
                                      checked : { _ : Bool }
                                        = {},
                                      error = m%"
                                    field `%{field}` didn't validate: %{result.error}
                                    "%,
                                      success = false,
                                    }
                                  else
                                    {
                                      checked =
                                        std.record.insert
                                          field
                                          true
                                          acc.checked,
                                      error = acc.error,
                                      success = acc.success,
                                    }
                            )
                            {
                              checked : { _ : Bool }
                                = {},
                              error = "",
                              success = true,
                            }
                            (std.record.to_array (matching_fields pattern))
                        in

                        if !result.success then
                          result
                        else
                          {
                            checked =
                              std.array.fold_left
                                (
                                  fun r field =>
                                    if !(
                                      std.record.has_field
                                        field
                                        r
                                    ) then
                                      std.record.insert field true r
                                    else
                                      r
                                )
                                acc.checked
                                (std.record.fields result.checked),
                            error = acc.error,
                            success = acc.success,
                          }
                  )
                  { checked : { _ : Bool } = {}, error = "", success = true, }
                  (std.record.to_array patternProperties)
              in

            let remaining_fields =
              std.array.fold_left
                (
                  fun acc field =>
                    if !(std.record.has_field field acc) then
                      acc
                    else
                      (std.record.remove field acc)
                )
                x
                (
                  (std.record.fields check_properties.checked)
                  @ (std.record.fields check_pattern_properties.checked)
                )
            in

            let check_additional_properties
              : { success : Bool, error : String } =
                if (!additionalAllowed)
                && (!(std.record.is_empty remaining_fields)) then
                  {
                    error = m%"
                          extra fields %{std.serialize 'Json
                          ((std.record.fields remaining_fields) | Dyn)}
                          "%,
                    success = false,
                  }
                else
                  (
                    std.array.fold_right
                      (
                        fun { field | Dyn, value | Dyn } =>
                          fun acc =>
                            let result =
                              additionalProperties value
                            in

                            if !result.success then
                              {
                                error = m%"
                              field `%{field}` didn't validate: %{result.error}
                              "%,
                                success = false,
                              }
                            else
                              acc
                      )
                      { error = "", success = true, }
                      (std.record.to_array remaining_fields)
                  )
              in

            if !check_properties.success then
              { error = check_properties.error, success = false, }
            else
              (
                if !check_pattern_properties.success then
                  { error = check_pattern_properties.error, success = false, }
                else
                  check_additional_properties
              )
          ),
    required
      : Array String -> Dyn -> { success : Bool, error : String }
      | doc m%"
            Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.5.3]
            "%
      = fun fields x =>
        if !(std.is_record x) then
          { error = "", success = true, }
        else
          (
            std.array.fold_right
              (
                fun field acc =>
                  if !(
                    std.record.has_field
                      field
                      (x | { _ : Dyn })
                  ) then
                    {
                      error = m%"
                          missing required field %{field}
                          "%,
                      success = false,
                    }
                  else
                    acc
              )
              { error = "", success = true, }
              fields
          ),
  },
  strings = {
    maxLength
      : Number -> Dyn -> { success : Bool, error : String }
      | doc m%"
            Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.3.1]
            "%
      = fun limit x =>
        if !(std.is_string x) then
          { error = "", success = true, }
        else
          (
            if (std.string.length (x | String)) <= limit then
              { error = "", success = true, }
            else
              {
                error = m%"
                      expected a string of length no larger than %{std.string.from_number
                      limit}
                      "%,
                success = false,
              }
          ),
    minLength
      : Number -> Dyn -> { success : Bool, error : String }
      | doc m%"
            Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.3.2]
            "%
      = fun limit x =>
        if !(std.is_string x) then
          { error = "", success = true, }
        else
          (
            if (std.string.length (x | String)) >= limit then
              { error = "", success = true, }
            else
              {
                error = m%"
                      expected a string of length no smaller than %{std.string.from_number
                      limit}
                      "%,
                success = false,
              }
          ),
    pattern
      : String -> Dyn -> { success : Bool, error : String }
      | doc m%"
            Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.3.3]
            "%
      = fun pattern x =>
        if !(std.is_string x) then
          { error = "", success = true, }
        else
          (
            if std.string.is_match pattern (x | String) then
              { error = "", success = true, }
            else
              {
                error = m%"
                      expected a string matching the pattern `%{pattern}`
                      "%,
                success = false,
              }
          ),
  },
}
in

let rec definitions = {
  contract = {
    architecture =
      std.contract.Sequence
        [std.enum.TagOrString, [| 'x86, 'x64, 'ARM32 |]],
    branch
      | doc m%"
            When using the push and pull_request events, you can configure a workflow to run on specific branches or tags. If you only define only tags or only branches, the workflow won't run for events affecting the undefined Git ref.
            The branches, branches-ignore, tags, and tags-ignore keywords accept glob patterns that use the * and ** wildcard characters to match more than one branch or tag name. For more information, see https://help.github.com/en/github/automating-your-workflow-with-github-actions/workflow-syntax-for-github-actions#filter-pattern-cheat-sheet.
            The patterns defined in branches and tags are evaluated against the Git ref's name. For example, defining the pattern mona/octocat in branches will match the refs/heads/mona/octocat Git ref. The pattern releases/** will match the refs/heads/releases/10 Git ref.
            You can use two types of filters to prevent a workflow from running on pushes and pull requests to tags and branches:
            - branches or branches-ignore - You cannot use both the branches and branches-ignore filters for the same event in a workflow. Use the branches filter when you need to filter branches for positive matches and exclude branches. Use the branches-ignore filter when you only need to exclude branch names.
            - tags or tags-ignore - You cannot use both the tags and tags-ignore filters for the same event in a workflow. Use the tags filter when you need to filter tags for positive matches and exclude tags. Use the tags-ignore filter when you only need to exclude tag names.
            You can exclude tags and branches using the ! character. The order that you define patterns matters.
            - A matching negative pattern (prefixed with !) after a positive match will exclude the Git ref.
            - A matching positive pattern after a negative match will include the Git ref again.
            "%
      = definitions.contract.globs,
    concurrency = {
      cancel-in-progress
        | predicates.contract_from_predicate
          (
            predicates.oneOf
              [
                predicates.isType '"Bool",
                definitions.predicate.expressionSyntax
              ]
          )
        | doc m%"
                  To cancel any currently running job or workflow in the same concurrency group, specify cancel-in-progress: true.
                  "%
        | optional,
      group
        | String
        | doc m%"
                  When a concurrent job or workflow is queued, if another job or workflow using the same concurrency group in the repository is in progress, the queued job or workflow will be pending. Any previously pending job or workflow in the concurrency group will be canceled.
                  "%,
    },
    configuration =
      predicates.contract_from_predicate
        definitions.predicate.configuration,
    container = {
      credentials
        | {
          password | String | optional,
          username | String | optional,
          ..
        }
        | doc m%"
                  If the image's container registry requires authentication to pull the image, you can use credentials to set a map of the username and password. The credentials are the same values that you would provide to the `docker login` command.
                  "%
        | optional,
      env
        | definitions.contract.env
        | doc m%"
                  Sets an array of environment variables in the container.
                  "%
        | optional,
      image
        | String
        | doc m%"
                  The Docker image to use as the container to run the action. The value can be the Docker Hub image name or a registry name.
                  "%,
      options
        | String
        | doc m%"
                  Additional Docker container resource options. For a list of options, see https://docs.docker.com/engine/reference/commandline/create/#options.
                  "%
        | optional,
      ports
        | predicates.contract_from_predicate
          (
            predicates.allOf
              [
                predicates.isType '"Array",
                predicates.arrays.arrayOf
                  (
                    predicates.oneOf
                      [
                        predicates.isType '"Number",
                        predicates.isType '"String"
                      ]
                  ),
                predicates.arrays.minItems 1
              ]
          )
        | doc m%"
                  Sets an array of ports to expose on the container.
                  "%
        | optional,
      volumes
        | predicates.contract_from_predicate
          (
            predicates.allOf
              [
                predicates.isType '"Array",
                predicates.arrays.arrayOf
                  (
                    predicates.allOf
                      [
                        predicates.isType '"String",
                        predicates.strings.pattern "^[^:]+:[^:]+$"
                      ]
                  ),
                predicates.arrays.minItems 1
              ]
          )
        | doc m%"
                  Sets an array of volumes for the container to use. You can use volumes to share data between services or other steps in a job. You can specify named Docker volumes, anonymous Docker volumes, or bind mounts on the host.
                  To specify a volume, you specify the source and destination path: <source>:<destinationPath>
                  The <source> is a volume name or an absolute path on the host machine, and <destinationPath> is an absolute path in the container.
                  "%
        | optional,
    },
    defaults =
      predicates.contract_from_predicate
        definitions.predicate.defaults,
    env
      | doc m%"
            To set custom environment variables, you need to specify the variables in the workflow file. You can define environment variables for a step, job, or entire workflow using the jobs.<job_id>.steps[*].env, jobs.<job_id>.env, and env keywords. For more information, see https://docs.github.com/en/actions/learn-github-actions/workflow-syntax-for-github-actions#jobsjob_idstepsenv
            "%
      = predicates.contract_from_predicate definitions.predicate.env,
    environment
      | doc m%"
            The environment that the job references
            "%
      = {
        name
          | String
          | doc m%"
                  The name of the environment configured in the repo.
                  "%,
        url
          | String
          | doc m%"
                  A deployment URL
                  "%
          | optional,
      },
    event =
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
            'member,
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
    eventObject =
      predicates.contract_from_predicate
        definitions.predicate.eventObject,
    expressionSyntax =
      predicates.contract_from_predicate
        definitions.predicate.expressionSyntax,
    globs =
      predicates.contract_from_predicate
        definitions.predicate.globs,
    jobNeeds
      | doc m%"
            Identifies any jobs that must complete successfully before this job will run. It can be a string or array of strings. If a job fails, all jobs that need it are skipped unless the jobs use a conditional statement that causes the job to continue.
            "%
      = predicates.contract_from_predicate definitions.predicate.jobNeeds,
    machine =
      std.contract.Sequence
        [std.enum.TagOrString, [| 'windows, 'macos, 'linux |]],
    name = predicates.contract_from_predicate definitions.predicate.name,
    normalJob
      | doc m%"
            Each job must have an id to associate with the job. The key job_id is a string and its value is a map of the job's configuration data. You must replace <job_id> with a string that is unique to the jobs object. The <job_id> must start with a letter or _ and contain only alphanumeric characters, -, or _.
            "%
      = {
        concurrency
          | predicates.contract_from_predicate
            (
              predicates.oneOf
                [
                  predicates.isType '"String",
                  definitions.predicate.concurrency
                ]
            )
          | doc m%"
                  Concurrency ensures that only a single job or workflow using the same concurrency group will run at a time. A concurrency group can be any string or expression. The expression can use any context except for the secrets context.
                  You can also specify concurrency at the workflow level.
                  When a concurrent job or workflow is queued, if another job or workflow using the same concurrency group in the repository is in progress, the queued job or workflow will be pending. Any previously pending job or workflow in the concurrency group will be canceled. To also cancel any currently running job or workflow in the same concurrency group, specify cancel-in-progress: true.
                  "%
          | optional,
        container
          | predicates.contract_from_predicate
            (
              predicates.oneOf
                [
                  predicates.isType '"String",
                  definitions.predicate.container
                ]
            )
          | doc m%"
                  A container to run any steps in a job that don't already specify a container. If you have steps that use both script and container actions, the container actions will run as sibling containers on the same network with the same volume mounts.
                  If you do not set a container, all steps will run directly on the host specified by runs-on unless a step refers to an action configured to run in a container.
                  "%
          | optional,
        continue-on-error
          | predicates.contract_from_predicate
            (
              predicates.oneOf
                [
                  predicates.isType '"Bool",
                  definitions.predicate.expressionSyntax
                ]
            )
          | doc m%"
                  Prevents a workflow run from failing when a job fails. Set to true to allow a workflow run to pass when this job fails.
                  "%
          | optional,
        defaults
          | definitions.contract.defaults
          | doc m%"
                  A map of default settings that will apply to all steps in the job.
                  "%
          | optional,
        env
          | definitions.contract.env
          | doc m%"
                  A map of environment variables that are available to all steps in the job.
                  "%
          | optional,
        environment
          | predicates.contract_from_predicate
            (
              predicates.oneOf
                [
                  predicates.isType '"String",
                  definitions.predicate.environment
                ]
            )
          | doc m%"
                  The environment that the job references.
                  "%
          | optional,
        "if"
          | predicates.contract_from_predicate
            (
              predicates.anyOf
                [
                  predicates.isType '"Bool",
                  predicates.isType '"Number",
                  predicates.isType '"String"
                ]
            )
          | doc m%"
                  You can use the if conditional to prevent a job from running unless a condition is met. You can use any supported context and expression to create a conditional.
                  Expressions in an if conditional do not require the ${{ }} syntax. For more information, see https://help.github.com/en/articles/contexts-and-expression-syntax-for-github-actions.
                  "%
          | optional,
        name
          | String
          | doc m%"
                  The name of the job displayed on GitHub.
                  "%
          | optional,
        needs | definitions.contract.jobNeeds | optional,
        outputs
          | predicates.contract_from_predicate
            (
              predicates.allOf
                [
                  predicates.isType 'Record,
                  predicates.records.minProperties 1,
                  predicates.records.record
                    {}
                    {}
                    true
                    (predicates.isType '"String")
                ]
            )
          | doc m%"
                  A map of outputs for a job. Job outputs are available to all downstream jobs that depend on this job.
                  "%
          | optional,
        permissions | definitions.contract.permissions | optional,
        runs-on
          | predicates.contract_from_predicate
            (
              predicates.oneOf
                [
                  predicates.allOf
                    [
                      predicates.isType '"String",
                      predicates.enum
                        [
                          "macos-10.15",
                          "macos-11",
                          "macos-12",
                          "macos-12-xl",
                          "macos-13",
                          "macos-13-xl",
                          "macos-latest",
                          "macos-latest-xl",
                          "self-hosted",
                          "ubuntu-18.04",
                          "ubuntu-20.04",
                          "ubuntu-22.04",
                          "ubuntu-latest",
                          "ubuntu-latest-4-cores",
                          "ubuntu-latest-8-cores",
                          "ubuntu-latest-16-cores",
                          "windows-2019",
                          "windows-2022",
                          "windows-latest",
                          "windows-latest-8-cores"
                        ]
                    ],
                  predicates.allOf
                    [
                      predicates.isType '"Array",
                      predicates.anyOf
                        [
                          predicates.allOf
                            [
                              predicates.arrays.items
                                [predicates.const "self-hosted"],
                              predicates.arrays.additionalItems
                                (predicates.isType '"String")
                                1,
                              predicates.arrays.minItems 1
                            ],
                          predicates.allOf
                            [
                              predicates.arrays.items
                                [
                                  predicates.const "self-hosted",
                                  definitions.predicate.machine
                                ],
                              predicates.arrays.additionalItems
                                (predicates.isType '"String")
                                2,
                              predicates.arrays.minItems 2
                            ],
                          predicates.allOf
                            [
                              predicates.arrays.items
                                [
                                  predicates.const "self-hosted",
                                  definitions.predicate.architecture
                                ],
                              predicates.arrays.additionalItems
                                (predicates.isType '"String")
                                2,
                              predicates.arrays.minItems 2
                            ],
                          predicates.allOf
                            [
                              predicates.arrays.items
                                [
                                  predicates.const "self-hosted",
                                  definitions.predicate.machine,
                                  definitions.predicate.architecture
                                ],
                              predicates.arrays.additionalItems
                                (predicates.isType '"String")
                                3,
                              predicates.arrays.minItems 3
                            ],
                          predicates.allOf
                            [
                              predicates.arrays.items
                                [
                                  predicates.const "self-hosted",
                                  definitions.predicate.architecture,
                                  definitions.predicate.machine
                                ],
                              predicates.arrays.additionalItems
                                (predicates.isType '"String")
                                3,
                              predicates.arrays.minItems 3
                            ],
                          predicates.allOf
                            [
                              predicates.arrays.items [predicates.const "linux"],
                              predicates.arrays.additionalItems
                                (predicates.isType '"String")
                                1,
                              predicates.arrays.maxItems 2,
                              predicates.arrays.minItems 2
                            ],
                          predicates.allOf
                            [
                              predicates.arrays.items
                                [predicates.const "windows"],
                              predicates.arrays.additionalItems
                                (predicates.isType '"String")
                                1,
                              predicates.arrays.maxItems 2,
                              predicates.arrays.minItems 2
                            ]
                        ]
                    ],
                  predicates.allOf
                    [
                      predicates.isType 'Record,
                      predicates.records.record
                        {
                          group = predicates.isType '"String",
                          labels =
                            predicates.oneOf
                              [
                                predicates.isType '"String",
                                predicates.allOf
                                  [
                                    predicates.isType '"Array",
                                    predicates.arrays.arrayOf
                                      (predicates.isType '"String")
                                  ]
                              ],
                        }
                        {}
                        true
                        predicates.always
                    ],
                  definitions.predicate.stringContainingExpressionSyntax
                ]
            )
          | doc m%"
                  The type of machine to run the job on. The machine can be either a GitHub-hosted runner, or a self-hosted runner.
                  "%,
        services
          | predicates.contract_from_predicate
            (
              predicates.allOf
                [
                  predicates.isType 'Record,
                  predicates.records.record
                    {}
                    {}
                    true
                    definitions.predicate.container
                ]
            )
          | doc m%"
                  Additional containers to host services for a job in a workflow. These are useful for creating databases or cache services like redis. The runner on the virtual machine will automatically create a network and manage the life cycle of the service containers.
                  When you use a service container for a job or your step uses container actions, you don't need to set port information to access the service. Docker automatically exposes all ports between containers on the same network.
                  When both the job and the action run in a container, you can directly reference the container by its hostname. The hostname is automatically mapped to the service name.
                  When a step does not use a container action, you must access the service using localhost and bind the ports.
                  "%
          | optional,
        steps
          | predicates.contract_from_predicate
            (
              predicates.allOf
                [
                  predicates.isType '"Array",
                  predicates.arrays.arrayOf
                    (
                      predicates.allOf
                        [
                          predicates.oneOf
                            [
                              predicates.allOf
                                [
                                  predicates.isType 'Record,
                                  predicates.records.required ["uses"],
                                  predicates.records.record
                                    { uses = predicates.isType '"String", }
                                    {}
                                    true
                                    predicates.always
                                ],
                              predicates.allOf
                                [
                                  predicates.isType 'Record,
                                  predicates.records.required ["run"],
                                  predicates.records.record
                                    { run = predicates.isType '"String", }
                                    {}
                                    true
                                    predicates.always
                                ]
                            ],
                          predicates.allOf
                            [
                              predicates.isType 'Record,
                              predicates.records.record
                                {
                                  continue-on-error =
                                    predicates.oneOf
                                      [
                                        predicates.isType '"Bool",
                                        definitions.predicate.expressionSyntax
                                      ],
                                  env = definitions.predicate.env,
                                  id = predicates.isType '"String",
                                  "if" =
                                    predicates.anyOf
                                      [
                                        predicates.isType '"Bool",
                                        predicates.isType '"Number",
                                        predicates.isType '"String"
                                      ],
                                  name = predicates.isType '"String",
                                  run = predicates.isType '"String",
                                  shell = definitions.predicate.shell,
                                  timeout-minutes =
                                    predicates.oneOf
                                      [
                                        predicates.isType '"Number",
                                        definitions.predicate.expressionSyntax
                                      ],
                                  uses = predicates.isType '"String",
                                  with =
                                    predicates.allOf
                                      [
                                        predicates.records.record
                                          {
                                            args = predicates.isType '"String",
                                            entrypoint = predicates.isType '"String",
                                          }
                                          {}
                                          true
                                          predicates.always,
                                        definitions.predicate.env
                                      ],
                                  working-directory =
                                    definitions.predicate.working-directory,
                                }
                                {}
                                false
                                predicates.never,
                              predicates.records.dependencies
                                { shell = ["run"], working-directory = ["run"], }
                            ]
                        ]
                    ),
                  predicates.arrays.minItems 1
                ]
            )
          | doc m%"
                  A job contains a sequence of tasks called steps. Steps can run commands, run setup tasks, or run an action in your repository, a public repository, or an action published in a Docker registry. Not all steps run actions, but all actions run as a step. Each step runs in its own process in the virtual environment and has access to the workspace and filesystem. Because steps run in their own process, changes to environment variables are not preserved between steps. GitHub provides built-in steps to set up and complete a job.
                  Must contain either `uses` or `run`
                  "%
          | optional,
        strategy
          | {
            fail-fast
              | Bool
              | doc m%"
                      When set to true, GitHub cancels all in-progress jobs if any matrix job fails. Default: true
                      "%
              | optional,
            matrix
              | predicates.contract_from_predicate
                (
                  predicates.allOf
                    [
                      predicates.oneOf
                        [
                          predicates.isType 'Record,
                          definitions.predicate.expressionSyntax
                        ],
                      predicates.records.minProperties 1,
                      predicates.records.record
                        {}
                        {
                          "^(in|ex)clude$" =
                            predicates.allOf
                              [
                                predicates.isType '"Array",
                                predicates.arrays.arrayOf
                                  (
                                    predicates.allOf
                                      [
                                        predicates.isType 'Record,
                                        predicates.records.record
                                          {}
                                          {}
                                          true
                                          definitions.predicate.configuration
                                      ]
                                  ),
                                predicates.arrays.minItems 1
                              ],
                        }
                        true
                        (
                          predicates.oneOf
                            [
                              predicates.allOf
                                [
                                  predicates.isType '"Array",
                                  predicates.arrays.arrayOf
                                    definitions.predicate.configuration,
                                  predicates.arrays.minItems 1
                                ],
                              definitions.predicate.expressionSyntax
                            ]
                        )
                    ]
                )
              | doc m%"
                      A build matrix is a set of different configurations of the virtual environment. For example you might run a job against more than one supported version of a language, operating system, or tool. Each configuration is a copy of the job that runs and reports a status.
                      You can specify a matrix by supplying an array for the configuration options. For example, if the GitHub virtual environment supports Node.js versions 6, 8, and 10 you could specify an array of those versions in the matrix.
                      When you define a matrix of operating systems, you must set the required runs-on keyword to the operating system of the current job, rather than hard-coding the operating system name. To access the operating system name, you can use the matrix.os context parameter to set runs-on. For more information, see https://help.github.com/en/articles/contexts-and-expression-syntax-for-github-actions.
                      "%,
            max-parallel
              | predicates.contract_from_predicate
                (
                  predicates.anyOf
                    [
                      predicates.isType '"Number",
                      predicates.isType '"String"
                    ]
                )
              | doc m%"
                      The maximum number of jobs that can run simultaneously when using a matrix job strategy. By default, GitHub will maximize the number of jobs run in parallel depending on the available runners on GitHub-hosted virtual machines.
                      "%
              | optional,
          }
          | doc m%"
                  A strategy creates a build matrix for your jobs. You can define different variations of an environment to run each job in.
                  "%
          | optional,
        timeout-minutes
          | predicates.contract_from_predicate
            (
              predicates.oneOf
                [
                  predicates.isType '"Number",
                  definitions.predicate.expressionSyntax
                ]
            )
          | doc m%"
                  The maximum number of minutes to let a workflow run before GitHub automatically cancels it. Default: 360
                  "%
          | optional,
      },
    path
      | doc m%"
            When using the push and pull_request events, you can configure a workflow to run when at least one file does not match paths-ignore or at least one modified file matches the configured paths. Path filters are not evaluated for pushes to tags.
            The paths-ignore and paths keywords accept glob patterns that use the * and ** wildcard characters to match more than one path name. For more information, see https://help.github.com/en/github/automating-your-workflow-with-github-actions/workflow-syntax-for-github-actions#filter-pattern-cheat-sheet.
            You can exclude paths using two types of filters. You cannot use both of these filters for the same event in a workflow.
            - paths-ignore - Use the paths-ignore filter when you only need to exclude path names.
            - paths - Use the paths filter when you need to filter paths for positive matches and exclude paths.
            "%
      = definitions.contract.globs,
    permissions
      | doc m%"
            You can modify the default permissions granted to the GITHUB_TOKEN, adding or removing access as required, so that you only allow the minimum required access.
            "%
      =
        predicates.contract_from_predicate
          definitions.predicate.permissions,
    permissions-event = {
      actions | definitions.contract.permissions-level | optional,
      checks | definitions.contract.permissions-level | optional,
      contents | definitions.contract.permissions-level | optional,
      deployments | definitions.contract.permissions-level | optional,
      discussions | definitions.contract.permissions-level | optional,
      id-token | definitions.contract.permissions-level | optional,
      issues | definitions.contract.permissions-level | optional,
      packages | definitions.contract.permissions-level | optional,
      pages | definitions.contract.permissions-level | optional,
      pull-requests
        | definitions.contract.permissions-level
        | optional,
      repository-projects
        | definitions.contract.permissions-level
        | optional,
      security-events
        | definitions.contract.permissions-level
        | optional,
      statuses | definitions.contract.permissions-level | optional,
    },
    permissions-level =
      std.contract.Sequence
        [std.enum.TagOrString, [| 'none, 'write, 'read |]],
    ref = predicates.contract_from_predicate definitions.predicate.ref,
    reusableWorkflowCallJob
      | doc m%"
            Each job must have an id to associate with the job. The key job_id is a string and its value is a map of the job's configuration data. You must replace <job_id> with a string that is unique to the jobs object. The <job_id> must start with a letter or _ and contain only alphanumeric characters, -, or _.
            "%
      = {
        concurrency
          | predicates.contract_from_predicate
            (
              predicates.oneOf
                [
                  predicates.isType '"String",
                  definitions.predicate.concurrency
                ]
            )
          | doc m%"
                  Concurrency ensures that only a single job or workflow using the same concurrency group will run at a time. A concurrency group can be any string or expression. The expression can use any context except for the secrets context.
                  You can also specify concurrency at the workflow level.
                  When a concurrent job or workflow is queued, if another job or workflow using the same concurrency group in the repository is in progress, the queued job or workflow will be pending. Any previously pending job or workflow in the concurrency group will be canceled. To also cancel any currently running job or workflow in the same concurrency group, specify cancel-in-progress: true.
                  "%
          | optional,
        "if"
          | predicates.contract_from_predicate
            (
              predicates.anyOf
                [
                  predicates.isType '"Bool",
                  predicates.isType '"Number",
                  predicates.isType '"String"
                ]
            )
          | doc m%"
                  You can use the if conditional to prevent a job from running unless a condition is met. You can use any supported context and expression to create a conditional.
                  Expressions in an if conditional do not require the ${{ }} syntax. For more information, see https://help.github.com/en/articles/contexts-and-expression-syntax-for-github-actions.
                  "%
          | optional,
        name
          | String
          | doc m%"
                  The name of the job displayed on GitHub.
                  "%
          | optional,
        needs | definitions.contract.jobNeeds | optional,
        permissions | definitions.contract.permissions-event | optional,
        secrets
          | predicates.contract_from_predicate
            (
              predicates.oneOf
                [
                  definitions.predicate.env,
                  predicates.allOf
                    [
                      predicates.isType '"String",
                      predicates.enum ["inherit"]
                    ]
                ]
            )
          | doc m%"
                  When a job is used to call a reusable workflow, you can use 'secrets' to provide a map of secrets that are passed to the called workflow. Any secrets that you pass must match the names defined in the called workflow.
                  "%
          | optional,
        strategy
          | {
            fail-fast
              | Bool
              | doc m%"
                      When set to true, GitHub cancels all in-progress jobs if any matrix job fails. Default: true
                      "%
              | optional,
            matrix
              | predicates.contract_from_predicate
                (
                  predicates.allOf
                    [
                      predicates.oneOf
                        [
                          predicates.isType 'Record,
                          definitions.predicate.expressionSyntax
                        ],
                      predicates.records.minProperties 1,
                      predicates.records.record
                        {}
                        {
                          "^(in|ex)clude$" =
                            predicates.allOf
                              [
                                predicates.isType '"Array",
                                predicates.arrays.arrayOf
                                  (
                                    predicates.allOf
                                      [
                                        predicates.isType 'Record,
                                        predicates.records.record
                                          {}
                                          {}
                                          true
                                          definitions.predicate.configuration
                                      ]
                                  ),
                                predicates.arrays.minItems 1
                              ],
                        }
                        true
                        (
                          predicates.oneOf
                            [
                              predicates.allOf
                                [
                                  predicates.isType '"Array",
                                  predicates.arrays.arrayOf
                                    definitions.predicate.configuration,
                                  predicates.arrays.minItems 1
                                ],
                              definitions.predicate.expressionSyntax
                            ]
                        )
                    ]
                )
              | doc m%"
                      A build matrix is a set of different configurations of the virtual environment. For example you might run a job against more than one supported version of a language, operating system, or tool. Each configuration is a copy of the job that runs and reports a status.
                      You can specify a matrix by supplying an array for the configuration options. For example, if the GitHub virtual environment supports Node.js versions 6, 8, and 10 you could specify an array of those versions in the matrix.
                      When you define a matrix of operating systems, you must set the required runs-on keyword to the operating system of the current job, rather than hard-coding the operating system name. To access the operating system name, you can use the matrix.os context parameter to set runs-on. For more information, see https://help.github.com/en/articles/contexts-and-expression-syntax-for-github-actions.
                      "%,
            max-parallel
              | predicates.contract_from_predicate
                (
                  predicates.anyOf
                    [
                      predicates.isType '"Number",
                      predicates.isType '"String"
                    ]
                )
              | doc m%"
                      The maximum number of jobs that can run simultaneously when using a matrix job strategy. By default, GitHub will maximize the number of jobs run in parallel depending on the available runners on GitHub-hosted virtual machines.
                      "%
              | optional,
          }
          | doc m%"
                  A strategy creates a build matrix for your jobs. You can define different variations of an environment to run each job in.
                  "%
          | optional,
        uses
          | predicates.contract_from_predicate
            (
              predicates.allOf
                [
                  predicates.isType '"String",
                  predicates.strings.pattern "^(.+/)+(.+)\\.(ya?ml)(@.+)?$"
                ]
            )
          | doc m%"
                  The location and version of a reusable workflow file to run as a job, of the form './{path/to}/{localfile}.yml' or '{owner}/{repo}/{path}/{filename}@{ref}'. {ref} can be a SHA, a release tag, or a branch name. Using the commit SHA is the safest for stability and security.
                  "%,
        with
          | definitions.contract.env
          | doc m%"
                  A map of inputs that are passed to the called workflow. Any inputs that you pass must match the input specifications defined in the called workflow. Unlike 'jobs.<job_id>.steps[*].with', the inputs you pass with 'jobs.<job_id>.with' are not be available as environment variables in the called workflow. Instead, you can reference the inputs by using the inputs context.
                  "%
          | optional,
      },
    shell
      | doc m%"
            You can override the default shell settings in the runner's operating system using the shell keyword. You can use built-in shell keywords, or you can define a custom set of shell options.
            "%
      = predicates.contract_from_predicate definitions.predicate.shell,
    stringContainingExpressionSyntax =
      predicates.contract_from_predicate
        definitions.predicate.stringContainingExpressionSyntax,
    types
      | doc m%"
            Selects the types of activity that will trigger a workflow run. Most GitHub events are triggered by more than one type of activity. For example, the event for the release resource is triggered when a release is published, unpublished, created, edited, deleted, or prereleased. The types keyword enables you to narrow down activity that causes the workflow to run. When only one activity type triggers a webhook event, the types keyword is unnecessary.
            You can use an array of event types. For more information about each event and their activity types, see https://help.github.com/en/articles/events-that-trigger-workflows#webhook-events.
            "%
      = predicates.contract_from_predicate definitions.predicate.types,
    working-directory
      | doc m%"
            Using the working-directory keyword, you can specify the working directory of where to run the command.
            "%
      = String,
  },
  predicate = {
    architecture =
      predicates.allOf
        [
          predicates.isType '"String",
          predicates.enum ["ARM32", "x64", "x86"]
        ],
    branch
      | doc m%"
            When using the push and pull_request events, you can configure a workflow to run on specific branches or tags. If you only define only tags or only branches, the workflow won't run for events affecting the undefined Git ref.
            The branches, branches-ignore, tags, and tags-ignore keywords accept glob patterns that use the * and ** wildcard characters to match more than one branch or tag name. For more information, see https://help.github.com/en/github/automating-your-workflow-with-github-actions/workflow-syntax-for-github-actions#filter-pattern-cheat-sheet.
            The patterns defined in branches and tags are evaluated against the Git ref's name. For example, defining the pattern mona/octocat in branches will match the refs/heads/mona/octocat Git ref. The pattern releases/** will match the refs/heads/releases/10 Git ref.
            You can use two types of filters to prevent a workflow from running on pushes and pull requests to tags and branches:
            - branches or branches-ignore - You cannot use both the branches and branches-ignore filters for the same event in a workflow. Use the branches filter when you need to filter branches for positive matches and exclude branches. Use the branches-ignore filter when you only need to exclude branch names.
            - tags or tags-ignore - You cannot use both the tags and tags-ignore filters for the same event in a workflow. Use the tags filter when you need to filter tags for positive matches and exclude tags. Use the tags-ignore filter when you only need to exclude tag names.
            You can exclude tags and branches using the ! character. The order that you define patterns matters.
            - A matching negative pattern (prefixed with !) after a positive match will exclude the Git ref.
            - A matching positive pattern after a negative match will include the Git ref again.
            "%
      = definitions.predicate.globs,
    concurrency =
      predicates.allOf
        [
          predicates.isType 'Record,
          predicates.records.required ["group"],
          predicates.records.record
            {
              cancel-in-progress =
                predicates.oneOf
                  [
                    predicates.isType '"Bool",
                    definitions.predicate.expressionSyntax
                  ],
              group = predicates.isType '"String",
            }
            {}
            false
            predicates.never
        ],
    configuration =
      predicates.oneOf
        [
          predicates.isType '"String",
          predicates.isType '"Number",
          predicates.isType '"Bool",
          predicates.allOf
            [
              predicates.isType 'Record,
              predicates.records.record
                {}
                {}
                true
                definitions.predicate.configuration
            ],
          predicates.allOf
            [
              predicates.isType '"Array",
              predicates.arrays.arrayOf definitions.predicate.configuration
            ]
        ],
    container =
      predicates.allOf
        [
          predicates.isType 'Record,
          predicates.records.required ["image"],
          predicates.records.record
            {
              credentials =
                predicates.allOf
                  [
                    predicates.isType 'Record,
                    predicates.records.record
                      {
                        password = predicates.isType '"String",
                        username = predicates.isType '"String",
                      }
                      {}
                      true
                      predicates.always
                  ],
              env = definitions.predicate.env,
              image = predicates.isType '"String",
              options = predicates.isType '"String",
              ports =
                predicates.allOf
                  [
                    predicates.isType '"Array",
                    predicates.arrays.arrayOf
                      (
                        predicates.oneOf
                          [
                            predicates.isType '"Number",
                            predicates.isType '"String"
                          ]
                      ),
                    predicates.arrays.minItems 1
                  ],
              volumes =
                predicates.allOf
                  [
                    predicates.isType '"Array",
                    predicates.arrays.arrayOf
                      (
                        predicates.allOf
                          [
                            predicates.isType '"String",
                            predicates.strings.pattern "^[^:]+:[^:]+$"
                          ]
                      ),
                    predicates.arrays.minItems 1
                  ],
            }
            {}
            false
            predicates.never
        ],
    defaults =
      predicates.allOf
        [
          predicates.isType 'Record,
          predicates.records.minProperties 1,
          predicates.records.record
            {
              run =
                predicates.allOf
                  [
                    predicates.isType 'Record,
                    predicates.records.minProperties 1,
                    predicates.records.record
                      {
                        shell = definitions.predicate.shell,
                        working-directory =
                          definitions.predicate.working-directory,
                      }
                      {}
                      false
                      predicates.never
                  ],
            }
            {}
            false
            predicates.never
        ],
    env
      | doc m%"
            To set custom environment variables, you need to specify the variables in the workflow file. You can define environment variables for a step, job, or entire workflow using the jobs.<job_id>.steps[*].env, jobs.<job_id>.env, and env keywords. For more information, see https://docs.github.com/en/actions/learn-github-actions/workflow-syntax-for-github-actions#jobsjob_idstepsenv
            "%
      =
        predicates.oneOf
          [
            predicates.allOf
              [
                predicates.isType 'Record,
                predicates.records.record
                  {}
                  {}
                  true
                  (
                    predicates.oneOf
                      [
                        predicates.isType '"String",
                        predicates.isType '"Number",
                        predicates.isType '"Bool"
                      ]
                  )
              ],
            definitions.predicate.stringContainingExpressionSyntax
          ],
    environment
      | doc m%"
            The environment that the job references
            "%
      =
        predicates.allOf
          [
            predicates.isType 'Record,
            predicates.records.required ["name"],
            predicates.records.record
              {
                name = predicates.isType '"String",
                url = predicates.isType '"String",
              }
              {}
              false
              predicates.never
          ],
    event =
      predicates.allOf
        [
          predicates.isType '"String",
          predicates.enum
            [
              "branch_protection_rule",
              "check_run",
              "check_suite",
              "create",
              "delete",
              "deployment",
              "deployment_status",
              "discussion",
              "discussion_comment",
              "fork",
              "gollum",
              "issue_comment",
              "issues",
              "label",
              "member",
              "milestone",
              "page_build",
              "project",
              "project_card",
              "project_column",
              "public",
              "pull_request",
              "pull_request_review",
              "pull_request_review_comment",
              "pull_request_target",
              "push",
              "registry_package",
              "release",
              "status",
              "watch",
              "workflow_call",
              "workflow_dispatch",
              "workflow_run",
              "repository_dispatch"
            ]
        ],
    eventObject =
      predicates.allOf
        [
          predicates.oneOf
            [predicates.isType 'Record, predicates.isType 'Null],
          predicates.records.record {} {} true predicates.always
        ],
    expressionSyntax =
      predicates.allOf
        [
          predicates.isType '"String",
          predicates.strings.pattern "^\\$\\{\\{(.|[\n])*\\}\\}$"
        ],
    globs =
      predicates.allOf
        [
          predicates.isType '"Array",
          predicates.arrays.arrayOf
            (
              predicates.allOf
                [
                  predicates.isType '"String",
                  predicates.strings.minLength 1
                ]
            ),
          predicates.arrays.minItems 1
        ],
    jobNeeds
      | doc m%"
            Identifies any jobs that must complete successfully before this job will run. It can be a string or array of strings. If a job fails, all jobs that need it are skipped unless the jobs use a conditional statement that causes the job to continue.
            "%
      =
        predicates.oneOf
          [
            predicates.allOf
              [
                predicates.isType '"Array",
                predicates.arrays.arrayOf definitions.predicate.name,
                predicates.arrays.minItems 1
              ],
            definitions.predicate.name
          ],
    machine =
      predicates.allOf
        [
          predicates.isType '"String",
          predicates.enum ["linux", "macos", "windows"]
        ],
    name =
      predicates.allOf
        [
          predicates.isType '"String",
          predicates.strings.pattern "^[_a-zA-Z][a-zA-Z0-9_-]*$"
        ],
    normalJob
      | doc m%"
            Each job must have an id to associate with the job. The key job_id is a string and its value is a map of the job's configuration data. You must replace <job_id> with a string that is unique to the jobs object. The <job_id> must start with a letter or _ and contain only alphanumeric characters, -, or _.
            "%
      =
        predicates.allOf
          [
            predicates.isType 'Record,
            predicates.records.required ["runs-on"],
            predicates.records.record
              {
                concurrency =
                  predicates.oneOf
                    [
                      predicates.isType '"String",
                      definitions.predicate.concurrency
                    ],
                container =
                  predicates.oneOf
                    [
                      predicates.isType '"String",
                      definitions.predicate.container
                    ],
                continue-on-error =
                  predicates.oneOf
                    [
                      predicates.isType '"Bool",
                      definitions.predicate.expressionSyntax
                    ],
                defaults = definitions.predicate.defaults,
                env = definitions.predicate.env,
                environment =
                  predicates.oneOf
                    [
                      predicates.isType '"String",
                      definitions.predicate.environment
                    ],
                "if" =
                  predicates.anyOf
                    [
                      predicates.isType '"Bool",
                      predicates.isType '"Number",
                      predicates.isType '"String"
                    ],
                name = predicates.isType '"String",
                needs = definitions.predicate.jobNeeds,
                outputs =
                  predicates.allOf
                    [
                      predicates.isType 'Record,
                      predicates.records.minProperties 1,
                      predicates.records.record
                        {}
                        {}
                        true
                        (predicates.isType '"String")
                    ],
                permissions = definitions.predicate.permissions,
                runs-on =
                  predicates.oneOf
                    [
                      predicates.allOf
                        [
                          predicates.isType '"String",
                          predicates.enum
                            [
                              "macos-10.15",
                              "macos-11",
                              "macos-12",
                              "macos-12-xl",
                              "macos-13",
                              "macos-13-xl",
                              "macos-latest",
                              "macos-latest-xl",
                              "self-hosted",
                              "ubuntu-18.04",
                              "ubuntu-20.04",
                              "ubuntu-22.04",
                              "ubuntu-latest",
                              "ubuntu-latest-4-cores",
                              "ubuntu-latest-8-cores",
                              "ubuntu-latest-16-cores",
                              "windows-2019",
                              "windows-2022",
                              "windows-latest",
                              "windows-latest-8-cores"
                            ]
                        ],
                      predicates.allOf
                        [
                          predicates.isType '"Array",
                          predicates.anyOf
                            [
                              predicates.allOf
                                [
                                  predicates.arrays.items
                                    [predicates.const "self-hosted"],
                                  predicates.arrays.additionalItems
                                    (predicates.isType '"String")
                                    1,
                                  predicates.arrays.minItems 1
                                ],
                              predicates.allOf
                                [
                                  predicates.arrays.items
                                    [
                                      predicates.const "self-hosted",
                                      definitions.predicate.machine
                                    ],
                                  predicates.arrays.additionalItems
                                    (predicates.isType '"String")
                                    2,
                                  predicates.arrays.minItems 2
                                ],
                              predicates.allOf
                                [
                                  predicates.arrays.items
                                    [
                                      predicates.const "self-hosted",
                                      definitions.predicate.architecture
                                    ],
                                  predicates.arrays.additionalItems
                                    (predicates.isType '"String")
                                    2,
                                  predicates.arrays.minItems 2
                                ],
                              predicates.allOf
                                [
                                  predicates.arrays.items
                                    [
                                      predicates.const "self-hosted",
                                      definitions.predicate.machine,
                                      definitions.predicate.architecture
                                    ],
                                  predicates.arrays.additionalItems
                                    (predicates.isType '"String")
                                    3,
                                  predicates.arrays.minItems 3
                                ],
                              predicates.allOf
                                [
                                  predicates.arrays.items
                                    [
                                      predicates.const "self-hosted",
                                      definitions.predicate.architecture,
                                      definitions.predicate.machine
                                    ],
                                  predicates.arrays.additionalItems
                                    (predicates.isType '"String")
                                    3,
                                  predicates.arrays.minItems 3
                                ],
                              predicates.allOf
                                [
                                  predicates.arrays.items
                                    [predicates.const "linux"],
                                  predicates.arrays.additionalItems
                                    (predicates.isType '"String")
                                    1,
                                  predicates.arrays.maxItems 2,
                                  predicates.arrays.minItems 2
                                ],
                              predicates.allOf
                                [
                                  predicates.arrays.items
                                    [predicates.const "windows"],
                                  predicates.arrays.additionalItems
                                    (predicates.isType '"String")
                                    1,
                                  predicates.arrays.maxItems 2,
                                  predicates.arrays.minItems 2
                                ]
                            ]
                        ],
                      predicates.allOf
                        [
                          predicates.isType 'Record,
                          predicates.records.record
                            {
                              group = predicates.isType '"String",
                              labels =
                                predicates.oneOf
                                  [
                                    predicates.isType '"String",
                                    predicates.allOf
                                      [
                                        predicates.isType '"Array",
                                        predicates.arrays.arrayOf
                                          (predicates.isType '"String")
                                      ]
                                  ],
                            }
                            {}
                            true
                            predicates.always
                        ],
                      definitions.predicate.stringContainingExpressionSyntax
                    ],
                services =
                  predicates.allOf
                    [
                      predicates.isType 'Record,
                      predicates.records.record
                        {}
                        {}
                        true
                        definitions.predicate.container
                    ],
                steps =
                  predicates.allOf
                    [
                      predicates.isType '"Array",
                      predicates.arrays.arrayOf
                        (
                          predicates.allOf
                            [
                              predicates.oneOf
                                [
                                  predicates.allOf
                                    [
                                      predicates.isType 'Record,
                                      predicates.records.required ["uses"],
                                      predicates.records.record
                                        { uses = predicates.isType '"String", }
                                        {}
                                        true
                                        predicates.always
                                    ],
                                  predicates.allOf
                                    [
                                      predicates.isType 'Record,
                                      predicates.records.required ["run"],
                                      predicates.records.record
                                        { run = predicates.isType '"String", }
                                        {}
                                        true
                                        predicates.always
                                    ]
                                ],
                              predicates.allOf
                                [
                                  predicates.isType 'Record,
                                  predicates.records.record
                                    {
                                      continue-on-error =
                                        predicates.oneOf
                                          [
                                            predicates.isType '"Bool",
                                            definitions.predicate.expressionSyntax
                                          ],
                                      env = definitions.predicate.env,
                                      id = predicates.isType '"String",
                                      "if" =
                                        predicates.anyOf
                                          [
                                            predicates.isType '"Bool",
                                            predicates.isType '"Number",
                                            predicates.isType '"String"
                                          ],
                                      name = predicates.isType '"String",
                                      run = predicates.isType '"String",
                                      shell = definitions.predicate.shell,
                                      timeout-minutes =
                                        predicates.oneOf
                                          [
                                            predicates.isType '"Number",
                                            definitions.predicate.expressionSyntax
                                          ],
                                      uses = predicates.isType '"String",
                                      with =
                                        predicates.allOf
                                          [
                                            predicates.records.record
                                              {
                                                args = predicates.isType '"String",
                                                entrypoint = predicates.isType '"String",
                                              }
                                              {}
                                              true
                                              predicates.always,
                                            definitions.predicate.env
                                          ],
                                      working-directory =
                                        definitions.predicate.working-directory,
                                    }
                                    {}
                                    false
                                    predicates.never,
                                  predicates.records.dependencies
                                    {
                                      shell = ["run"],
                                      working-directory = ["run"],
                                    }
                                ]
                            ]
                        ),
                      predicates.arrays.minItems 1
                    ],
                strategy =
                  predicates.allOf
                    [
                      predicates.isType 'Record,
                      predicates.records.required ["matrix"],
                      predicates.records.record
                        {
                          fail-fast = predicates.isType '"Bool",
                          matrix =
                            predicates.allOf
                              [
                                predicates.oneOf
                                  [
                                    predicates.isType 'Record,
                                    definitions.predicate.expressionSyntax
                                  ],
                                predicates.records.minProperties 1,
                                predicates.records.record
                                  {}
                                  {
                                    "^(in|ex)clude$" =
                                      predicates.allOf
                                        [
                                          predicates.isType '"Array",
                                          predicates.arrays.arrayOf
                                            (
                                              predicates.allOf
                                                [
                                                  predicates.isType 'Record,
                                                  predicates.records.record
                                                    {}
                                                    {}
                                                    true
                                                    definitions.predicate.configuration
                                                ]
                                            ),
                                          predicates.arrays.minItems 1
                                        ],
                                  }
                                  true
                                  (
                                    predicates.oneOf
                                      [
                                        predicates.allOf
                                          [
                                            predicates.isType '"Array",
                                            predicates.arrays.arrayOf
                                              definitions.predicate.configuration,
                                            predicates.arrays.minItems 1
                                          ],
                                        definitions.predicate.expressionSyntax
                                      ]
                                  )
                              ],
                          max-parallel =
                            predicates.anyOf
                              [
                                predicates.isType '"Number",
                                predicates.isType '"String"
                              ],
                        }
                        {}
                        false
                        predicates.never
                    ],
                timeout-minutes =
                  predicates.oneOf
                    [
                      predicates.isType '"Number",
                      definitions.predicate.expressionSyntax
                    ],
              }
              {}
              false
              predicates.never
          ],
    path
      | doc m%"
            When using the push and pull_request events, you can configure a workflow to run when at least one file does not match paths-ignore or at least one modified file matches the configured paths. Path filters are not evaluated for pushes to tags.
            The paths-ignore and paths keywords accept glob patterns that use the * and ** wildcard characters to match more than one path name. For more information, see https://help.github.com/en/github/automating-your-workflow-with-github-actions/workflow-syntax-for-github-actions#filter-pattern-cheat-sheet.
            You can exclude paths using two types of filters. You cannot use both of these filters for the same event in a workflow.
            - paths-ignore - Use the paths-ignore filter when you only need to exclude path names.
            - paths - Use the paths filter when you need to filter paths for positive matches and exclude paths.
            "%
      = definitions.predicate.globs,
    permissions
      | doc m%"
            You can modify the default permissions granted to the GITHUB_TOKEN, adding or removing access as required, so that you only allow the minimum required access.
            "%
      =
        predicates.oneOf
          [
            predicates.allOf
              [
                predicates.isType '"String",
                predicates.enum ["read-all", "write-all"]
              ],
            definitions.predicate.permissions-event
          ],
    permissions-event =
      predicates.allOf
        [
          predicates.isType 'Record,
          predicates.records.record
            {
              actions = definitions.predicate.permissions-level,
              checks = definitions.predicate.permissions-level,
              contents = definitions.predicate.permissions-level,
              deployments = definitions.predicate.permissions-level,
              discussions = definitions.predicate.permissions-level,
              id-token = definitions.predicate.permissions-level,
              issues = definitions.predicate.permissions-level,
              packages = definitions.predicate.permissions-level,
              pages = definitions.predicate.permissions-level,
              pull-requests = definitions.predicate.permissions-level,
              repository-projects = definitions.predicate.permissions-level,
              security-events = definitions.predicate.permissions-level,
              statuses = definitions.predicate.permissions-level,
            }
            {}
            false
            predicates.never
        ],
    permissions-level =
      predicates.allOf
        [
          predicates.isType '"String",
          predicates.enum ["read", "write", "none"]
        ],
    ref =
      predicates.allOf
        [
          predicates.oneOf
            [
              predicates.allOf
                [
                  predicates.isType 'Record,
                  predicates.allOf
                    [
                      predicates.not
                        (
                          predicates.allOf
                            [
                              predicates.records.required
                                ["branches", "branches-ignore"],
                              predicates.records.record
                                {}
                                {}
                                true
                                predicates.always
                            ]
                        ),
                      predicates.not
                        (
                          predicates.allOf
                            [
                              predicates.records.required ["tags", "tags-ignore"],
                              predicates.records.record
                                {}
                                {}
                                true
                                predicates.always
                            ]
                        ),
                      predicates.not
                        (
                          predicates.allOf
                            [
                              predicates.records.required ["paths", "paths-ignore"],
                              predicates.records.record
                                {}
                                {}
                                true
                                predicates.always
                            ]
                        )
                    ]
                ],
              predicates.isType 'Null
            ],
          predicates.records.record
            {
              branches = definitions.predicate.branch,
              branches-ignore = definitions.predicate.branch,
              paths = definitions.predicate.path,
              paths-ignore = definitions.predicate.path,
              tags = definitions.predicate.branch,
              tags-ignore = definitions.predicate.branch,
            }
            {}
            true
            predicates.always
        ],
    reusableWorkflowCallJob
      | doc m%"
            Each job must have an id to associate with the job. The key job_id is a string and its value is a map of the job's configuration data. You must replace <job_id> with a string that is unique to the jobs object. The <job_id> must start with a letter or _ and contain only alphanumeric characters, -, or _.
            "%
      =
        predicates.allOf
          [
            predicates.isType 'Record,
            predicates.records.required ["uses"],
            predicates.records.record
              {
                concurrency =
                  predicates.oneOf
                    [
                      predicates.isType '"String",
                      definitions.predicate.concurrency
                    ],
                "if" =
                  predicates.anyOf
                    [
                      predicates.isType '"Bool",
                      predicates.isType '"Number",
                      predicates.isType '"String"
                    ],
                name = predicates.isType '"String",
                needs = definitions.predicate.jobNeeds,
                permissions = definitions.predicate.permissions-event,
                secrets =
                  predicates.oneOf
                    [
                      definitions.predicate.env,
                      predicates.allOf
                        [
                          predicates.isType '"String",
                          predicates.enum ["inherit"]
                        ]
                    ],
                strategy =
                  predicates.allOf
                    [
                      predicates.isType 'Record,
                      predicates.records.required ["matrix"],
                      predicates.records.record
                        {
                          fail-fast = predicates.isType '"Bool",
                          matrix =
                            predicates.allOf
                              [
                                predicates.oneOf
                                  [
                                    predicates.isType 'Record,
                                    definitions.predicate.expressionSyntax
                                  ],
                                predicates.records.minProperties 1,
                                predicates.records.record
                                  {}
                                  {
                                    "^(in|ex)clude$" =
                                      predicates.allOf
                                        [
                                          predicates.isType '"Array",
                                          predicates.arrays.arrayOf
                                            (
                                              predicates.allOf
                                                [
                                                  predicates.isType 'Record,
                                                  predicates.records.record
                                                    {}
                                                    {}
                                                    true
                                                    definitions.predicate.configuration
                                                ]
                                            ),
                                          predicates.arrays.minItems 1
                                        ],
                                  }
                                  true
                                  (
                                    predicates.oneOf
                                      [
                                        predicates.allOf
                                          [
                                            predicates.isType '"Array",
                                            predicates.arrays.arrayOf
                                              definitions.predicate.configuration,
                                            predicates.arrays.minItems 1
                                          ],
                                        definitions.predicate.expressionSyntax
                                      ]
                                  )
                              ],
                          max-parallel =
                            predicates.anyOf
                              [
                                predicates.isType '"Number",
                                predicates.isType '"String"
                              ],
                        }
                        {}
                        false
                        predicates.never
                    ],
                uses =
                  predicates.allOf
                    [
                      predicates.isType '"String",
                      predicates.strings.pattern
                        "^(.+/)+(.+)\\.(ya?ml)(@.+)?$"
                    ],
                with = definitions.predicate.env,
              }
              {}
              false
              predicates.never
          ],
    shell
      | doc m%"
            You can override the default shell settings in the runner's operating system using the shell keyword. You can use built-in shell keywords, or you can define a custom set of shell options.
            "%
      =
        predicates.anyOf
          [
            predicates.isType '"String",
            predicates.allOf
              [
                predicates.isType '"String",
                predicates.enum
                  ["bash", "pwsh", "python", "sh", "cmd", "powershell"]
              ]
          ],
    stringContainingExpressionSyntax =
      predicates.allOf
        [
          predicates.isType '"String",
          predicates.strings.pattern "^.*\\$\\{\\{(.|[\n])*\\}\\}.*$"
        ],
    types
      | doc m%"
            Selects the types of activity that will trigger a workflow run. Most GitHub events are triggered by more than one type of activity. For example, the event for the release resource is triggered when a release is published, unpublished, created, edited, deleted, or prereleased. The types keyword enables you to narrow down activity that causes the workflow to run. When only one activity type triggers a webhook event, the types keyword is unnecessary.
            You can use an array of event types. For more information about each event and their activity types, see https://help.github.com/en/articles/events-that-trigger-workflows#webhook-events.
            "%
      =
        predicates.allOf
          [predicates.isType '"Array", predicates.arrays.minItems 1],
    working-directory
      | doc m%"
            Using the working-directory keyword, you can specify the working directory of where to run the command.
            "%
      = predicates.isType '"String",
  },
}
in

{
  concurrency
    | predicates.contract_from_predicate
      (
        predicates.oneOf
          [predicates.isType '"String", definitions.predicate.concurrency]
      )
    | doc m%"
    Concurrency ensures that only a single job or workflow using the same concurrency group will run at a time. A concurrency group can be any string or expression. The expression can use any context except for the secrets context.
    You can also specify concurrency at the workflow level.
    When a concurrent job or workflow is queued, if another job or workflow using the same concurrency group in the repository is in progress, the queued job or workflow will be pending. Any previously pending job or workflow in the concurrency group will be canceled. To also cancel any currently running job or workflow in the same concurrency group, specify cancel-in-progress: true.
    "%
    | optional,
  defaults
    | definitions.contract.defaults
    | doc m%"
    A map of default settings that will apply to all jobs in the workflow.
    "%
    | optional,
  env
    | definitions.contract.env
    | doc m%"
    A map of environment variables that are available to all jobs and steps in the workflow.
    "%
    | optional,
  jobs
    | predicates.contract_from_predicate
      (
        predicates.allOf
          [
            predicates.isType 'Record,
            predicates.records.minProperties 1,
            predicates.records.record
              {}
              {
                "^[_a-zA-Z][a-zA-Z0-9_-]*$" =
                  predicates.oneOf
                    [
                      definitions.predicate.normalJob,
                      definitions.predicate.reusableWorkflowCallJob
                    ],
              }
              false
              predicates.never
          ]
      )
    | doc m%"
    A workflow run is made up of one or more jobs. Jobs run in parallel by default. To run jobs sequentially, you can define dependencies on other jobs using the jobs.<job_id>.needs keyword.
    Each job runs in a fresh instance of the virtual environment specified by runs-on.
    You can run an unlimited number of jobs as long as you are within the workflow usage limits. For more information, see https://help.github.com/en/github/automating-your-workflow-with-github-actions/workflow-syntax-for-github-actions#usage-limits.
    "%,
  name
    | String
    | doc m%"
    The name of your workflow. GitHub displays the names of your workflows on your repository's actions page. If you omit this field, GitHub sets the name to the workflow's filename.
    "%
    | optional,
  on
    | predicates.contract_from_predicate
      (
        predicates.oneOf
          [
            definitions.predicate.event,
            predicates.allOf
              [
                predicates.isType '"Array",
                predicates.arrays.arrayOf definitions.predicate.event,
                predicates.arrays.minItems 1
              ],
            predicates.allOf
              [
                predicates.isType 'Record,
                predicates.records.record
                  {
                    branch_protection_rule =
                      predicates.allOf
                        [
                          predicates.records.record
                            {
                              types =
                                predicates.allOf
                                  [
                                    predicates.arrays.arrayOf
                                      (
                                        predicates.allOf
                                          [
                                            predicates.isType '"String",
                                            predicates.enum ["created", "edited", "deleted"]
                                          ]
                                      ),
                                    definitions.predicate.types
                                  ],
                            }
                            {}
                            true
                            predicates.always,
                          definitions.predicate.eventObject
                        ],
                    check_run =
                      predicates.allOf
                        [
                          predicates.records.record
                            {
                              types =
                                predicates.allOf
                                  [
                                    predicates.arrays.arrayOf
                                      (
                                        predicates.allOf
                                          [
                                            predicates.isType '"String",
                                            predicates.enum
                                              [
                                                "created",
                                                "rerequested",
                                                "completed",
                                                "requested_action"
                                              ]
                                          ]
                                      ),
                                    definitions.predicate.types
                                  ],
                            }
                            {}
                            true
                            predicates.always,
                          definitions.predicate.eventObject
                        ],
                    check_suite =
                      predicates.allOf
                        [
                          predicates.records.record
                            {
                              types =
                                predicates.allOf
                                  [
                                    predicates.arrays.arrayOf
                                      (
                                        predicates.allOf
                                          [
                                            predicates.isType '"String",
                                            predicates.enum
                                              ["completed", "requested", "rerequested"]
                                          ]
                                      ),
                                    definitions.predicate.types
                                  ],
                            }
                            {}
                            true
                            predicates.always,
                          definitions.predicate.eventObject
                        ],
                    create = definitions.predicate.eventObject,
                    delete = definitions.predicate.eventObject,
                    deployment = definitions.predicate.eventObject,
                    deployment_status = definitions.predicate.eventObject,
                    discussion =
                      predicates.allOf
                        [
                          predicates.records.record
                            {
                              types =
                                predicates.allOf
                                  [
                                    predicates.arrays.arrayOf
                                      (
                                        predicates.allOf
                                          [
                                            predicates.isType '"String",
                                            predicates.enum
                                              [
                                                "created",
                                                "edited",
                                                "deleted",
                                                "transferred",
                                                "pinned",
                                                "unpinned",
                                                "labeled",
                                                "unlabeled",
                                                "locked",
                                                "unlocked",
                                                "category_changed",
                                                "answered",
                                                "unanswered"
                                              ]
                                          ]
                                      ),
                                    definitions.predicate.types
                                  ],
                            }
                            {}
                            true
                            predicates.always,
                          definitions.predicate.eventObject
                        ],
                    discussion_comment =
                      predicates.allOf
                        [
                          predicates.records.record
                            {
                              types =
                                predicates.allOf
                                  [
                                    predicates.arrays.arrayOf
                                      (
                                        predicates.allOf
                                          [
                                            predicates.isType '"String",
                                            predicates.enum ["created", "edited", "deleted"]
                                          ]
                                      ),
                                    definitions.predicate.types
                                  ],
                            }
                            {}
                            true
                            predicates.always,
                          definitions.predicate.eventObject
                        ],
                    fork = definitions.predicate.eventObject,
                    gollum = definitions.predicate.eventObject,
                    issue_comment =
                      predicates.allOf
                        [
                          predicates.records.record
                            {
                              types =
                                predicates.allOf
                                  [
                                    predicates.arrays.arrayOf
                                      (
                                        predicates.allOf
                                          [
                                            predicates.isType '"String",
                                            predicates.enum ["created", "edited", "deleted"]
                                          ]
                                      ),
                                    definitions.predicate.types
                                  ],
                            }
                            {}
                            true
                            predicates.always,
                          definitions.predicate.eventObject
                        ],
                    issues =
                      predicates.allOf
                        [
                          predicates.records.record
                            {
                              types =
                                predicates.allOf
                                  [
                                    predicates.arrays.arrayOf
                                      (
                                        predicates.allOf
                                          [
                                            predicates.isType '"String",
                                            predicates.enum
                                              [
                                                "opened",
                                                "edited",
                                                "deleted",
                                                "transferred",
                                                "pinned",
                                                "unpinned",
                                                "closed",
                                                "reopened",
                                                "assigned",
                                                "unassigned",
                                                "labeled",
                                                "unlabeled",
                                                "locked",
                                                "unlocked",
                                                "milestoned",
                                                "demilestoned"
                                              ]
                                          ]
                                      ),
                                    definitions.predicate.types
                                  ],
                            }
                            {}
                            true
                            predicates.always,
                          definitions.predicate.eventObject
                        ],
                    label =
                      predicates.allOf
                        [
                          predicates.records.record
                            {
                              types =
                                predicates.allOf
                                  [
                                    predicates.arrays.arrayOf
                                      (
                                        predicates.allOf
                                          [
                                            predicates.isType '"String",
                                            predicates.enum ["created", "edited", "deleted"]
                                          ]
                                      ),
                                    definitions.predicate.types
                                  ],
                            }
                            {}
                            true
                            predicates.always,
                          definitions.predicate.eventObject
                        ],
                    member =
                      predicates.allOf
                        [
                          predicates.records.record
                            {
                              types =
                                predicates.allOf
                                  [
                                    predicates.arrays.arrayOf
                                      (
                                        predicates.allOf
                                          [
                                            predicates.isType '"String",
                                            predicates.enum ["added", "edited", "deleted"]
                                          ]
                                      ),
                                    definitions.predicate.types
                                  ],
                            }
                            {}
                            true
                            predicates.always,
                          definitions.predicate.eventObject
                        ],
                    merge_group =
                      predicates.allOf
                        [
                          predicates.records.record
                            {
                              types =
                                predicates.allOf
                                  [
                                    predicates.arrays.arrayOf
                                      (
                                        predicates.allOf
                                          [
                                            predicates.isType '"String",
                                            predicates.enum ["checks_requested"]
                                          ]
                                      ),
                                    definitions.predicate.types
                                  ],
                            }
                            {}
                            true
                            predicates.always,
                          definitions.predicate.eventObject
                        ],
                    milestone =
                      predicates.allOf
                        [
                          predicates.records.record
                            {
                              types =
                                predicates.allOf
                                  [
                                    predicates.arrays.arrayOf
                                      (
                                        predicates.allOf
                                          [
                                            predicates.isType '"String",
                                            predicates.enum
                                              ["created", "closed", "opened", "edited", "deleted"]
                                          ]
                                      ),
                                    definitions.predicate.types
                                  ],
                            }
                            {}
                            true
                            predicates.always,
                          definitions.predicate.eventObject
                        ],
                    page_build = definitions.predicate.eventObject,
                    project =
                      predicates.allOf
                        [
                          predicates.records.record
                            {
                              types =
                                predicates.allOf
                                  [
                                    predicates.arrays.arrayOf
                                      (
                                        predicates.allOf
                                          [
                                            predicates.isType '"String",
                                            predicates.enum
                                              [
                                                "created",
                                                "updated",
                                                "closed",
                                                "reopened",
                                                "edited",
                                                "deleted"
                                              ]
                                          ]
                                      ),
                                    definitions.predicate.types
                                  ],
                            }
                            {}
                            true
                            predicates.always,
                          definitions.predicate.eventObject
                        ],
                    project_card =
                      predicates.allOf
                        [
                          predicates.records.record
                            {
                              types =
                                predicates.allOf
                                  [
                                    predicates.arrays.arrayOf
                                      (
                                        predicates.allOf
                                          [
                                            predicates.isType '"String",
                                            predicates.enum
                                              [
                                                "created",
                                                "moved",
                                                "converted",
                                                "edited",
                                                "deleted"
                                              ]
                                          ]
                                      ),
                                    definitions.predicate.types
                                  ],
                            }
                            {}
                            true
                            predicates.always,
                          definitions.predicate.eventObject
                        ],
                    project_column =
                      predicates.allOf
                        [
                          predicates.records.record
                            {
                              types =
                                predicates.allOf
                                  [
                                    predicates.arrays.arrayOf
                                      (
                                        predicates.allOf
                                          [
                                            predicates.isType '"String",
                                            predicates.enum
                                              ["created", "updated", "moved", "deleted"]
                                          ]
                                      ),
                                    definitions.predicate.types
                                  ],
                            }
                            {}
                            true
                            predicates.always,
                          definitions.predicate.eventObject
                        ],
                    public = definitions.predicate.eventObject,
                    pull_request =
                      predicates.allOf
                        [
                          predicates.records.record
                            {
                              types =
                                predicates.allOf
                                  [
                                    predicates.arrays.arrayOf
                                      (
                                        predicates.allOf
                                          [
                                            predicates.isType '"String",
                                            predicates.enum
                                              [
                                                "assigned",
                                                "unassigned",
                                                "labeled",
                                                "unlabeled",
                                                "opened",
                                                "edited",
                                                "closed",
                                                "reopened",
                                                "synchronize",
                                                "converted_to_draft",
                                                "ready_for_review",
                                                "locked",
                                                "unlocked",
                                                "review_requested",
                                                "review_request_removed",
                                                "auto_merge_enabled",
                                                "auto_merge_disabled"
                                              ]
                                          ]
                                      ),
                                    definitions.predicate.types
                                  ],
                            }
                            {
                              "^(branche|tag|path)s(-ignore)?$" =
                                predicates.isType
                                  '"Array",
                            }
                            false
                            predicates.never,
                          definitions.predicate.ref
                        ],
                    pull_request_review =
                      predicates.allOf
                        [
                          predicates.records.record
                            {
                              types =
                                predicates.allOf
                                  [
                                    predicates.arrays.arrayOf
                                      (
                                        predicates.allOf
                                          [
                                            predicates.isType '"String",
                                            predicates.enum ["submitted", "edited", "dismissed"]
                                          ]
                                      ),
                                    definitions.predicate.types
                                  ],
                            }
                            {}
                            true
                            predicates.always,
                          definitions.predicate.eventObject
                        ],
                    pull_request_review_comment =
                      predicates.allOf
                        [
                          predicates.records.record
                            {
                              types =
                                predicates.allOf
                                  [
                                    predicates.arrays.arrayOf
                                      (
                                        predicates.allOf
                                          [
                                            predicates.isType '"String",
                                            predicates.enum ["created", "edited", "deleted"]
                                          ]
                                      ),
                                    definitions.predicate.types
                                  ],
                            }
                            {}
                            true
                            predicates.always,
                          definitions.predicate.eventObject
                        ],
                    pull_request_target =
                      predicates.allOf
                        [
                          predicates.records.record
                            {
                              types =
                                predicates.allOf
                                  [
                                    predicates.arrays.arrayOf
                                      (
                                        predicates.allOf
                                          [
                                            predicates.isType '"String",
                                            predicates.enum
                                              [
                                                "assigned",
                                                "unassigned",
                                                "labeled",
                                                "unlabeled",
                                                "opened",
                                                "edited",
                                                "closed",
                                                "reopened",
                                                "synchronize",
                                                "converted_to_draft",
                                                "ready_for_review",
                                                "locked",
                                                "unlocked",
                                                "review_requested",
                                                "review_request_removed",
                                                "auto_merge_enabled",
                                                "auto_merge_disabled"
                                              ]
                                          ]
                                      ),
                                    definitions.predicate.types
                                  ],
                            }
                            { "^(branche|tag|path)s(-ignore)?$" = predicates.always, }
                            false
                            predicates.never,
                          definitions.predicate.ref
                        ],
                    push =
                      predicates.allOf
                        [
                          predicates.records.record
                            {}
                            {
                              "^(branche|tag|path)s(-ignore)?$" =
                                predicates.allOf
                                  [
                                    predicates.isType '"Array",
                                    predicates.arrays.arrayOf (predicates.isType '"String")
                                  ],
                            }
                            false
                            predicates.never,
                          definitions.predicate.ref
                        ],
                    registry_package =
                      predicates.allOf
                        [
                          predicates.records.record
                            {
                              types =
                                predicates.allOf
                                  [
                                    predicates.arrays.arrayOf
                                      (
                                        predicates.allOf
                                          [
                                            predicates.isType '"String",
                                            predicates.enum ["published", "updated"]
                                          ]
                                      ),
                                    definitions.predicate.types
                                  ],
                            }
                            {}
                            true
                            predicates.always,
                          definitions.predicate.eventObject
                        ],
                    release =
                      predicates.allOf
                        [
                          predicates.records.record
                            {
                              types =
                                predicates.allOf
                                  [
                                    predicates.arrays.arrayOf
                                      (
                                        predicates.allOf
                                          [
                                            predicates.isType '"String",
                                            predicates.enum
                                              [
                                                "published",
                                                "unpublished",
                                                "created",
                                                "edited",
                                                "deleted",
                                                "prereleased",
                                                "released"
                                              ]
                                          ]
                                      ),
                                    definitions.predicate.types
                                  ],
                            }
                            {}
                            true
                            predicates.always,
                          definitions.predicate.eventObject
                        ],
                    repository_dispatch = definitions.predicate.eventObject,
                    schedule =
                      predicates.allOf
                        [
                          predicates.isType '"Array",
                          predicates.arrays.arrayOf
                            (
                              predicates.records.record
                                {
                                  cron =
                                    predicates.allOf
                                      [
                                        predicates.isType '"String",
                                        predicates.strings.pattern
                                          "^(((\\d+,)+\\d+|((\\d+|\\*)/\\d+|((JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP|OCT|NOV|DEC)(-(JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP|OCT|NOV|DEC))?))|(\\d+-\\d+)|\\d+(-\\d+)?/\\d+(-\\d+)?|\\d+|\\*|(MON|TUE|WED|THU|FRI|SAT|SUN)(-(MON|TUE|WED|THU|FRI|SAT|SUN))?) ?){5}$"
                                      ],
                                }
                                {}
                                false
                                predicates.never
                            ),
                          predicates.arrays.minItems 1
                        ],
                    status = definitions.predicate.eventObject,
                    watch = definitions.predicate.eventObject,
                    workflow_call =
                      predicates.records.record
                        {
                          inputs =
                            predicates.allOf
                              [
                                predicates.isType 'Record,
                                predicates.records.record
                                  {}
                                  {
                                    "^[_a-zA-Z][a-zA-Z0-9_-]*$" =
                                      predicates.allOf
                                        [
                                          predicates.isType 'Record,
                                          predicates.records.required ["type"],
                                          predicates.records.record
                                            {
                                              "default" =
                                                predicates.anyOf
                                                  [
                                                    predicates.isType '"Bool",
                                                    predicates.isType '"Number",
                                                    predicates.isType '"String"
                                                  ],
                                              deprecationMessage =
                                                predicates.isType
                                                  '"String",
                                              description = predicates.isType '"String",
                                              required = predicates.isType '"Bool",
                                              type =
                                                predicates.allOf
                                                  [
                                                    predicates.isType '"String",
                                                    predicates.enum
                                                      ["boolean", "number", "string"]
                                                  ],
                                            }
                                            {}
                                            false
                                            predicates.never
                                        ],
                                  }
                                  false
                                  predicates.never
                              ],
                          secrets =
                            predicates.records.record
                              {}
                              {
                                "^[_a-zA-Z][a-zA-Z0-9_-]*$" =
                                  predicates.allOf
                                    [
                                      predicates.records.required ["required"],
                                      predicates.records.record
                                        {
                                          description = predicates.isType '"String",
                                          required = predicates.isType '"Bool",
                                        }
                                        {}
                                        false
                                        predicates.never
                                    ],
                              }
                              false
                              predicates.never,
                        }
                        {}
                        true
                        predicates.always,
                    workflow_dispatch =
                      predicates.records.record
                        {
                          inputs =
                            predicates.allOf
                              [
                                predicates.isType 'Record,
                                predicates.records.record
                                  {}
                                  {
                                    "^[_a-zA-Z][a-zA-Z0-9_-]*$" =
                                      predicates.allOf
                                        [
                                          predicates.isType 'Record,
                                          predicates.allOf
                                            [
                                              predicates.ifThenElse
                                                (
                                                  predicates.allOf
                                                    [
                                                      predicates.records.required ["type"],
                                                      predicates.records.record
                                                        { type = predicates.const "string", }
                                                        {}
                                                        true
                                                        predicates.always
                                                    ]
                                                )
                                                (
                                                  predicates.records.record
                                                    { "default" = predicates.isType '"String", }
                                                    {}
                                                    true
                                                    predicates.always
                                                )
                                                predicates.always,
                                              predicates.ifThenElse
                                                (
                                                  predicates.allOf
                                                    [
                                                      predicates.records.required ["type"],
                                                      predicates.records.record
                                                        { type = predicates.const "boolean", }
                                                        {}
                                                        true
                                                        predicates.always
                                                    ]
                                                )
                                                (
                                                  predicates.records.record
                                                    { "default" = predicates.isType '"Bool", }
                                                    {}
                                                    true
                                                    predicates.always
                                                )
                                                predicates.always,
                                              predicates.ifThenElse
                                                (
                                                  predicates.allOf
                                                    [
                                                      predicates.records.required ["type"],
                                                      predicates.records.record
                                                        { type = predicates.const "number", }
                                                        {}
                                                        true
                                                        predicates.always
                                                    ]
                                                )
                                                (
                                                  predicates.records.record
                                                    { "default" = predicates.isType '"Number", }
                                                    {}
                                                    true
                                                    predicates.always
                                                )
                                                predicates.always,
                                              predicates.ifThenElse
                                                (
                                                  predicates.allOf
                                                    [
                                                      predicates.records.required ["type"],
                                                      predicates.records.record
                                                        { type = predicates.const "environment", }
                                                        {}
                                                        true
                                                        predicates.always
                                                    ]
                                                )
                                                (
                                                  predicates.records.record
                                                    { "default" = predicates.isType '"String", }
                                                    {}
                                                    true
                                                    predicates.always
                                                )
                                                predicates.always,
                                              predicates.ifThenElse
                                                (
                                                  predicates.allOf
                                                    [
                                                      predicates.records.required ["type"],
                                                      predicates.records.record
                                                        { type = predicates.const "choice", }
                                                        {}
                                                        true
                                                        predicates.always
                                                    ]
                                                )
                                                (
                                                  predicates.allOf
                                                    [
                                                      predicates.records.required ["options"],
                                                      predicates.records.record
                                                        {}
                                                        {}
                                                        true
                                                        predicates.always
                                                    ]
                                                )
                                                predicates.always
                                            ],
                                          predicates.records.required ["description"],
                                          predicates.records.record
                                            {
                                              "default" = predicates.always,
                                              deprecationMessage =
                                                predicates.isType
                                                  '"String",
                                              description = predicates.isType '"String",
                                              options =
                                                predicates.allOf
                                                  [
                                                    predicates.isType '"Array",
                                                    predicates.arrays.arrayOf
                                                      (predicates.isType '"String"),
                                                    predicates.arrays.minItems 1
                                                  ],
                                              required = predicates.isType '"Bool",
                                              type =
                                                predicates.allOf
                                                  [
                                                    predicates.isType '"String",
                                                    predicates.enum
                                                      [
                                                        "string",
                                                        "choice",
                                                        "boolean",
                                                        "number",
                                                        "environment"
                                                      ]
                                                  ],
                                            }
                                            {}
                                            false
                                            predicates.never
                                        ],
                                  }
                                  false
                                  predicates.never
                              ],
                        }
                        {}
                        true
                        predicates.always,
                    workflow_run =
                      predicates.allOf
                        [
                          predicates.records.record
                            {
                              types =
                                predicates.allOf
                                  [
                                    predicates.arrays.arrayOf
                                      (
                                        predicates.allOf
                                          [
                                            predicates.isType '"String",
                                            predicates.enum ["requested", "completed"]
                                          ]
                                      ),
                                    definitions.predicate.types
                                  ],
                              workflows =
                                predicates.allOf
                                  [
                                    predicates.isType '"Array",
                                    predicates.arrays.arrayOf (predicates.isType '"String"),
                                    predicates.arrays.minItems 1
                                  ],
                            }
                            { "^branches(-ignore)?$" = predicates.always, }
                            true
                            predicates.always,
                          definitions.predicate.eventObject
                        ],
                  }
                  {}
                  false
                  predicates.never
              ]
          ]
      )
    | doc m%"
    The name of the GitHub event that triggers the workflow. You can provide a single event string, array of events, array of event types, or an event configuration map that schedules a workflow or restricts the execution of a workflow to specific files, tags, or branch changes. For a list of available events, see https://help.github.com/en/github/automating-your-workflow-with-github-actions/events-that-trigger-workflows.
    "%,
  permissions | definitions.contract.permissions | optional,
  run-name
    | String
    | doc m%"
    The name for workflow runs generated from the workflow. GitHub displays the workflow run name in the list of workflow runs on your repository's 'Actions' tab.
    "%
    | optional,
}
