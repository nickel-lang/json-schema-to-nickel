{
  arrayOf
    : (
      Dyn -> [|
        'Ok,
        'Err {
          message : String,
          source
            : Array [|
              'Array Number,
              'Field String
            |]
        }
      |]
    ) -> Dyn -> [|
      'Ok,
      'Err {
        message : String,
        source
          : Array [| 'Array Number, 'Field String |]
      }
    |]
    | doc m%"
      Apply a predicate to all elements of an array, succeeding if all
      applications succeed.
    "%
    = fun pred x =>
      if !std.is_array x then
        'Ok
      else
        let x = x | Array Dyn in
        x
        |> std.array.fold_right
          (fun x acc =>
            pred x
            |> match {
              'Ok => acc,
              'Err msg => 'Err msg,
            }
          )
          'Ok,

  contains
    : (Dyn -> [| 'Ok, 'Err String |]) -> Dyn -> [| 'Ok, 'Err String |]
    | doc m%"
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.4.6]
    "%
    = fun pred x =>
      if !std.is_array x then
        'Ok
      else
        let x = x | Array Dyn in
        x
        |> std.array.fold_right
          (fun x acc =>
            acc
            |> match {
              'Ok => 'Ok,
              'Errs msgs =>
                pred x
                |> match {
                  'Ok => 'Ok,
                  'Err msg => 'Errs (["  - %{msg}"] @ msgs),
                }
            }
          )
          ('Errs [])
        |> match {
          'Ok => 'Ok,
          'Errs msgs => 'Err "contains: no elements matched\n%{std.string.join "\n" msgs}"
        },

  items
    : Array (Dyn -> [| 'Ok, 'Err String |]) -> Dyn -> [| 'Ok, 'Err String |]
    | doc m%"
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.4.1]
    "%
    = fun preds x =>
      if !std.is_array x then
        'Ok
      else
        let x = x | Array Dyn in
        let length_to_check = std.number.min (std.array.length preds) (std.array.length x) in
        std.array.range 0 length_to_check
        |> std.array.fold_right
          (fun i acc =>
            (std.array.at i preds) (std.array.at i x)
            |> match {
              'Err msg => 'Err msg,
              'Ok => acc
            }
          )
          'Ok,

  additionalItems
    : (Dyn -> [| 'Ok, 'Err String |]) -> Number -> Dyn -> [| 'Ok, 'Err String |]
    | doc m%"
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.4.2]
    "%
    = fun pred start x =>
      if !std.is_array x then
        'Ok
      else
        let x = x | Array Dyn in
        let value_length = std.array.length x in
        if start >= value_length then
          'Ok
        else
          (std.array.slice start value_length x | Dyn)
          |> arrayOf pred,

  maxItems
    : Number -> Dyn -> [| 'Ok, 'Err String |]
    | doc m%"
      `maxItems n x` fails if `x` is an array of length strictly greater than `n` and succeeds otherwise.
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.4.3]
    "%
    = fun n x =>
      if !std.is_array x then
        'Ok
      else if std.array.length (x | Array Dyn) > n then
        'Err "array is longer than %{std.string.from_number n} items"
      else
        'Ok,

  minItems
    : Number -> Dyn -> [| 'Ok, 'Err String |]
    | doc m%"
      `minItems n x` fails if `x` is an array of length strictly smaller than `n` and succeeds otherwise.
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.4.4]
    "%
    = fun n x =>
      if !std.is_array x then
        'Ok
      else if std.array.length (x | Array Dyn) < n then
        'Err "array is shorter than %{std.string.from_number n} items"
      else
        'Ok,

  uniqueItems
    : Dyn -> [| 'Ok, 'Err String |]
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
            (fun elt acc =>
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
          'Ok
        else
          let { has_duplicate, duplicate, .. } = find_duplicate (x | Array Dyn)
          in
          if has_duplicate then
            'Err "duplicate found: %{std.serialize 'Json duplicate}"
          else
            'Ok
}
