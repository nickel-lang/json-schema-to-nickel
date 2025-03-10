{
  eager = {
    ArrayOf
      | Dyn -> Dyn
      | doc m%"
      An eager version of `Array`. `value | ArrayOf C` will succeed if `value` is an array and all of
      its elements satisfy the contract `C`.
    "%
      = fun ctr =>
        std.contract.from_validator (fun value =>
          if !std.is_array value then
            'Error { message = "expected an array" }
          else
            value
            |> std.array.fold_right
              (fun elt acc =>
                std.contract.check ctr elt
                |> match {
                  'Ok _ => acc,
                  'Error e => 'Error e
                }
              )
              'Ok
        ),

    Items | Array Dyn -> Dyn -> Dyn = fun items rest =>
      std.contract.from_validator (fun value =>
        if !std.is_array value then
          'Error { message = "expected an array" }
        else
          # This seems off to me, but it's what the json-schema spec says: it's ok if the array
          # we're validating is shorter than the array of contracts.
          let length_to_check = std.number.min (std.array.length items) (std.array.length value) in
          let { left, right } = std.array.split_at length_to_check value in
          std.array.zip_with (fun ctr x => std.contract.check ctr x) items left
          |> std.array.fold_left
            (fun acc =>
              match {
                'Ok _ => acc,
                'Error e => 'Error e
              }
            )
            'Ok
          |> match {
            'Error e => 'Error e,
            'Ok =>
              # The individual elements succeeded. If there's anything left in the array, check the `rest` contract.
              right
              |> std.array.fold_right
                (fun elt acc =>
                  std.contract.check rest elt
                  |> match {
                    'Ok _ => acc,
                    'Error e => 'Error e
                  }
                )
                'Ok
          }
      ),
  },

  contains
    : (
      Dyn -> [|
        'Ok,
        'Error { message : String, source : Array [| 'Array Number, 'Field String |] }
      |]
    ) -> Dyn -> [|
      'Ok,
      'Error { message : String, source : Array [| 'Array Number, 'Field String |] }
    |]
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
              'Errors errors =>
                pred x
                |> match {
                  'Ok => 'Ok,
                  'Error error => 'Errors (std.array.prepend error errors),
                }
            }
          )
          ('Errors [])
        |> match {
          'Ok => 'Ok,
          'Errors errors => 'Ok
          #'Error (error_lib.combine "contains: no elements matched" errors),
        },

  # TODO: lazy version of Items

  uniqueItems
    | doc m%"
      Succeeds for any array if its elements are pairwise distinct.
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.4.5]
    "%
    =
      let find_duplicate
        : Array Dyn -> { has_duplicate : Bool, duplicate : Dyn, seen : { _ : Bool } }
        | doc m%"
            Try to find a duplicate in an array of Nickel values. For
            efficiency, this function uses JSON serialization to index into
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
          'Error { message = "expected an array" }
        else
          let { has_duplicate, duplicate, .. } = find_duplicate x
          in
          if has_duplicate then
            'Error { message = "duplicate found: %{std.serialize 'Json duplicate}" }
          else
            'Ok,

  minItems
    : Number -> Dyn
    | doc m%"
        A contract that checks whether an array has at least a certain number of items.
      "%
    = fun n =>
      std.contract.from_predicate (fun arr => std.typeof arr == 'Array && std.array.length (arr | Array Dyn) >= n),

  maxItems
    : Number -> Dyn
    | doc m%"
        A contract that checks whether an array has at most a certain number of items.
      "%
    = fun n =>
      std.contract.from_predicate (fun arr => std.typeof arr == 'Array && std.array.length (arr | Array Dyn) <= n),
}
