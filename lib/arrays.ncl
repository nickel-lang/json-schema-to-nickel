{
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
        if !std.is_array value then
          'Error { message = "expected an array" }
        else
          value
          |> std.array.try_fold_left
            (fun acc elt =>
              std.contract.check ctr label elt
              |> match {
                'Ok v => 'Ok (acc @ [v]),
                'Error e => 'Error e
              }
            )
            []
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
        if !std.is_array value then
          'Error { message = "expected an array" }
        else
          let good_elt = (fun elt => std.contract.check contract label elt |> match { 'Ok _ => true, 'Error _ => false }) in
          if value |> std.array.any good_elt then
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
        if !std.is_array value then
          'Error { message = "expected an array" }
        else
          let length_to_check = std.number.min (std.array.length elt_contracts) (std.array.length value) in
          let { left, right } = std.array.split_at length_to_check value in
          std.array.zip_with (fun contract elt => std.contract.check contract label elt) elt_contracts left
          |> std.array.try_fold_left
            (fun acc =>
              match {
                'Ok v => 'Ok (acc @ [v]),
                'Error e => 'Error e
              }
            )
            []
          |> match {
            'Error e => 'Error e,
            'Ok left_with_contracts =>
              # The individual elements succeeded. If there's anything left in the array, check the `rest` contract.
              right
              |> std.array.try_fold_left
                (fun acc elt =>
                  std.contract.check rest label elt
                  |> match {
                    'Ok v => 'Ok (acc @ [v]),
                    'Error e => 'Error e
                  }
                )
                left_with_contracts
          }
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
      std.contract.from_validator (fun x =>
        if !std.is_array x then
          'Error { message = "expected an array" }
        else
          let { has_duplicate, duplicate, .. } = find_duplicate x
          in
          if has_duplicate then
            'Error { message = "duplicate found: %{std.serialize 'Json duplicate}" }
          else
            'Ok
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
      std.contract.from_predicate (fun arr => std.is_array arr && std.array.length (arr | Array Dyn) >= n),

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
      std.contract.from_predicate (fun arr => std.is_array arr && std.array.length (arr | Array Dyn) <= n),
}
