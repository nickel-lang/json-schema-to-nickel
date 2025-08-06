{
  EagerRecordField
    | doc m%"
      `EagerRecordField name Ctr` is a contract asserting that the value is a record, with a field
      satisfying the contract `Ctr`.

      This differs from `{ name | Ctr }` in that it is one level more eager: it eagerly checks the
      existence of the field `name` and applies `Ctr` to it.

      # Examples

      ```nickel multiline
      { foo = 1, bar = 2 } | EagerRecordField "foo" Number
      
      # This doesn't fail, because the record contract is lazy and
      # foo never gets evaluated.
      ({ foo = 1, bar = 2 } | { foo | String, .. }).bar
      
      # This does fail, because EagerRecordField is that little bit more eager.
      ({ foo = 1, bar = 2 } | EagerRecordField "foo" String).bar
      # => error: contract broken by a value
      ```
    "%
    = fun field Ctr =>
      std.contract.custom (fun label record =>
        if !(std.is_record record) then
          'Error { message = "expected a record" }
        else if !(std.record.has_field field record) then
          'Error { message = "missing field `%{field}`" }
        else
          std.contract.check Ctr label (record."%{field}")
          |> match {
            'Ok v => 'Ok (std.record.update field v record),
            'Error e => 'Error e,
          }
      ),
  ArrayOf
    | Dyn -> Dyn
    | doc m%"
      An eager version of `Array`. `value | ArrayOf C` will succeed if `value` is an array and all of
      its elements satisfy the eager part of the contract `C`.

      This is lifted from the json schema lib, because it didn't feel like it made sense for the
      github contract to depend on json-schema-lib. It might make sense to have it somewhere common.

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
  UniqueNames
    | doc m%"
      A contract that you can apply to an array of records, each of which has a `name` field.

      Checks that the `name` fields are all unique.

      # Examples

      ```nickel multiline
      [ { name = "hi" }, { name = "bye" } ] | UniqueNames

      [ { name = "hi" }, { name = "bye" }, { name = "hi" } ] | UniqueNames
      # => error: name `hi` is duplicated

      [ { id = "not a name" } ] | UniqueNames
      # => error: contract broken by a value
      ```
    "%
    =
      std.contract.custom (fun label vals =>
        std.contract.check (ArrayOf (EagerRecordField "name" String)) label vals
        |> match {
          'Error e => 'Error e,
          'Ok vals =>
            # Our accumulator is { _ | Bool }, representing
            # the set of field names we've seen so far.
            # (but it's a { _ | Bool } because Nickel doesn't have sets yet...
            vals
            |> std.array.try_fold_left
              (fun acc val =>
                if std.record.has_field val.name acc then
                  # This little hack allows us to point to the location of the duplicated name.
                  # We know the Number contract will fail because we already checked that the
                  # `name` fields are strings.
                  #
                  # (I also tried `std.contract.from_predicate fun _ => false`, but it didn't
                  # point to the right location for some reason.
                  let 'Error loc = std.contract.check Number label val.name in
                  'Error { message = "name `%{val.name}` is duplicated", blame_location = loc.blame_location }
                else
                  'Ok (std.record.insert val.name true acc)
              )
              {}
            |> match {
              'Ok _ => 'Ok vals,
              'Error e => 'Error e,
            }
          # let name_counts = vals |> std.array.fold_left (fun acc val => std.record.update val.name ((std.record.get_or val.name 0 acc) + 1) acc) {} in
          # let duplicated = name_counts |> std.record.filter (fun _name count => count > 1) |> std.record.fields in
          # if std.array.length duplicated == 0 then
          #   'Ok vals
          # else
          #   'Error { message = "name `%{std.array.first duplicated}` is duplicated" }
        }
      ),
}
