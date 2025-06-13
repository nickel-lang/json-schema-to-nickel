{
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
    = std.contract.from_predicate ((==) null),

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
          'Error {
            message = "expected %{std.to_string expected}"
          }
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
        contracts
        |> std.array.fold_right
        # The accumulator is either `'Count n`, where `n != 1` is the number of contract
        # checks that succeeded, or it's `'Ok v`, where `v` is the value propagated by
        # the successful contract.
          (fun ctr acc =>
            std.contract.check ctr label value
            |> match {
              'Ok val =>
                acc
                |> match {
                  'Count 0 => 'Ok val,
                  'Count n => 'Count (n + 1),
                  'Ok _ => 'Count 2,
                },
              'Error e => acc
            }
          )
          ('Count 0)
        |> match {
          'Count n => 'Error { message = "%{std.to_string n} of the contracts passed, expected precisely one to pass" },
          'Ok v => 'Ok v
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
        std.contract.check iph label value
        |> match {
          'Ok v => std.contract.check zen label v,
          'Error _ => std.contract.check els label value,
        }
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
          input == variant || (std.is_enum input && (std.string.from_enum input == variant | Bool))
        in
        if std.array.any (check_equal value) variants then
          'Ok
        else
          'Error { message = "expected one of %{std.serialize 'Json variants}" }
      ),

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

  array = import "./arrays.ncl",
  record = import "./records.ncl",
  number = import "./numbers.ncl",
  string = import "./strings.ncl",
}
