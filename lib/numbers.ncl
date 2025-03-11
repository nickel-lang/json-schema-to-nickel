let from_predicate | (Number -> Bool) -> String -> Dyn = fun pred msg =>
  std.contract.from_validator (fun value =>
    if !std.is_number value then
      'Error { message = "expected a number" }
    else if pred (value | Number) then
      'Ok
    else
      'Error { message = msg }
  )
in
{
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
        if !std.is_number value then
          'Error { message = "expected a number" }
        else if n == 0 then
          'Error { message = "expected a multiple of 0" }
        else if std.number.is_integer (value / n) then
          'Ok
        else
          'Error { message = "expected a multiple of %{std.string.from_number n}" }
      ),

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
    = fun limit => from_predicate (fun x => x <= limit) "expected a maximum of %{std.string.from_number limit}",

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
    = fun limit => from_predicate (fun x => x < limit) "expected an exclusive maximum of %{std.string.from_number limit}",

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
    = fun limit => from_predicate (fun x => x >= limit) "expected a minimum of %{std.string.from_number limit}",

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
    = fun limit => from_predicate (fun x => x > limit) "expected an exclusive minimum of %{std.string.from_number limit}",
}
