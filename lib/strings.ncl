{
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
        if !std.is_string value then
          'Error { message = "expected a string" }
        else if std.string.length value <= limit then
          'Ok
        else
          'Error {
            message = "expected a string of length at most %{std.string.from_number limit}",
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
        if !std.is_string value then
          'Error { message = "expected a string" }
        else if std.string.length value >= limit then
          'Ok
        else
          'Error {
            message = "expected a string of length at least %{std.string.from_number limit}",
          }
      ),

  # TODO: remove this after updating nickel to a version with std.string.Matches
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
    = fun regex =>
      let is_match = std.string.is_match regex in
      std.contract.custom (fun _label value =>
        if std.typeof value != 'String then
          'Error { message = "expected a string" }
        else if is_match value then
          'Ok value
        else
          'Error { message = "expected a string matching `%{regex}`" }
      ),
}
