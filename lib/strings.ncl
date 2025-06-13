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
}
