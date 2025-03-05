{
  maxLength
    : Number -> Dyn -> [|
      'Ok,
      'Error { message : String, source : Array [| 'Array Number, 'Field String |] }
    |]
    | doc m%"
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.3.1]
    "%
    = fun limit x =>
      if !std.is_string x then
        'Ok
      else if std.string.length (x | String) <= limit then
        'Ok
      else
        'Error {
          message = "expected a string of length no larger than %{std.string.from_number limit}",
          source = [],
        },

  minLength
    : Number -> Dyn -> [|
      'Ok,
      'Error { message : String, source : Array [| 'Array Number, 'Field String |] }
    |]
    | doc m%"
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.3.2]
    "%
    = fun limit x =>
      if !std.is_string x then
        'Ok
      else if std.string.length (x | String) >= limit then
        'Ok
      else
        'Error {
          message = "expected a string of length no smaller than %{std.string.from_number limit}",
          source = [],
        },

  pattern
    : String -> Dyn -> [|
      'Ok,
      'Error { message : String, source : Array [| 'Array Number, 'Field String |] }
    |]
    | doc m%"
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.3.3]
    "%
    = fun pattern x =>
      if !std.is_string x then
        'Ok
      else if std.string.is_match pattern (x | String) then
        'Ok
      else
        'Error {
          message = "expected a string matching the pattern `%{pattern}`",
          source = [],
        },

  contract = {
    Pattern | String -> Dyn = fun regex =>
      std.contract.from_validator (fun s =>
        let is_match = std.string.is_match regex in
        if !std.is_string s then
          'Error { message = "expected a string" }
        else if !is_match s then
          'Error { message = "expected %{regex}" }
        else
          'Ok
      ),

    MinLength | Number -> Dyn = (fun len =>
      std.contract.from_validator (fun s =>
        if !std.is_string s then
          'Error { message = "expected a string" }
        else if std.string.length s < len then
          'Error { message = "expected at least %{len} characters" }
        else
          'Ok
      )
    ),

    MaxLength | Number -> Dyn = (fun len =>
      std.contract.from_validator (fun s =>
        if !std.is_string s then
          'Error { message = "expected a string" }
        else if std.string.length s > len then
          'Error { message = "expected at most %{len} characters" }
        else
          'Ok
      )
    ),
  },
}
