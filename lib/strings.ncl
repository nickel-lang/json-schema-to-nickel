{
  maxLength
    : Number -> Dyn -> [| 'Ok, 'Err String |]
    | doc m%"
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.3.1]
    "%
    = fun limit x =>
      if !std.is_string x then
        'Ok
      else if std.string.length (x | String) <= limit then
        'Ok
      else
        'Err "expected a string of length no larger than %{std.string.from_number limit}",

  minLength
    : Number -> Dyn -> [| 'Ok, 'Err String |]
    | doc m%"
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.3.2]
    "%
    = fun limit x =>
      if !std.is_string x then
        'Ok
      else if std.string.length (x | String) >= limit then
        'Ok
      else
        'Err "expected a string of length no smaller than %{std.string.from_number limit}",

  pattern
    : String -> Dyn -> [| 'Ok, 'Err String |]
    | doc m%"
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.3.3]
    "%
    = fun pattern x =>
      if !std.is_string x then
        'Ok
      else if std.string.is_match pattern (x | String) then
        'Ok
      else
        'Err "expected a string matching the pattern `%{pattern}`",
}

