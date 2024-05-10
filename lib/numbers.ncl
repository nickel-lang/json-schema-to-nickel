{
  multipleOf
    : Number -> Dyn -> [| 'Ok, 'Err String |]
    | doc m%"
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.2.1]
    "%
    = fun mult x =>
      if !std.is_number x then
        'Ok
      else if std.number.is_integer ((x | Number) / mult) then
        'Ok
      else
        'Err "expected a multiple of %{std.string.from_number mult}",

  maximum
    : Number -> Dyn -> [| 'Ok, 'Err String |]
    | doc m%"
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.2.2]
    "%
    = fun limit x =>
      if !std.is_number x then
        'Ok
      else if (x | Number) <= limit then
        'Ok
      else
        'Err "expected a maximum of %{std.string.from_number limit}",

  exclusiveMaximum
    : Number -> Dyn -> [| 'Ok, 'Err String |]
    | doc m%"
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.2.3]
    "%
    = fun limit x =>
      if !std.is_number x then
        'Ok
      else if (x | Number) < limit then
        'Ok
      else
        'Err "expected an exclusive maximum of %{std.string.from_number limit}",

  minimum
    : Number -> Dyn -> [| 'Ok, 'Err String |]
    | doc m%"
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.2.4]
    "%
    = fun limit x =>
      if !std.is_number x then
        'Ok
      else if (x | Number) >= limit then
        'Ok
      else
        'Err "expected a minimum of %{std.string.from_number limit}",

  exclusiveMinimum
    : Number -> Dyn -> [| 'Ok, 'Err String |]
    | doc m%"
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.2.5]
    "%
    = fun limit x =>
      if !std.is_number x then
        'Ok
      else if (x | Number) > limit then
        'Ok
      else
        'Err "expected an exclusive minimum of %{std.string.from_number limit}",
}

