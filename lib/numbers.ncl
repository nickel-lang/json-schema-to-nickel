{
  multipleOf
    : Number -> Dyn -> [| 'Ok, 'Error { message : String, source : Array [| 'Array Number, 'Field String |] } |]
    | doc m%"
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.2.1]
    "%
    = fun mult x =>
      if !std.is_number x then
        'Ok
      else if std.number.is_integer ((x | Number) / mult) then
        'Ok
      else
        'Error {
          message = "expected a multiple of %{std.string.from_number mult}",
          source = [],
        },

  maximum
    : Number -> Dyn -> [| 'Ok, 'Error { message : String, source : Array [| 'Array Number, 'Field String |] } |]
    | doc m%"
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.2.2]
    "%
    = fun limit x =>
      if !std.is_number x then
        'Ok
      else if (x | Number) <= limit then
        'Ok
      else
        'Error {
          message = "expected a maximum of %{std.string.from_number limit}",
          source = [],
        },

  exclusiveMaximum
    : Number -> Dyn -> [| 'Ok, 'Error { message : String, source : Array [| 'Array Number, 'Field String |] } |]
    | doc m%"
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.2.3]
    "%
    = fun limit x =>
      if !std.is_number x then
        'Ok
      else if (x | Number) < limit then
        'Ok
      else
        'Error {
          message = "expected an exclusive maximum of %{std.string.from_number limit}",
          source = [],
        },

  minimum
    : Number -> Dyn -> [| 'Ok, 'Error { message : String, source : Array [| 'Array Number, 'Field String |] } |]
    | doc m%"
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.2.4]
    "%
    = fun limit x =>
      if !std.is_number x then
        'Ok
      else if (x | Number) >= limit then
        'Ok
      else
        'Error {
          message = "expected a minimum of %{std.string.from_number limit}",
          source = [],
        },

  exclusiveMinimum
    : Number -> Dyn -> [| 'Ok, 'Error { message : String, source : Array [| 'Array Number, 'Field String |] } |]
    | doc m%"
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.2.5]
    "%
    = fun limit x =>
      if !std.is_number x then
        'Ok
      else if (x | Number) > limit then
        'Ok
      else
        'Error {
          message = "expected an exclusive minimum of %{std.string.from_number limit}",
          source = [],
        },
}
