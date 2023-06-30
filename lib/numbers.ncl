{
  multipleOf
    : Number -> Dyn -> { success : Bool, error : String }
    | doc m%"
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.2.1]
    "%
    = fun mult x =>
      if !std.is_number x then
        { success = true, error = "" }
      else if std.number.is_integer ((x | Number) / mult) then
        { success = true, error = "" }
      else
        { success = false, error = "expected a multiple of %{std.string.from_number mult}" },

  maximum
    : Number -> Dyn -> { success : Bool, error : String }
    | doc m%"
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.2.2]
    "%
    = fun limit x =>
      if !std.is_number x then
        { success = true, error = "" }
      else if (x | Number) <= limit then
        { success = true, error = "" }
      else
        { success = false, error = "expected a maximum of %{std.string.from_number limit}" },

  exclusiveMaximum
    : Number -> Dyn -> { success : Bool, error : String }
    | doc m%"
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.2.3]
    "%
    = fun limit x =>
      if !std.is_number x then
        { success = true, error = "" }
      else if (x | Number) < limit then
        { success = true, error = "" }
      else
        { success = false, error = "expected an exclusive maximum of %{std.string.from_number limit}" },

  minimum
    : Number -> Dyn -> { success : Bool, error : String }
    | doc m%"
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.2.4]
    "%
    = fun limit x =>
      if !std.is_number x then
        { success = true, error = "" }
      else if (x | Number) >= limit then
        { success = true, error = "" }
      else
        { success = false, error = "expected a minimum of %{std.string.from_number limit}" },

  exclusiveMinimum
    : Number -> Dyn -> { success : Bool, error : String }
    | doc m%"
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.2.5]
    "%
    = fun limit x =>
      if !std.is_number x then
        { success = true, error = "" }
      else if (x | Number) > limit then
        { success = true, error = "" }
      else
        { success = false, error = "expected an exclusive minimum of %{std.string.from_number limit}" },
}

