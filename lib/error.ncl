# Helpers for annotating error data
{
  at_field
    | doc m%"
      Attaches a given field name to the error source of this result, if it is
      an `'Error`. Returns the result unchanged if it is an `'Ok`.
    "%
    = fun field =>
      match {
        'Ok => 'Ok,
        'Error error =>
          'Error {
            message = error.message,
            source =
              std.array.append
                ('Field field)
                error.source
          },
      },

  at_index
    | doc m%"
        Appends an array access at the specificed index to the error source of
        this result, if it is an `'Error`. Returns the result unchanged if it is
        an `'Ok`.
      "%
    = fun index =>
      match {
        'Ok => 'Ok,
        'Error error =>
          'Error {
            message = error.message,
            source =
              std.array.append
                ('Array index)
                error.source
          },
      },

  source_to_string
    : {
      message : String,
      source
        : Array [|
          'Array Number,
          'Field String
        |]
    } -> String
    | doc "Renders an error source location as a string."
    = fun { source, .. } =>
      source
      |> std.array.map (match {
        'Array index => "[%{std.string.from_number index}]",
        'Field field => field,
      }
      )
      |> std.string.join ".",

  combine
    | doc m%"
        Combines an array of errors into a single error with a given initial message.
        Each error message will be listed after the initial message together
        with their source rendered as a path.
      "%
    = fun init_msg errors =>
      let msgs =
        errors
        |> std.array.map (fun error => " • %{error_to_string error}")
        |> std.string.join "\n"
      in
      {
        message = "%{init_msg}\n%{msgs}",
        source = [],
      },

  error_to_string
    | doc m%"
      Converts an error to a string representation, including the source.
    "%
    = fun error =>
      let location =
        if error.source == [] then
          ""
        else
          "at %{source_to_string error}: "
      in
      "%{location}%{error.message}",

  to_validator_result
    | doc m%"
      Converts a json-schema-to-nickel result type to the error type expected
      from validators in the Nickel stdlib.
    "%
    = match {
      'Ok => 'Ok,
      'Error error =>
        'Error {
          message = error_to_string error,
          notes = [],
        } | _,
    },

  mk_error
    | doc m%"
      Creates a fresh error with a given message and an empty source.
    "%
    = fun msg =>
      'Error {
        message = msg,
        source = [],
      },
}
# We have to type the top-level record if we want to be able to use this library
# in typed code elswhere
  : {
    at_field
      : String -> [|
        'Ok,
        'Error { message : String, source : Array [| 'Array Number, 'Field String |] }
      |] -> [|
        'Ok,
        'Error { message : String, source : Array [| 'Array Number, 'Field String |] }
      |],

    at_index
      : Number -> [|
        'Ok,
        'Error { message : String, source : Array [| 'Array Number, 'Field String |] }
      |] -> [|
        'Ok,
        'Error { message : String, source : Array [| 'Array Number, 'Field String |] }
      |],

    combine
      : String -> Array {
        message : String,
        source : Array [| 'Array Number, 'Field String |]
      } -> {
        message : String,
        source : Array [| 'Array Number, 'Field String |]
      },

    error_to_string
      : {
        message : String,
        source : Array [| 'Array Number, 'Field String |]
      } -> String,

    to_validator_result
      : [|
        'Ok,
        'Error { message : String, source : Array [| 'Array Number, 'Field String |] }
      |] -> [|
        'Ok,
        'Error { message | String | optional, notes | Array String | optional },
      |],

    source_to_string
      : {
        message : String,
        source : Array [| 'Array Number, 'Field String |]
      } -> String,

    mk_error
      : String -> [|
        'Ok,
        'Error { message : String, source : Array [| 'Array Number, 'Field String |] }
      |],
  }
