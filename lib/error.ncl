# Helpers for annotating error data
{
  at_field
    : String -> {
      message : String,
      source
        : Array [|
          'Array Number,
          'Field String
        |]
    } -> {
      message : String,
      source : Array [| 'Array Number, 'Field String |]
    }
    | doc "Attaches a given field name to the given error source"
    = fun field error =>
      {
        message = error.message,
        source =
          std.array.append
            ('Field field)
            error.source
      },

  at_index
    : Number -> {
      message : String,
      source
        : Array [|
          'Array Number,
          'Field String
        |]
    } -> { message : String, source : Array [| 'Array Number, 'Field String |] }
    | doc "Appends an array access to the given error source"
    = fun index error =>
      {
        message = error.message,
        source =
          std.array.append
            ('Array index)
            error.source
      },

  source_as_json_pointer
    : {
      message : String,
      source
        : Array [|
          'Array Number,
          'Field String
        |]
    } -> String
    | doc "Returns a JSON pointer corresponding to this error's source"
    = fun { source, .. } =>
      source
      |> std.array.map (match {
        'Array index => std.string.from_number index,
        'Field field => field,
      }
      )
      |> std.string.join "/",
}
