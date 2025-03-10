{
  Always
    | doc m%"
        A contract that always succeeds. The same as `Dyn`, but with a more JSON-schema name.
      "%
    = std.contract.from_predicate (fun x => true),

  Never = std.contract.from_predicate (fun x => false),

  one_of | Array Dyn -> Dyn = fun contracts =>
    std.contract.custom (fun label value =>
      let count_ok =
        contracts
        |> std.array.filter (fun ctr =>
          std.contract.check ctr label value
          |> match {
            'Ok _ => true,
            'Error _ => false
          }
        )
        |> std.array.length
      in
      if count_ok == 1 then
        'Ok value
      else
        'Error { message = "%{count_ok} of the contracts passed, expected precisely one to pass" }
    ),

  if_then_else | Dyn -> Dyn -> Dyn -> Dyn = fun iph zen els =>
    std.contract.custom (fun label value =>
      std.contract.check iph label value
      |> match {
        'Ok _ => std.contract.check zen label value,
        'Error _ => std.contract.check els label value,
      }
    ),

  enum
    | Array Dyn -> Dyn
    | doc m%"
      `enum values x` succeeds if and only if `x` is equal to one of the elements of `values`.
      Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.1.2]
    "%
    = fun variants =>
      std.contract.from_validator (fun value =>
        let checkEqual = fun input variant =>
          input == variant || (std.is_enum input && (std.string.from_enum input == variant | Bool))
        in
        if std.array.any (checkEqual value) variants then
          'Ok
        else
          'Error { message = "expected one of %{std.serialize 'Json variants}" }
      ),

  Nullable
    : Dyn -> Dyn
    | doc m%"
        For a contract `C`, `or_null C` returns a contract that allows either `C` or a null value.
      "%
    = fun Contract =>
      std.contract.custom (fun label value =>
        if value == null then
          'Ok value
        else
          std.contract.check Contract label value
      ),

  arrays = import "./arrays.ncl",
  records = import "./records.ncl",
  numbers = import "./numbers.ncl",
  strings = import "./strings.ncl",
}
