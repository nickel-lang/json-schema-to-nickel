let predicates = import "./lib/predicates.ncl" in
let contract_from_predicate : (Dyn -> { success : Bool, error : String }) -> Dyn -> Dyn -> Dyn = fun predicate label value =>
    let { success, error } = predicate value
    in
    if success then
      value
    else
      (std.contract.blame_with_message error label | Dyn)
  in

let definitions = {
  MyEnum =
    predicates.oneOf
      [
        predicates.allOf
          [
            predicates.records.record
              {
                StringNewType = predicates.isType 'String
              }
              {}
              false
              predicates.never,
            predicates.records.required ["StringNewType"]
          ],
        predicates.allOf
          [
            predicates.records.record
              {
                floats = predicates.arrays.arrayOf (predicates.isType 'Number)
              }
              {}
              false
              predicates.never,
            predicates.records.required ["floats"]
          ],
        predicates.allOf
          [
            predicates.records.record
              {
                ints = predicates.arrays.arrayOf (predicates.isType 'Integer)
              }
              {}
              false
              predicates.never,
            predicates.records.required ["ints"]
          ]
      ]
}
in

let Schema = {
  my_bool | Bool,
  my_int | std.number.Integer,
  four | contract_from_predicate (predicates.const 4),
  my_json_enum
    | contract_from_predicate
      (predicates.enum [1, 42, "a string", null ]),
  my_nullable_enum
    | optional
    | contract_from_predicate
      (
        predicates.anyOf
          [
            definitions.MyEnum,
            predicates.isType 'Null
          ]
      )
}
in

{
  my_bool = true,
  my_int = 7,
  four = 4,
  my_json_enum = "a string",
  my_nullable_enum = {
    ints = [1, 2, 3]
  },
} | Schema

