# DO NOT EDIT
# This file was automatically generated using json-schema-to-nickel
let predicates = import "./lib/predicates.ncl" in
let rec definitions = {
  contract = {
    MyEnum =
      predicates.contract_from_predicate
        definitions.predicate.MyEnum,
    MyObject = { bar | Bool, foo | String | optional, },
  },
  predicate = {
    MyEnum =
      predicates.oneOf
        [
          predicates.allOf
            [
              predicates.isType 'Record,
              predicates.records.required ["StringNewType"],
              predicates.records.record
                { StringNewType = predicates.isType '"String", }
                {}
                false
                predicates.never
            ],
          predicates.allOf
            [
              predicates.isType 'Record,
              predicates.records.required ["StructVariant"],
              predicates.records.record
                {
                  StructVariant =
                    predicates.allOf
                      [
                        predicates.isType 'Record,
                        predicates.records.required ["floats"],
                        predicates.records.record
                          {
                            floats =
                              predicates.allOf
                                [
                                  predicates.isType '"Array",
                                  predicates.arrays.arrayOf
                                    (predicates.isType '"Number")
                                ],
                          }
                          {}
                          true
                          predicates.always
                      ],
                }
                {}
                false
                predicates.never
            ]
        ],
    MyObject =
      predicates.allOf
        [
          predicates.isType 'Record,
          predicates.records.required ["bar"],
          predicates.records.record
            {
              bar = predicates.isType '"Bool",
              foo = predicates.isType '"String",
            }
            {}
            false
            predicates.never
        ],
  },
}
in

{
  four | predicates.contract_from_predicate (predicates.const 4) | optional,
  my_bool | Bool,
  my_int | std.number.Integer,
  my_json_enum
    | predicates.contract_from_predicate
      (predicates.enum [1, 42, "a string", null ])
    | optional,
  my_nullable_enum
    | predicates.contract_from_predicate
      (predicates.anyOf [definitions.predicate.MyEnum, predicates.isType 'Null])
    | optional,
  my_object | definitions.contract.MyObject,
  my_reasonable_enum | std.enum.TagOrString | [| 'baz, 'bar, 'foo |] | optional,
  ..
}
