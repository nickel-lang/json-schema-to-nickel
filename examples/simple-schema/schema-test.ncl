let Schema = import "./test.schema.ncl" in
{
  my_bool = true,
  my_int = 7,
  four = 4,
  my_json_enum = "a string",
  my_object = {
    foo = "foo",
    bar = false,
  },
  my_nullable_enum = {
    StructVariant = {
      floats = [1, 2, 3]
    }
  },
} | Schema

