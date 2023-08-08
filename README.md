# json-schema-to-nickel

A tool to convert [JSON schema](https://json-schema.org) into [Nickel](https://www.nickel-lang.org) contracts.

## How to use it?

`json-schema-to-nickel` is a standalone binary that reads a JSON schema file and
outputs a Nickel contract. You can build it from source using `cargo` by running

```shell
cargo build --release
```

in a checkout of this repository. The resulting binary will be `./target/
release/json-schema-to-nickel` and can also be run using `cargo run --release`.
Alternatively, you can build it using `nix` by running

```shell
nix build github:nickel-lang/json-schema-to-nickel
```

anywhere. This will retrieve the latest revision from GitHub, build `json-
schema-to-nickel` and produce a symlink `./result/bin/json-schema-to-nickel` in
the current directory.

Once you have a binary, to convert a JSON schema in `schema.json` into a Nickel
contract, use

```shell
json-schema-to-nickel schema.json > contract.ncl
```

`json-schema-to-nickel` will print the generated Nickel contract on standard
output. It can get rather large, so it's recommended to redirect the output into
a file as illustrated above. Once you have a Nickel contract in `contract.ncl`
you can check a Nickel configuration against by simply importing the file by its
path, e.g.

```nickel
let
  SchemaContract = import "./contract.ncl"
in {
  foo = "bar"
} | SchemaContract
```

## How it works

We're using the [schemars][schemars] crate to parse
JSON schema. A JSON schema encodes a predicate on JSON objects, that is, a
function from JSON objects returning `true` or `false`. Such a function can also
be written in Nickel and a big part of `json-schema-to-nickel` is mechanically
generating such a predicate using a [Nickel library](./lib/predicates.ncl).
However, such predicates are not ideal for use in Nickel and many aspects
of JSON schema can be encoded as Nickel record contracts. The second major
part of `json-schema-to-nickel` is figuring out when a JSON schema can be
represented as a record contract, and generating this contract when it makes
sense. Finally, there is some housekeeping to do for dispatching between these
two functionalities.

## Limitations

- JSON schema comes in many dialects. We rely on [schemars][schemars] to
  interpret schemas and this way should support any draft specification and
  dialect that [schemars][schemars] does.
- The `format` keyword of JSON schema is currently ignored, [#24][i24]
- Error reporting in contracts converted from predicates can be a bit hit or miss, [#27][i27]

[i24]: https://github.com/nickel-lang/json-schema-to-nickel/issues/24
[i27]: https://github.com/nickel-lang/json-schema-to-nickel/issues/27
[schemars]: https://crates.io/crates/schemars
