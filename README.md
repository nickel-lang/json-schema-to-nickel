# json-schema-to-nickel

A tool to convert [JSON schema](https://json-schema.org) into [Nickel](https://www.nickel-lang.org) contracts.

## How to use it?

`json-schema-to-nickel` is a standalone binary that reads a JSON schema file and
outputs a Nickel contract. You can run it from source using `cargo` by running

```shell
cargo run -- <path to your schema.json>
```

in a checkout of this repository. Alternatively, you can use `nix`:

```shell
nix run github:nickel-lang/json-schema-to-nickel -- <path to your schema.json>
```

anywhere. This will retrieve the latest revision from GitHub, build
`json-schema-to-nickel` and run the resulting binary on your JSON schema.

Either invocation will print the generated Nickel contract to standard
output. It can get rather large, so it's recommended to redirect the output into
a file, for example by using

```shell
cargo run -- schema.json > contract.ncl
```

Once you have a output of `json-schema-to-nickel` in a file, say `contract.ncl`,
you can check a Nickel configuration against it by importing the file by its
path, e.g.

```nickel
let
  SchemaContract = import "./contract.ncl"
in {
  foo = "bar"
} | SchemaContract
```

## How it works

A JSON schema encodes a predicate on JSON objects, that is, a function from
JSON objects returning `true` or `false`. Such a function can always be written
as a Nickel contract and a big part of `json-schema-to-nickel` is mechanically
generating such a contract using a [Nickel library](./lib/main.ncl).
General JSON schema predicates cannot be converted to idiomatic Nickel contracts
because JSON schema assumes eager evaluation (especially because of the
[`anyOf`][union-contracts] and similar keywords) and most Nickel contracts are
lazy. The second major part of `json-schema-to-nickel` is figuring out when a
JSON schema can be represented as a lazy Nickel contract, and generating this
contract when it makes sense.

Lazy record contracts, when they can be produced, are much more
useful for code inspection tools. For example, the Nickel language server can
offer completion information based on record contracts and `nickel doc` can
extract structured documentation. When only a predicate check can be generated
these niceties are lost.

Our approach for generating record contracts is certainly not optimal, yet.
There are JSON schema predicates that we could reasonably convert into lazy
contracts which we're currently missing. But the end goal is to convert JSON
schema into contracts that are as lazy and inspectable for the LSP as possible,
and fall back to a strict predicate check only when absolutely necessary.

## Limitations

- JSON schema comes in many dialects. We currently support draft 7, which is the
  most widely used version. For example, it is the one supported by
  [SchemaStore][schemastore].
- The `format` keyword of JSON schema is currently ignored, [#24][i24]
- Error reporting in contracts converted from predicates can be a bit hit or miss, [#27][i27]
- The generated contracts might be more eager than expected. This chiefly
  depends on whether our heuristics are able to produce a proper Nickel record
  contract or not. If they do, the top level contract will be as lazy as
  expected in Nickel, otherwise the result will just be a validation function.
  Similar reasoning applies to each field of a record contract if one can be
  generated.
- Related to the last point, if our contract heuristics produce a proper record
  contract, completion using the Nickel language server will work as expected.
  Otherwise, the generated contract will be opaque to the language server.
  One approach to improve this situation could be to generate record contracts
  that are more tolerant than the JSON schema specifies and then refine those
  contracts with an accurate predicate check, [i25]

[i24]: https://github.com/nickel-lang/json-schema-to-nickel/issues/24
[i25]: https://github.com/nickel-lang/json-schema-to-nickel/issues/25
[i27]: https://github.com/nickel-lang/json-schema-to-nickel/issues/27
[schemastore]: https://github.com/SchemaStore/schemastore
[union-contracts]: https://www.tweag.io/blog/2022-04-28-union-intersection-contracts/
