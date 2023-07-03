{
  description = "json-schema-to-nickel";

  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
    crane = {
      url = "github:ipetkov/crane";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nickel = {
      url = "github:tweag/nickel";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    topiary.url = "github:tweag/topiary";
  };

  outputs = inputs:
    let
      SYSTEMS = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];

      lib = inputs.nixpkgs.lib;
      foreach = xs: f: with lib; foldr recursiveUpdate { } (map f xs);
      forSystems = systems: f: foreach systems (system: f system inputs.nixpkgs.legacyPackages.${system});
    in
    forSystems SYSTEMS (system: pkgs:
      let
        craneLib = inputs.crane.mkLib pkgs;

        missingSysPkgs =
          if pkgs.stdenv.isDarwin then
            [
              pkgs.darwin.apple_sdk.frameworks.Security
              pkgs.darwin.libiconv
            ]
          else
            [ ];


        src =
          let
            mkFilter = regexp: path: _type: builtins.match regexp path != null;
          in
          lib.cleanSourceWith {
            src = lib.cleanSource (craneLib.path ./.);
            filter = path: type:
              builtins.any (filter: filter path type) [
                (mkFilter ".*/vendor/JSON-Schema-Test-Suite/tests/.*json$")
                (mkFilter ".*ncl$")
                craneLib.filterCargoSources
              ];
          };

        commonArgs = {
          inherit src;
          nativeBuildInputs = missingSysPkgs;
        };

        cargoArtifacts = craneLib.buildDepsOnly commonArgs;
        json-schema-to-nickel = craneLib.buildPackage (commonArgs // {
          inherit cargoArtifacts;
        });
      in
      {
        checks.${system} = {
          json-schema-to-nickel-clippy = craneLib.cargoClippy (commonArgs // {
            inherit cargoArtifacts;
            # `json_schema_test_suite` uses deprecated ways of
            # interacting with `mockito` in a macro expansion in `tests/json_schema_test_suite_test.rs`
            cargoClippyExtraArgs = "--all-targets -- --deny warnings --allow deprecated";
          });

          json-schema-to-nickel-fmt = craneLib.cargoFmt commonArgs;

          json-schema-to-nickel-test = craneLib.cargoTest (commonArgs // {
            inherit cargoArtifacts;
          });

          inherit (inputs.self.packages.${system}) json-schema-to-nickel;
        };

        packages.${system} = rec {
          inherit json-schema-to-nickel;
          default = json-schema-to-nickel;
        };

        devShells.${system}.default = pkgs.mkShell {
          inputsFrom = lib.attrValues inputs.self.checks.${system};
          packages = with pkgs; [
            cargo-watch
            inputs.topiary.packages.${system}.default
            inputs.nickel.packages.${system}.default
            rust-analyzer
            rustfmt
          ];
        };
      });
}
