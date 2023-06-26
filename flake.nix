{
  description = "json-schema-to-nickel";

  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
    naersk = {
      url = "github:nix-community/naersk";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nickel = {
      url = "github:tweag/nickel";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "utils";
      };
    };

    topiary.url = "github:tweag/topiary";
  };

  outputs = { self, nixpkgs, utils, naersk, nickel, ... }@inputs: {
    overlays = rec {
      expects-naersk = final: _: {
        json-schema-to-nickel = final.naersk.buildPackage {
          pname = "json-schema-to-nickel";
          root = ./.;
        };
      };

      default = final: _: {
        inherit (final.appendOverlays [
          naersk.overlay
          expects-naersk
        ]) json-schema-to-nickel;
      };
    };
  } // utils.lib.eachDefaultSystem (system: with import nixpkgs
    { overlays = [ self.overlays.default ]; inherit system; }; {
    packages.default = json-schema-to-nickel;

    devShells.default = mkShell {
      packages = [
        cargo
        cargo-watch
        clippy
        inputs.topiary.packages.${system}.default
        nickel.packages.${system}.default
        rust-analyzer
        rustc
        rustfmt
      ];
    };
  });
}
