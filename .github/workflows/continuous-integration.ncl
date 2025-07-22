# This file configures our github workflows, but in order for changes to take
# effect, it needs to be exported to yaml and the yaml changes need to be
# committed.
#
# Run `nix run .#generate-ci` to generate the yaml.
let workflow = import "workflow.ncl"
in
let rec
  checkout | workflow.Step = {
    name = "Checking out the repository",
    uses = "actions/checkout@v4",
    with = {
      fetch-depth = "0",
    },
  },
  nix | workflow.Step = {
    name = "Installing Nix",
    uses = "cachix/install-nix-action@v31",
    with = {
      nix_path = "nixpkgs=channel:nixos-unstable",
      extra_nix_config = m%"
        experimental-features = nix-command flakes
        accept-flake-config = true
      "%,
    }
  },
  flake_check | workflow.Step = {
    name = "Run all checks",
    run = "nix flake check --print-build-logs",
  },
  check_steps = [checkout, nix, flake_check],
in
{
  name = "Continuous integration",
  on = {
    push = { branches = ["master"] },
    pull_request = {},
    merge_group = {},
  },

  jobs = {
    "build-and-test-linux" = {
      runs-on = "ubuntu-latest",
      steps = check_steps,
    },
    "build-and-test-macos" = {
      runs-on = "macos-latest",
      steps = check_steps,
    }
  },
} | workflow.Workflow
