# This is an artisinal, opinionated github workflow contract to complement the
# auto-generated one. Currently, it's a subset (i.e. a stricter contract) that
# enforces some more idiomatic nickel conventions (like, less permissiveness
# with strings vs arrays of strings, less nulls, etc.). Eventually, we may also
# experiment with fancier things like ADTs.
#
# # No "matrix"
#
# Our first opinion (that we're testing out here) is lack of support for
# github's matrix. Let's instead try to generate multiple jobs natively
# in Nickel. One limitation that I've already encountered: matrix jobs
# get names like "build-and-test (ubuntu-latest, stable)", and we can't
# imitate names like these without a matrix because spaces are disallowed
# in job names.
let
  WorkflowExport = import "generated-workflow.ncl",
  util = import "util.ncl",
in
{
  Job = {
    runs-on | String,
    continue-on-error | Bool | default = false,
    steps | Array Step | util.UniqueNames,
  },

  # TODO: make branches / branches-ignore mutually exclusive
  Ref = {
    branches | Array String | optional,
    branches-ignore | Array String | optional,
    paths | Array String | optional,
    paths-ignore | Array String | optional,
    tags | Array String | optional,
    tags-ignore | Array String | optional,
  },

  Step = {
    name | String,
    run | String | optional,
    uses | String | optional,
    with | { _ | String } | optional,
  },

  Trigger = {
    pull_request | Ref | optional,
    push | Ref | optional,
    merge_group | {} | optional
  },

  Workflow =
    {
      name | String,
      on | Trigger,
      jobs | { _ | Job },
    } | WorkflowExport,
}
