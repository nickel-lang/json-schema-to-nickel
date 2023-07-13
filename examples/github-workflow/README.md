# GitHub Workflow syntax validation

This is a small example of using a published JSON schema, namely the [GitHub
Workflow schema](https://json.schemastore.org/github-workflow.json). The top-
level configuration in `ci.ncl` is using some Nickel programming features to
generate a GitHub workflow specification. The result is then checked against the
JSON schema to ensure no typos have snuck in.

To generate the workflow file use `nickel export -f ci.ncl > ci.yaml`.
