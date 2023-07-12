let predicates = import "./lib/predicates.ncl" in
let rec definitions = {
  contract = {
    architecture =
      predicates.contract_from_predicate
        definitions.predicate.architecture,
    branch =
      predicates.contract_from_predicate
        definitions.predicate.branch,
    concurrency = {
      cancel-in-progress
        | predicates.contract_from_predicate
          (
            predicates.oneOf
              [
                predicates.isType '"Bool",
                definitions.predicate.expressionSyntax
              ]
          )
        | optional,
      group
        | predicates.contract_from_predicate
          (predicates.isType 'String),
    },
    configuration =
      predicates.contract_from_predicate
        definitions.predicate.configuration,
    container = {
      credentials
        | predicates.contract_from_predicate
          (
            predicates.allOf
              [
                predicates.isType 'Record,
                predicates.records.record
                  {
                    password = predicates.isType 'String,
                    username = predicates.isType 'String,
                  }
                  {}
                  true
                  predicates.always
              ]
          )
        | optional,
      env
        | predicates.contract_from_predicate definitions.predicate.env
        | optional,
      image
        | predicates.contract_from_predicate
          (predicates.isType 'String),
      options
        | predicates.contract_from_predicate
          (predicates.isType 'String)
        | optional,
      ports
        | predicates.contract_from_predicate
          (
            predicates.allOf
              [
                predicates.isType '"Array",
                predicates.arrays.arrayOf
                  (
                    predicates.oneOf
                      [predicates.isType 'Number, predicates.isType 'String]
                  ),
                predicates.arrays.minItems 1
              ]
          )
        | optional,
      volumes
        | predicates.contract_from_predicate
          (
            predicates.allOf
              [
                predicates.isType '"Array",
                predicates.arrays.arrayOf
                  (
                    predicates.allOf
                      [
                        predicates.isType 'String,
                        predicates.strings.pattern "^[^:]+:[^:]+$"
                      ]
                  ),
                predicates.arrays.minItems 1
              ]
          )
        | optional,
    },
    defaults =
      predicates.contract_from_predicate
        definitions.predicate.defaults,
    env = predicates.contract_from_predicate definitions.predicate.env,
    environment =
      predicates.contract_from_predicate
        definitions.predicate.environment,
    event =
      predicates.contract_from_predicate
        definitions.predicate.event,
    eventObject =
      predicates.contract_from_predicate
        definitions.predicate.eventObject,
    expressionSyntax =
      predicates.contract_from_predicate
        definitions.predicate.expressionSyntax,
    globs =
      predicates.contract_from_predicate
        definitions.predicate.globs,
    jobNeeds =
      predicates.contract_from_predicate
        definitions.predicate.jobNeeds,
    machine =
      predicates.contract_from_predicate
        definitions.predicate.machine,
    name = predicates.contract_from_predicate definitions.predicate.name,
    normalJob =
      predicates.contract_from_predicate
        definitions.predicate.normalJob,
    path = predicates.contract_from_predicate definitions.predicate.path,
    permissions =
      predicates.contract_from_predicate
        definitions.predicate.permissions,
    permissions-event = {
      actions | definitions.contract.permissions-level | optional,
      checks | definitions.contract.permissions-level | optional,
      contents | definitions.contract.permissions-level | optional,
      deployments | definitions.contract.permissions-level | optional,
      discussions | definitions.contract.permissions-level | optional,
      id-token | definitions.contract.permissions-level | optional,
      issues | definitions.contract.permissions-level | optional,
      packages | definitions.contract.permissions-level | optional,
      pages | definitions.contract.permissions-level | optional,
      pull-requests
        | definitions.contract.permissions-level
        | optional,
      repository-projects
        | definitions.contract.permissions-level
        | optional,
      security-events
        | definitions.contract.permissions-level
        | optional,
      statuses | definitions.contract.permissions-level | optional,
    },
    permissions-level =
      predicates.contract_from_predicate
        definitions.predicate.permissions-level,
    ref = predicates.contract_from_predicate definitions.predicate.ref,
    reusableWorkflowCallJob =
      predicates.contract_from_predicate
        definitions.predicate.reusableWorkflowCallJob,
    shell =
      predicates.contract_from_predicate
        definitions.predicate.shell,
    stringContainingExpressionSyntax =
      predicates.contract_from_predicate
        definitions.predicate.stringContainingExpressionSyntax,
    types =
      predicates.contract_from_predicate
        definitions.predicate.types,
    working-directory =
      predicates.contract_from_predicate
        definitions.predicate.working-directory,
  },
  predicate = {
    architecture =
      predicates.allOf
        [
          predicates.isType 'String,
          predicates.enum ["ARM32", "x64", "x86"]
        ],
    branch = definitions.predicate.globs,
    concurrency =
      predicates.allOf
        [
          predicates.isType 'Record,
          predicates.records.required ["group"],
          predicates.records.record
            {
              cancel-in-progress =
                predicates.oneOf
                  [
                    predicates.isType '"Bool",
                    definitions.predicate.expressionSyntax
                  ],
              group = predicates.isType 'String,
            }
            {}
            false
            predicates.never
        ],
    configuration =
      predicates.oneOf
        [
          predicates.isType 'String,
          predicates.isType 'Number,
          predicates.isType '"Bool",
          predicates.allOf
            [
              predicates.isType 'Record,
              predicates.records.record
                {}
                {}
                true
                definitions.predicate.configuration
            ],
          predicates.allOf
            [
              predicates.isType '"Array",
              predicates.arrays.arrayOf definitions.predicate.configuration
            ]
        ],
    container =
      predicates.allOf
        [
          predicates.isType 'Record,
          predicates.records.required ["image"],
          predicates.records.record
            {
              credentials =
                predicates.allOf
                  [
                    predicates.isType 'Record,
                    predicates.records.record
                      {
                        password = predicates.isType 'String,
                        username = predicates.isType 'String,
                      }
                      {}
                      true
                      predicates.always
                  ],
              env = definitions.predicate.env,
              image = predicates.isType 'String,
              options = predicates.isType 'String,
              ports =
                predicates.allOf
                  [
                    predicates.isType '"Array",
                    predicates.arrays.arrayOf
                      (
                        predicates.oneOf
                          [
                            predicates.isType 'Number,
                            predicates.isType 'String
                          ]
                      ),
                    predicates.arrays.minItems 1
                  ],
              volumes =
                predicates.allOf
                  [
                    predicates.isType '"Array",
                    predicates.arrays.arrayOf
                      (
                        predicates.allOf
                          [
                            predicates.isType 'String,
                            predicates.strings.pattern "^[^:]+:[^:]+$"
                          ]
                      ),
                    predicates.arrays.minItems 1
                  ],
            }
            {}
            false
            predicates.never
        ],
    defaults =
      predicates.allOf
        [
          predicates.isType 'Record,
          predicates.records.minProperties 1,
          predicates.records.record
            {
              run =
                predicates.allOf
                  [
                    predicates.isType 'Record,
                    predicates.records.minProperties 1,
                    predicates.records.record
                      {
                        shell = definitions.predicate.shell,
                        working-directory =
                          definitions.predicate.working-directory,
                      }
                      {}
                      false
                      predicates.never
                  ],
            }
            {}
            false
            predicates.never
        ],
    env =
      predicates.oneOf
        [
          predicates.allOf
            [
              predicates.isType 'Record,
              predicates.records.record
                {}
                {}
                true
                (
                  predicates.oneOf
                    [
                      predicates.isType 'String,
                      predicates.isType 'Number,
                      predicates.isType '"Bool"
                    ]
                )
            ],
          definitions.predicate.stringContainingExpressionSyntax
        ],
    environment =
      predicates.allOf
        [
          predicates.isType 'Record,
          predicates.records.required ["name"],
          predicates.records.record
            {
              name = predicates.isType 'String,
              url = predicates.isType 'String,
            }
            {}
            false
            predicates.never
        ],
    event =
      predicates.allOf
        [
          predicates.isType 'String,
          predicates.enum
            [
              "branch_protection_rule",
              "check_run",
              "check_suite",
              "create",
              "delete",
              "deployment",
              "deployment_status",
              "discussion",
              "discussion_comment",
              "fork",
              "gollum",
              "issue_comment",
              "issues",
              "label",
              "member",
              "milestone",
              "page_build",
              "project",
              "project_card",
              "project_column",
              "public",
              "pull_request",
              "pull_request_review",
              "pull_request_review_comment",
              "pull_request_target",
              "push",
              "registry_package",
              "release",
              "status",
              "watch",
              "workflow_call",
              "workflow_dispatch",
              "workflow_run",
              "repository_dispatch"
            ]
        ],
    eventObject =
      predicates.allOf
        [
          predicates.oneOf
            [predicates.isType 'Record, predicates.isType 'Null],
          predicates.records.record {} {} true predicates.always
        ],
    expressionSyntax =
      predicates.allOf
        [
          predicates.isType 'String,
          predicates.strings.pattern "^\\$\\{\\{(.|[\n])*\\}\\}$"
        ],
    globs =
      predicates.allOf
        [
          predicates.isType '"Array",
          predicates.arrays.arrayOf
            (
              predicates.allOf
                [predicates.isType 'String, predicates.strings.minLength 1]
            ),
          predicates.arrays.minItems 1
        ],
    jobNeeds =
      predicates.oneOf
        [
          predicates.allOf
            [
              predicates.isType '"Array",
              predicates.arrays.arrayOf definitions.predicate.name,
              predicates.arrays.minItems 1
            ],
          definitions.predicate.name
        ],
    machine =
      predicates.allOf
        [
          predicates.isType 'String,
          predicates.enum ["linux", "macos", "windows"]
        ],
    name =
      predicates.allOf
        [
          predicates.isType 'String,
          predicates.strings.pattern "^[_a-zA-Z][a-zA-Z0-9_-]*$"
        ],
    normalJob =
      predicates.allOf
        [
          predicates.isType 'Record,
          predicates.records.required ["runs-on"],
          predicates.records.record
            {
              concurrency =
                predicates.oneOf
                  [
                    predicates.isType 'String,
                    definitions.predicate.concurrency
                  ],
              container =
                predicates.oneOf
                  [
                    predicates.isType 'String,
                    definitions.predicate.container
                  ],
              continue-on-error =
                predicates.oneOf
                  [
                    predicates.isType '"Bool",
                    definitions.predicate.expressionSyntax
                  ],
              defaults = definitions.predicate.defaults,
              env = definitions.predicate.env,
              environment =
                predicates.oneOf
                  [
                    predicates.isType 'String,
                    definitions.predicate.environment
                  ],
              "if" =
                predicates.anyOf
                  [
                    predicates.isType '"Bool",
                    predicates.isType 'Number,
                    predicates.isType 'String
                  ],
              name = predicates.isType 'String,
              needs = definitions.predicate.jobNeeds,
              outputs =
                predicates.allOf
                  [
                    predicates.isType 'Record,
                    predicates.records.minProperties 1,
                    predicates.records.record
                      {}
                      {}
                      true
                      (predicates.isType 'String)
                  ],
              permissions = definitions.predicate.permissions,
              runs-on =
                predicates.oneOf
                  [
                    predicates.allOf
                      [
                        predicates.isType 'String,
                        predicates.enum
                          [
                            "macos-10.15",
                            "macos-11",
                            "macos-12",
                            "macos-12-xl",
                            "macos-13",
                            "macos-13-xl",
                            "macos-latest",
                            "macos-latest-xl",
                            "self-hosted",
                            "ubuntu-18.04",
                            "ubuntu-20.04",
                            "ubuntu-22.04",
                            "ubuntu-latest",
                            "ubuntu-latest-4-cores",
                            "ubuntu-latest-8-cores",
                            "ubuntu-latest-16-cores",
                            "windows-2019",
                            "windows-2022",
                            "windows-latest",
                            "windows-latest-8-cores"
                          ]
                      ],
                    predicates.allOf
                      [
                        predicates.isType '"Array",
                        predicates.anyOf
                          [
                            predicates.allOf
                              [
                                predicates.arrays.items
                                  [predicates.const "self-hosted"],
                                predicates.arrays.additionalItems
                                  (predicates.isType 'String)
                                  1,
                                predicates.arrays.minItems 1
                              ],
                            predicates.allOf
                              [
                                predicates.arrays.items
                                  [
                                    predicates.const "self-hosted",
                                    definitions.predicate.machine
                                  ],
                                predicates.arrays.additionalItems
                                  (predicates.isType 'String)
                                  2,
                                predicates.arrays.minItems 2
                              ],
                            predicates.allOf
                              [
                                predicates.arrays.items
                                  [
                                    predicates.const "self-hosted",
                                    definitions.predicate.architecture
                                  ],
                                predicates.arrays.additionalItems
                                  (predicates.isType 'String)
                                  2,
                                predicates.arrays.minItems 2
                              ],
                            predicates.allOf
                              [
                                predicates.arrays.items
                                  [
                                    predicates.const "self-hosted",
                                    definitions.predicate.machine,
                                    definitions.predicate.architecture
                                  ],
                                predicates.arrays.additionalItems
                                  (predicates.isType 'String)
                                  3,
                                predicates.arrays.minItems 3
                              ],
                            predicates.allOf
                              [
                                predicates.arrays.items
                                  [
                                    predicates.const "self-hosted",
                                    definitions.predicate.architecture,
                                    definitions.predicate.machine
                                  ],
                                predicates.arrays.additionalItems
                                  (predicates.isType 'String)
                                  3,
                                predicates.arrays.minItems 3
                              ],
                            predicates.allOf
                              [
                                predicates.arrays.items
                                  [predicates.const "linux"],
                                predicates.arrays.additionalItems
                                  (predicates.isType 'String)
                                  1,
                                predicates.arrays.maxItems 2,
                                predicates.arrays.minItems 2
                              ],
                            predicates.allOf
                              [
                                predicates.arrays.items
                                  [predicates.const "windows"],
                                predicates.arrays.additionalItems
                                  (predicates.isType 'String)
                                  1,
                                predicates.arrays.maxItems 2,
                                predicates.arrays.minItems 2
                              ]
                          ]
                      ],
                    predicates.allOf
                      [
                        predicates.isType 'Record,
                        predicates.records.record
                          {
                            group = predicates.isType 'String,
                            labels =
                              predicates.oneOf
                                [
                                  predicates.isType 'String,
                                  predicates.allOf
                                    [
                                      predicates.isType '"Array",
                                      predicates.arrays.arrayOf
                                        (predicates.isType 'String)
                                    ]
                                ],
                          }
                          {}
                          true
                          predicates.always
                      ],
                    definitions.predicate.stringContainingExpressionSyntax
                  ],
              services =
                predicates.allOf
                  [
                    predicates.isType 'Record,
                    predicates.records.record
                      {}
                      {}
                      true
                      definitions.predicate.container
                  ],
              steps =
                predicates.allOf
                  [
                    predicates.isType '"Array",
                    predicates.arrays.arrayOf
                      (
                        predicates.allOf
                          [
                            predicates.oneOf
                              [
                                predicates.allOf
                                  [
                                    predicates.isType 'Record,
                                    predicates.records.required ["uses"],
                                    predicates.records.record
                                      { uses = predicates.isType 'String, }
                                      {}
                                      true
                                      predicates.always
                                  ],
                                predicates.allOf
                                  [
                                    predicates.isType 'Record,
                                    predicates.records.required ["run"],
                                    predicates.records.record
                                      { run = predicates.isType 'String, }
                                      {}
                                      true
                                      predicates.always
                                  ]
                              ],
                            predicates.allOf
                              [
                                predicates.isType 'Record,
                                predicates.records.record
                                  {
                                    continue-on-error =
                                      predicates.oneOf
                                        [
                                          predicates.isType '"Bool",
                                          definitions.predicate.expressionSyntax
                                        ],
                                    env = definitions.predicate.env,
                                    id = predicates.isType 'String,
                                    "if" =
                                      predicates.anyOf
                                        [
                                          predicates.isType '"Bool",
                                          predicates.isType 'Number,
                                          predicates.isType 'String
                                        ],
                                    name = predicates.isType 'String,
                                    run = predicates.isType 'String,
                                    shell = definitions.predicate.shell,
                                    timeout-minutes =
                                      predicates.oneOf
                                        [
                                          predicates.isType 'Number,
                                          definitions.predicate.expressionSyntax
                                        ],
                                    uses = predicates.isType 'String,
                                    with =
                                      predicates.allOf
                                        [
                                          predicates.records.record
                                            {
                                              args = predicates.isType 'String,
                                              entrypoint = predicates.isType 'String,
                                            }
                                            {}
                                            true
                                            predicates.always,
                                          definitions.predicate.env
                                        ],
                                    working-directory =
                                      definitions.predicate.working-directory,
                                  }
                                  {}
                                  false
                                  predicates.never,
                                predicates.records.dependencies
                                  {
                                    shell = ["run"],
                                    working-directory = ["run"],
                                  }
                              ]
                          ]
                      ),
                    predicates.arrays.minItems 1
                  ],
              strategy =
                predicates.allOf
                  [
                    predicates.isType 'Record,
                    predicates.records.required ["matrix"],
                    predicates.records.record
                      {
                        fail-fast = predicates.isType '"Bool",
                        matrix =
                          predicates.allOf
                            [
                              predicates.oneOf
                                [
                                  predicates.isType 'Record,
                                  definitions.predicate.expressionSyntax
                                ],
                              predicates.records.minProperties 1,
                              predicates.records.record
                                {}
                                {
                                  "^(in|ex)clude$" =
                                    predicates.allOf
                                      [
                                        predicates.isType '"Array",
                                        predicates.arrays.arrayOf
                                          (
                                            predicates.allOf
                                              [
                                                predicates.isType 'Record,
                                                predicates.records.record
                                                  {}
                                                  {}
                                                  true
                                                  definitions.predicate.configuration
                                              ]
                                          ),
                                        predicates.arrays.minItems 1
                                      ],
                                }
                                true
                                (
                                  predicates.oneOf
                                    [
                                      predicates.allOf
                                        [
                                          predicates.isType '"Array",
                                          predicates.arrays.arrayOf
                                            definitions.predicate.configuration,
                                          predicates.arrays.minItems 1
                                        ],
                                      definitions.predicate.expressionSyntax
                                    ]
                                )
                            ],
                        max-parallel =
                          predicates.anyOf
                            [
                              predicates.isType 'Number,
                              predicates.isType 'String
                            ],
                      }
                      {}
                      false
                      predicates.never
                  ],
              timeout-minutes =
                predicates.oneOf
                  [
                    predicates.isType 'Number,
                    definitions.predicate.expressionSyntax
                  ],
            }
            {}
            false
            predicates.never
        ],
    path = definitions.predicate.globs,
    permissions =
      predicates.oneOf
        [
          predicates.allOf
            [
              predicates.isType 'String,
              predicates.enum ["read-all", "write-all"]
            ],
          definitions.predicate.permissions-event
        ],
    permissions-event =
      predicates.allOf
        [
          predicates.isType 'Record,
          predicates.records.record
            {
              actions = definitions.predicate.permissions-level,
              checks = definitions.predicate.permissions-level,
              contents = definitions.predicate.permissions-level,
              deployments = definitions.predicate.permissions-level,
              discussions = definitions.predicate.permissions-level,
              id-token = definitions.predicate.permissions-level,
              issues = definitions.predicate.permissions-level,
              packages = definitions.predicate.permissions-level,
              pages = definitions.predicate.permissions-level,
              pull-requests = definitions.predicate.permissions-level,
              repository-projects = definitions.predicate.permissions-level,
              security-events = definitions.predicate.permissions-level,
              statuses = definitions.predicate.permissions-level,
            }
            {}
            false
            predicates.never
        ],
    permissions-level =
      predicates.allOf
        [
          predicates.isType 'String,
          predicates.enum ["read", "write", "none"]
        ],
    ref =
      predicates.allOf
        [
          predicates.oneOf
            [
              predicates.allOf
                [
                  predicates.isType 'Record,
                  predicates.allOf
                    [
                      predicates.not
                        (
                          predicates.allOf
                            [
                              predicates.records.required
                                ["branches", "branches-ignore"],
                              predicates.records.record
                                {}
                                {}
                                true
                                predicates.always
                            ]
                        ),
                      predicates.not
                        (
                          predicates.allOf
                            [
                              predicates.records.required ["tags", "tags-ignore"],
                              predicates.records.record
                                {}
                                {}
                                true
                                predicates.always
                            ]
                        ),
                      predicates.not
                        (
                          predicates.allOf
                            [
                              predicates.records.required ["paths", "paths-ignore"],
                              predicates.records.record
                                {}
                                {}
                                true
                                predicates.always
                            ]
                        )
                    ]
                ],
              predicates.isType 'Null
            ],
          predicates.records.record
            {
              branches = definitions.predicate.branch,
              branches-ignore = definitions.predicate.branch,
              paths = definitions.predicate.path,
              paths-ignore = definitions.predicate.path,
              tags = definitions.predicate.branch,
              tags-ignore = definitions.predicate.branch,
            }
            {}
            true
            predicates.always
        ],
    reusableWorkflowCallJob =
      predicates.allOf
        [
          predicates.isType 'Record,
          predicates.records.required ["uses"],
          predicates.records.record
            {
              concurrency =
                predicates.oneOf
                  [
                    predicates.isType 'String,
                    definitions.predicate.concurrency
                  ],
              "if" =
                predicates.anyOf
                  [
                    predicates.isType '"Bool",
                    predicates.isType 'Number,
                    predicates.isType 'String
                  ],
              name = predicates.isType 'String,
              needs = definitions.predicate.jobNeeds,
              permissions = definitions.predicate.permissions-event,
              secrets =
                predicates.oneOf
                  [
                    definitions.predicate.env,
                    predicates.allOf
                      [
                        predicates.isType 'String,
                        predicates.enum ["inherit"]
                      ]
                  ],
              strategy =
                predicates.allOf
                  [
                    predicates.isType 'Record,
                    predicates.records.required ["matrix"],
                    predicates.records.record
                      {
                        fail-fast = predicates.isType '"Bool",
                        matrix =
                          predicates.allOf
                            [
                              predicates.oneOf
                                [
                                  predicates.isType 'Record,
                                  definitions.predicate.expressionSyntax
                                ],
                              predicates.records.minProperties 1,
                              predicates.records.record
                                {}
                                {
                                  "^(in|ex)clude$" =
                                    predicates.allOf
                                      [
                                        predicates.isType '"Array",
                                        predicates.arrays.arrayOf
                                          (
                                            predicates.allOf
                                              [
                                                predicates.isType 'Record,
                                                predicates.records.record
                                                  {}
                                                  {}
                                                  true
                                                  definitions.predicate.configuration
                                              ]
                                          ),
                                        predicates.arrays.minItems 1
                                      ],
                                }
                                true
                                (
                                  predicates.oneOf
                                    [
                                      predicates.allOf
                                        [
                                          predicates.isType '"Array",
                                          predicates.arrays.arrayOf
                                            definitions.predicate.configuration,
                                          predicates.arrays.minItems 1
                                        ],
                                      definitions.predicate.expressionSyntax
                                    ]
                                )
                            ],
                        max-parallel =
                          predicates.anyOf
                            [
                              predicates.isType 'Number,
                              predicates.isType 'String
                            ],
                      }
                      {}
                      false
                      predicates.never
                  ],
              uses =
                predicates.allOf
                  [
                    predicates.isType 'String,
                    predicates.strings.pattern
                      "^(.+/)+(.+)\\.(ya?ml)(@.+)?$"
                  ],
              with = definitions.predicate.env,
            }
            {}
            false
            predicates.never
        ],
    shell =
      predicates.anyOf
        [
          predicates.isType 'String,
          predicates.allOf
            [
              predicates.isType 'String,
              predicates.enum
                ["bash", "pwsh", "python", "sh", "cmd", "powershell"]
            ]
        ],
    stringContainingExpressionSyntax =
      predicates.allOf
        [
          predicates.isType 'String,
          predicates.strings.pattern "^.*\\$\\{\\{(.|[\n])*\\}\\}.*$"
        ],
    types =
      predicates.allOf
        [predicates.isType '"Array", predicates.arrays.minItems 1],
    working-directory = predicates.isType 'String,
  },
}
in

predicates.contract_from_predicate
  (
    predicates.allOf
      [
        predicates.isType 'Record,
        predicates.records.required ["jobs", "on"],
        predicates.records.record
          {
            concurrency =
              predicates.oneOf
                [predicates.isType 'String, definitions.predicate.concurrency],
            defaults = definitions.predicate.defaults,
            env = definitions.predicate.env,
            jobs =
              predicates.allOf
                [
                  predicates.isType 'Record,
                  predicates.records.minProperties 1,
                  predicates.records.record
                    {}
                    {
                      "^[_a-zA-Z][a-zA-Z0-9_-]*$" =
                        predicates.oneOf
                          [
                            definitions.predicate.normalJob,
                            definitions.predicate.reusableWorkflowCallJob
                          ],
                    }
                    false
                    predicates.never
                ],
            name = predicates.isType 'String,
            on =
              predicates.oneOf
                [
                  definitions.predicate.event,
                  predicates.allOf
                    [
                      predicates.isType '"Array",
                      predicates.arrays.arrayOf definitions.predicate.event,
                      predicates.arrays.minItems 1
                    ],
                  predicates.allOf
                    [
                      predicates.isType 'Record,
                      predicates.records.record
                        {
                          branch_protection_rule =
                            predicates.allOf
                              [
                                predicates.records.record
                                  {
                                    types =
                                      predicates.allOf
                                        [
                                          predicates.arrays.arrayOf
                                            (
                                              predicates.allOf
                                                [
                                                  predicates.isType 'String,
                                                  predicates.enum ["created", "edited", "deleted"]
                                                ]
                                            ),
                                          definitions.predicate.types
                                        ],
                                  }
                                  {}
                                  true
                                  predicates.always,
                                definitions.predicate.eventObject
                              ],
                          check_run =
                            predicates.allOf
                              [
                                predicates.records.record
                                  {
                                    types =
                                      predicates.allOf
                                        [
                                          predicates.arrays.arrayOf
                                            (
                                              predicates.allOf
                                                [
                                                  predicates.isType 'String,
                                                  predicates.enum
                                                    [
                                                      "created",
                                                      "rerequested",
                                                      "completed",
                                                      "requested_action"
                                                    ]
                                                ]
                                            ),
                                          definitions.predicate.types
                                        ],
                                  }
                                  {}
                                  true
                                  predicates.always,
                                definitions.predicate.eventObject
                              ],
                          check_suite =
                            predicates.allOf
                              [
                                predicates.records.record
                                  {
                                    types =
                                      predicates.allOf
                                        [
                                          predicates.arrays.arrayOf
                                            (
                                              predicates.allOf
                                                [
                                                  predicates.isType 'String,
                                                  predicates.enum
                                                    ["completed", "requested", "rerequested"]
                                                ]
                                            ),
                                          definitions.predicate.types
                                        ],
                                  }
                                  {}
                                  true
                                  predicates.always,
                                definitions.predicate.eventObject
                              ],
                          create = definitions.predicate.eventObject,
                          delete = definitions.predicate.eventObject,
                          deployment = definitions.predicate.eventObject,
                          deployment_status = definitions.predicate.eventObject,
                          discussion =
                            predicates.allOf
                              [
                                predicates.records.record
                                  {
                                    types =
                                      predicates.allOf
                                        [
                                          predicates.arrays.arrayOf
                                            (
                                              predicates.allOf
                                                [
                                                  predicates.isType 'String,
                                                  predicates.enum
                                                    [
                                                      "created",
                                                      "edited",
                                                      "deleted",
                                                      "transferred",
                                                      "pinned",
                                                      "unpinned",
                                                      "labeled",
                                                      "unlabeled",
                                                      "locked",
                                                      "unlocked",
                                                      "category_changed",
                                                      "answered",
                                                      "unanswered"
                                                    ]
                                                ]
                                            ),
                                          definitions.predicate.types
                                        ],
                                  }
                                  {}
                                  true
                                  predicates.always,
                                definitions.predicate.eventObject
                              ],
                          discussion_comment =
                            predicates.allOf
                              [
                                predicates.records.record
                                  {
                                    types =
                                      predicates.allOf
                                        [
                                          predicates.arrays.arrayOf
                                            (
                                              predicates.allOf
                                                [
                                                  predicates.isType 'String,
                                                  predicates.enum ["created", "edited", "deleted"]
                                                ]
                                            ),
                                          definitions.predicate.types
                                        ],
                                  }
                                  {}
                                  true
                                  predicates.always,
                                definitions.predicate.eventObject
                              ],
                          fork = definitions.predicate.eventObject,
                          gollum = definitions.predicate.eventObject,
                          issue_comment =
                            predicates.allOf
                              [
                                predicates.records.record
                                  {
                                    types =
                                      predicates.allOf
                                        [
                                          predicates.arrays.arrayOf
                                            (
                                              predicates.allOf
                                                [
                                                  predicates.isType 'String,
                                                  predicates.enum ["created", "edited", "deleted"]
                                                ]
                                            ),
                                          definitions.predicate.types
                                        ],
                                  }
                                  {}
                                  true
                                  predicates.always,
                                definitions.predicate.eventObject
                              ],
                          issues =
                            predicates.allOf
                              [
                                predicates.records.record
                                  {
                                    types =
                                      predicates.allOf
                                        [
                                          predicates.arrays.arrayOf
                                            (
                                              predicates.allOf
                                                [
                                                  predicates.isType 'String,
                                                  predicates.enum
                                                    [
                                                      "opened",
                                                      "edited",
                                                      "deleted",
                                                      "transferred",
                                                      "pinned",
                                                      "unpinned",
                                                      "closed",
                                                      "reopened",
                                                      "assigned",
                                                      "unassigned",
                                                      "labeled",
                                                      "unlabeled",
                                                      "locked",
                                                      "unlocked",
                                                      "milestoned",
                                                      "demilestoned"
                                                    ]
                                                ]
                                            ),
                                          definitions.predicate.types
                                        ],
                                  }
                                  {}
                                  true
                                  predicates.always,
                                definitions.predicate.eventObject
                              ],
                          label =
                            predicates.allOf
                              [
                                predicates.records.record
                                  {
                                    types =
                                      predicates.allOf
                                        [
                                          predicates.arrays.arrayOf
                                            (
                                              predicates.allOf
                                                [
                                                  predicates.isType 'String,
                                                  predicates.enum ["created", "edited", "deleted"]
                                                ]
                                            ),
                                          definitions.predicate.types
                                        ],
                                  }
                                  {}
                                  true
                                  predicates.always,
                                definitions.predicate.eventObject
                              ],
                          member =
                            predicates.allOf
                              [
                                predicates.records.record
                                  {
                                    types =
                                      predicates.allOf
                                        [
                                          predicates.arrays.arrayOf
                                            (
                                              predicates.allOf
                                                [
                                                  predicates.isType 'String,
                                                  predicates.enum ["added", "edited", "deleted"]
                                                ]
                                            ),
                                          definitions.predicate.types
                                        ],
                                  }
                                  {}
                                  true
                                  predicates.always,
                                definitions.predicate.eventObject
                              ],
                          merge_group =
                            predicates.allOf
                              [
                                predicates.records.record
                                  {
                                    types =
                                      predicates.allOf
                                        [
                                          predicates.arrays.arrayOf
                                            (
                                              predicates.allOf
                                                [
                                                  predicates.isType 'String,
                                                  predicates.enum ["checks_requested"]
                                                ]
                                            ),
                                          definitions.predicate.types
                                        ],
                                  }
                                  {}
                                  true
                                  predicates.always,
                                definitions.predicate.eventObject
                              ],
                          milestone =
                            predicates.allOf
                              [
                                predicates.records.record
                                  {
                                    types =
                                      predicates.allOf
                                        [
                                          predicates.arrays.arrayOf
                                            (
                                              predicates.allOf
                                                [
                                                  predicates.isType 'String,
                                                  predicates.enum
                                                    [
                                                      "created",
                                                      "closed",
                                                      "opened",
                                                      "edited",
                                                      "deleted"
                                                    ]
                                                ]
                                            ),
                                          definitions.predicate.types
                                        ],
                                  }
                                  {}
                                  true
                                  predicates.always,
                                definitions.predicate.eventObject
                              ],
                          page_build = definitions.predicate.eventObject,
                          project =
                            predicates.allOf
                              [
                                predicates.records.record
                                  {
                                    types =
                                      predicates.allOf
                                        [
                                          predicates.arrays.arrayOf
                                            (
                                              predicates.allOf
                                                [
                                                  predicates.isType 'String,
                                                  predicates.enum
                                                    [
                                                      "created",
                                                      "updated",
                                                      "closed",
                                                      "reopened",
                                                      "edited",
                                                      "deleted"
                                                    ]
                                                ]
                                            ),
                                          definitions.predicate.types
                                        ],
                                  }
                                  {}
                                  true
                                  predicates.always,
                                definitions.predicate.eventObject
                              ],
                          project_card =
                            predicates.allOf
                              [
                                predicates.records.record
                                  {
                                    types =
                                      predicates.allOf
                                        [
                                          predicates.arrays.arrayOf
                                            (
                                              predicates.allOf
                                                [
                                                  predicates.isType 'String,
                                                  predicates.enum
                                                    [
                                                      "created",
                                                      "moved",
                                                      "converted",
                                                      "edited",
                                                      "deleted"
                                                    ]
                                                ]
                                            ),
                                          definitions.predicate.types
                                        ],
                                  }
                                  {}
                                  true
                                  predicates.always,
                                definitions.predicate.eventObject
                              ],
                          project_column =
                            predicates.allOf
                              [
                                predicates.records.record
                                  {
                                    types =
                                      predicates.allOf
                                        [
                                          predicates.arrays.arrayOf
                                            (
                                              predicates.allOf
                                                [
                                                  predicates.isType 'String,
                                                  predicates.enum
                                                    ["created", "updated", "moved", "deleted"]
                                                ]
                                            ),
                                          definitions.predicate.types
                                        ],
                                  }
                                  {}
                                  true
                                  predicates.always,
                                definitions.predicate.eventObject
                              ],
                          public = definitions.predicate.eventObject,
                          pull_request =
                            predicates.allOf
                              [
                                predicates.records.record
                                  {
                                    types =
                                      predicates.allOf
                                        [
                                          predicates.arrays.arrayOf
                                            (
                                              predicates.allOf
                                                [
                                                  predicates.isType 'String,
                                                  predicates.enum
                                                    [
                                                      "assigned",
                                                      "unassigned",
                                                      "labeled",
                                                      "unlabeled",
                                                      "opened",
                                                      "edited",
                                                      "closed",
                                                      "reopened",
                                                      "synchronize",
                                                      "converted_to_draft",
                                                      "ready_for_review",
                                                      "locked",
                                                      "unlocked",
                                                      "review_requested",
                                                      "review_request_removed",
                                                      "auto_merge_enabled",
                                                      "auto_merge_disabled"
                                                    ]
                                                ]
                                            ),
                                          definitions.predicate.types
                                        ],
                                  }
                                  {
                                    "^(branche|tag|path)s(-ignore)?$" =
                                      predicates.isType
                                        '"Array",
                                  }
                                  false
                                  predicates.never,
                                definitions.predicate.ref
                              ],
                          pull_request_review =
                            predicates.allOf
                              [
                                predicates.records.record
                                  {
                                    types =
                                      predicates.allOf
                                        [
                                          predicates.arrays.arrayOf
                                            (
                                              predicates.allOf
                                                [
                                                  predicates.isType 'String,
                                                  predicates.enum
                                                    ["submitted", "edited", "dismissed"]
                                                ]
                                            ),
                                          definitions.predicate.types
                                        ],
                                  }
                                  {}
                                  true
                                  predicates.always,
                                definitions.predicate.eventObject
                              ],
                          pull_request_review_comment =
                            predicates.allOf
                              [
                                predicates.records.record
                                  {
                                    types =
                                      predicates.allOf
                                        [
                                          predicates.arrays.arrayOf
                                            (
                                              predicates.allOf
                                                [
                                                  predicates.isType 'String,
                                                  predicates.enum ["created", "edited", "deleted"]
                                                ]
                                            ),
                                          definitions.predicate.types
                                        ],
                                  }
                                  {}
                                  true
                                  predicates.always,
                                definitions.predicate.eventObject
                              ],
                          pull_request_target =
                            predicates.allOf
                              [
                                predicates.records.record
                                  {
                                    types =
                                      predicates.allOf
                                        [
                                          predicates.arrays.arrayOf
                                            (
                                              predicates.allOf
                                                [
                                                  predicates.isType 'String,
                                                  predicates.enum
                                                    [
                                                      "assigned",
                                                      "unassigned",
                                                      "labeled",
                                                      "unlabeled",
                                                      "opened",
                                                      "edited",
                                                      "closed",
                                                      "reopened",
                                                      "synchronize",
                                                      "converted_to_draft",
                                                      "ready_for_review",
                                                      "locked",
                                                      "unlocked",
                                                      "review_requested",
                                                      "review_request_removed",
                                                      "auto_merge_enabled",
                                                      "auto_merge_disabled"
                                                    ]
                                                ]
                                            ),
                                          definitions.predicate.types
                                        ],
                                  }
                                  { "^(branche|tag|path)s(-ignore)?$" = predicates.always, }
                                  false
                                  predicates.never,
                                definitions.predicate.ref
                              ],
                          push =
                            predicates.allOf
                              [
                                predicates.records.record
                                  {}
                                  {
                                    "^(branche|tag|path)s(-ignore)?$" =
                                      predicates.allOf
                                        [
                                          predicates.isType '"Array",
                                          predicates.arrays.arrayOf
                                            (predicates.isType 'String)
                                        ],
                                  }
                                  false
                                  predicates.never,
                                definitions.predicate.ref
                              ],
                          registry_package =
                            predicates.allOf
                              [
                                predicates.records.record
                                  {
                                    types =
                                      predicates.allOf
                                        [
                                          predicates.arrays.arrayOf
                                            (
                                              predicates.allOf
                                                [
                                                  predicates.isType 'String,
                                                  predicates.enum ["published", "updated"]
                                                ]
                                            ),
                                          definitions.predicate.types
                                        ],
                                  }
                                  {}
                                  true
                                  predicates.always,
                                definitions.predicate.eventObject
                              ],
                          release =
                            predicates.allOf
                              [
                                predicates.records.record
                                  {
                                    types =
                                      predicates.allOf
                                        [
                                          predicates.arrays.arrayOf
                                            (
                                              predicates.allOf
                                                [
                                                  predicates.isType 'String,
                                                  predicates.enum
                                                    [
                                                      "published",
                                                      "unpublished",
                                                      "created",
                                                      "edited",
                                                      "deleted",
                                                      "prereleased",
                                                      "released"
                                                    ]
                                                ]
                                            ),
                                          definitions.predicate.types
                                        ],
                                  }
                                  {}
                                  true
                                  predicates.always,
                                definitions.predicate.eventObject
                              ],
                          repository_dispatch = definitions.predicate.eventObject,
                          schedule =
                            predicates.allOf
                              [
                                predicates.isType '"Array",
                                predicates.arrays.arrayOf
                                  (
                                    predicates.records.record
                                      {
                                        cron =
                                          predicates.allOf
                                            [
                                              predicates.isType 'String,
                                              predicates.strings.pattern
                                                "^(((\\d+,)+\\d+|((\\d+|\\*)/\\d+|((JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP|OCT|NOV|DEC)(-(JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP|OCT|NOV|DEC))?))|(\\d+-\\d+)|\\d+(-\\d+)?/\\d+(-\\d+)?|\\d+|\\*|(MON|TUE|WED|THU|FRI|SAT|SUN)(-(MON|TUE|WED|THU|FRI|SAT|SUN))?) ?){5}$"
                                            ],
                                      }
                                      {}
                                      false
                                      predicates.never
                                  ),
                                predicates.arrays.minItems 1
                              ],
                          status = definitions.predicate.eventObject,
                          watch = definitions.predicate.eventObject,
                          workflow_call =
                            predicates.records.record
                              {
                                inputs =
                                  predicates.allOf
                                    [
                                      predicates.isType 'Record,
                                      predicates.records.record
                                        {}
                                        {
                                          "^[_a-zA-Z][a-zA-Z0-9_-]*$" =
                                            predicates.allOf
                                              [
                                                predicates.isType 'Record,
                                                predicates.records.required ["type"],
                                                predicates.records.record
                                                  {
                                                    "default" =
                                                      predicates.anyOf
                                                        [
                                                          predicates.isType '"Bool",
                                                          predicates.isType 'Number,
                                                          predicates.isType 'String
                                                        ],
                                                    deprecationMessage =
                                                      predicates.isType
                                                        'String,
                                                    description = predicates.isType 'String,
                                                    required = predicates.isType '"Bool",
                                                    type =
                                                      predicates.allOf
                                                        [
                                                          predicates.isType 'String,
                                                          predicates.enum
                                                            ["boolean", "number", "string"]
                                                        ],
                                                  }
                                                  {}
                                                  false
                                                  predicates.never
                                              ],
                                        }
                                        false
                                        predicates.never
                                    ],
                                secrets =
                                  predicates.records.record
                                    {}
                                    {
                                      "^[_a-zA-Z][a-zA-Z0-9_-]*$" =
                                        predicates.allOf
                                          [
                                            predicates.records.required ["required"],
                                            predicates.records.record
                                              {
                                                description = predicates.isType 'String,
                                                required = predicates.isType '"Bool",
                                              }
                                              {}
                                              false
                                              predicates.never
                                          ],
                                    }
                                    false
                                    predicates.never,
                              }
                              {}
                              true
                              predicates.always,
                          workflow_dispatch =
                            predicates.records.record
                              {
                                inputs =
                                  predicates.allOf
                                    [
                                      predicates.isType 'Record,
                                      predicates.records.record
                                        {}
                                        {
                                          "^[_a-zA-Z][a-zA-Z0-9_-]*$" =
                                            predicates.allOf
                                              [
                                                predicates.isType 'Record,
                                                predicates.allOf
                                                  [
                                                    predicates.ifThenElse
                                                      (
                                                        predicates.allOf
                                                          [
                                                            predicates.records.required ["type"],
                                                            predicates.records.record
                                                              { type = predicates.const "string", }
                                                              {}
                                                              true
                                                              predicates.always
                                                          ]
                                                      )
                                                      (
                                                        predicates.records.record
                                                          { "default" = predicates.isType 'String, }
                                                          {}
                                                          true
                                                          predicates.always
                                                      )
                                                      predicates.always,
                                                    predicates.ifThenElse
                                                      (
                                                        predicates.allOf
                                                          [
                                                            predicates.records.required ["type"],
                                                            predicates.records.record
                                                              { type = predicates.const "boolean", }
                                                              {}
                                                              true
                                                              predicates.always
                                                          ]
                                                      )
                                                      (
                                                        predicates.records.record
                                                          { "default" = predicates.isType '"Bool", }
                                                          {}
                                                          true
                                                          predicates.always
                                                      )
                                                      predicates.always,
                                                    predicates.ifThenElse
                                                      (
                                                        predicates.allOf
                                                          [
                                                            predicates.records.required ["type"],
                                                            predicates.records.record
                                                              { type = predicates.const "number", }
                                                              {}
                                                              true
                                                              predicates.always
                                                          ]
                                                      )
                                                      (
                                                        predicates.records.record
                                                          { "default" = predicates.isType 'Number, }
                                                          {}
                                                          true
                                                          predicates.always
                                                      )
                                                      predicates.always,
                                                    predicates.ifThenElse
                                                      (
                                                        predicates.allOf
                                                          [
                                                            predicates.records.required ["type"],
                                                            predicates.records.record
                                                              { type = predicates.const "environment", }
                                                              {}
                                                              true
                                                              predicates.always
                                                          ]
                                                      )
                                                      (
                                                        predicates.records.record
                                                          { "default" = predicates.isType 'String, }
                                                          {}
                                                          true
                                                          predicates.always
                                                      )
                                                      predicates.always,
                                                    predicates.ifThenElse
                                                      (
                                                        predicates.allOf
                                                          [
                                                            predicates.records.required ["type"],
                                                            predicates.records.record
                                                              { type = predicates.const "choice", }
                                                              {}
                                                              true
                                                              predicates.always
                                                          ]
                                                      )
                                                      (
                                                        predicates.allOf
                                                          [
                                                            predicates.records.required ["options"],
                                                            predicates.records.record
                                                              {}
                                                              {}
                                                              true
                                                              predicates.always
                                                          ]
                                                      )
                                                      predicates.always
                                                  ],
                                                predicates.records.required ["description"],
                                                predicates.records.record
                                                  {
                                                    "default" = predicates.always,
                                                    deprecationMessage =
                                                      predicates.isType
                                                        'String,
                                                    description = predicates.isType 'String,
                                                    options =
                                                      predicates.allOf
                                                        [
                                                          predicates.isType '"Array",
                                                          predicates.arrays.arrayOf
                                                            (predicates.isType 'String),
                                                          predicates.arrays.minItems 1
                                                        ],
                                                    required = predicates.isType '"Bool",
                                                    type =
                                                      predicates.allOf
                                                        [
                                                          predicates.isType 'String,
                                                          predicates.enum
                                                            [
                                                              "string",
                                                              "choice",
                                                              "boolean",
                                                              "number",
                                                              "environment"
                                                            ]
                                                        ],
                                                  }
                                                  {}
                                                  false
                                                  predicates.never
                                              ],
                                        }
                                        false
                                        predicates.never
                                    ],
                              }
                              {}
                              true
                              predicates.always,
                          workflow_run =
                            predicates.allOf
                              [
                                predicates.records.record
                                  {
                                    types =
                                      predicates.allOf
                                        [
                                          predicates.arrays.arrayOf
                                            (
                                              predicates.allOf
                                                [
                                                  predicates.isType 'String,
                                                  predicates.enum ["requested", "completed"]
                                                ]
                                            ),
                                          definitions.predicate.types
                                        ],
                                    workflows =
                                      predicates.allOf
                                        [
                                          predicates.isType '"Array",
                                          predicates.arrays.arrayOf
                                            (predicates.isType 'String),
                                          predicates.arrays.minItems 1
                                        ],
                                  }
                                  { "^branches(-ignore)?$" = predicates.always, }
                                  true
                                  predicates.always,
                                definitions.predicate.eventObject
                              ],
                        }
                        {}
                        false
                        predicates.never
                    ]
                ],
            permissions = definitions.predicate.permissions,
            run-name = predicates.isType 'String,
          }
          {}
          false
          predicates.never
      ]
  )
