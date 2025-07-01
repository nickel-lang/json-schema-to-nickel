# DO NOT EDIT
# This file was automatically generated using json-schema-to-nickel
let js2n = import "../../lib/main.ncl" in
let rec refs = {
  definitions.branch =
    std.contract.Sequence [refs.definitions.globs, Array Dyn ],
  definitions.concurrency = {
    cancel-in-progress
      | std.contract.any_of [
        Bool,
        std.contract.Sequence [refs.definitions.expressionSyntax, String ]
      ]
      | optional,
    group | String
  },
  definitions.configuration =
    std.contract.any_of [ String, Number, Bool, { _ | js2n.Always }, Array js2n.Always],
  definitions.container = {
    credentials
      | {
        password | String | optional,
        username | String | optional,
        ..
      }
      | optional,
    env | refs.definitions.env | optional,
    image | String,
    options | String | optional,
    ports
      | js2n.array.MinItems 1
      | Array (std.contract.any_of [ Number, String ])
      | optional,
    volumes | js2n.array.MinItems 1 | Array String | optional
  },
  definitions.defaults =
    std.contract.Sequence [
      js2n.record.MinProperties 1,
      {
        run
          | js2n.record.MinProperties 1
          | {
            shell | refs.definitions.shell | String | optional,
            working-directory
              | refs.definitions.working-directory
              | String
              | optional
          }
          | optional
      }
    ],
  definitions.env =
    std.contract.any_of [
      { _ | std.contract.any_of [ Bool, Number, String ] },
      std.contract.Sequence [refs.definitions.stringContainingExpressionSyntax, String ]
    ],
  definitions.environment = { name | String, url | String | optional },
  definitions.event =
    std.contract.Sequence [
      std.enum.TagOrString,
      [|
        'repository_dispatch,
        'workflow_run,
        'workflow_dispatch,
        'workflow_call,
        'watch,
        'status,
        'release,
        'registry_package,
        'push,
        'pull_request_target,
        'pull_request_review_comment,
        'pull_request_review,
        'pull_request,
        'public,
        'project_column,
        'project_card,
        'project,
        'page_build,
        'milestone,
        'merge_group,
        'label,
        'issues,
        'issue_comment,
        'gollum,
        'fork,
        'discussion_comment,
        'discussion,
        'deployment_status,
        'deployment,
        'delete,
        'create,
        'check_suite,
        'check_run,
        'branch_protection_rule
      |]
    ],
  definitions.eventObject = js2n.Nullable { .. },
  definitions.expressionSyntax =
    std.string.Matches "^\\$\\{\\{(.|[\r\n])*\\}\\}$",
  definitions.globs =
    std.contract.Sequence [js2n.array.MinItems 1, Array (js2n.string.MinLength 1)],
  definitions.jobNeeds =
    std.contract.any_of [
      std.contract.Sequence [
        js2n.array.MinItems 1,
        Array (
          std.contract.Sequence [refs.definitions.name, String ]
        )
      ],
      std.contract.Sequence [refs.definitions.name, String ]
    ],
  definitions.matrix =
    std.contract.any_of [
      std.contract.Sequence [
        js2n.record.MinProperties 1,
        js2n.record.Record {
          properties = {
            exclude =
              std.contract.any_of [
                std.contract.Sequence [
                  refs.definitions.expressionSyntax,
                  String
                ],
                std.contract.Sequence [
                  js2n.array.MinItems 1,
                  Array (
                    { _ | refs.definitions.configuration }
                  )
                ]
              ],
            include =
              std.contract.any_of [
                std.contract.Sequence [
                  refs.definitions.expressionSyntax,
                  String
                ],
                std.contract.Sequence [
                  js2n.array.MinItems 1,
                  Array (
                    { _ | refs.definitions.configuration }
                  )
                ]
              ]
          },
          required = {},
          patterns = {},
          additional =
            'Some (
              std.contract.any_of [
                std.contract.Sequence [
                  js2n.array.MinItems 1,
                  Array refs.definitions.configuration
                ],
                std.contract.Sequence [refs.definitions.expressionSyntax, String ]
              ]
            )
        }
      ],
      std.contract.Sequence [refs.definitions.expressionSyntax, String ]
    ],
  definitions.name = std.string.Matches "^[_a-zA-Z][a-zA-Z0-9_-]*$",
  definitions.normalJob = {
    concurrency
      | std.contract.any_of [
        String,
        std.contract.Sequence [refs.definitions.concurrency, { _ | Dyn }]
      ]
      | optional,
    container
      | std.contract.any_of [
        String,
        std.contract.Sequence [refs.definitions.container, { _ | Dyn }]
      ]
      | optional,
    continue-on-error
      | std.contract.any_of [
        Bool,
        std.contract.Sequence [refs.definitions.expressionSyntax, String ]
      ]
      | optional,
    defaults | refs.definitions.defaults | { _ | Dyn } | optional,
    env | refs.definitions.env | optional,
    environment
      | std.contract.any_of [
        String,
        std.contract.Sequence [refs.definitions.environment, { _ | Dyn }]
      ]
      | optional,
    "if" | std.contract.any_of [ Bool, Number, String ] | optional,
    name | String | optional,
    needs | refs.definitions.jobNeeds | optional,
    outputs | js2n.record.MinProperties 1 | { _ | String } | optional,
    permissions | refs.definitions.permissions | optional,
    runs-on
      | std.contract.any_of [
        String,
        std.contract.Sequence [
          js2n.array.MinItems 1,
          js2n.array.Items [ String ] String
        ],
        js2n.record.Record {
          properties = {
            group = String,
            labels =
              std.contract.any_of [ String, js2n.array.ArrayOf String ]
          },
          required = {},
          patterns = {},
          additional = 'Some Dyn
        },
        std.contract.Sequence [
          refs.definitions.stringContainingExpressionSyntax,
          String
        ],
        std.contract.Sequence [refs.definitions.expressionSyntax, String ]
      ],
    services
      | {
        _ | std.contract.Sequence [refs.definitions.container, { _ | Dyn }]
      }
      | optional,
    steps
      | js2n.array.MinItems 1
      | Array (
        std.contract.Sequence [refs.definitions.step, { _ | Dyn }]
      )
      | optional,
    strategy
      | {
        fail-fast | std.contract.any_of [ Bool, String ] | optional,
        matrix | refs.definitions.matrix,
        max-parallel | std.contract.any_of [ Number, String ] | optional
      }
      | optional,
    timeout-minutes
      | std.contract.any_of [
        Number,
        std.contract.Sequence [refs.definitions.expressionSyntax, String ]
      ]
      | optional
  },
  definitions.path =
    std.contract.Sequence [refs.definitions.globs, Array Dyn ],
  definitions.permissions =
    std.contract.any_of [
      std.contract.Sequence [std.enum.TagOrString, [| 'write-all, 'read-all |]],
      std.contract.Sequence [refs.definitions.permissions-event, { _ | Dyn }]
    ],
  definitions.permissions-event = {
    actions | refs.definitions.permissions-level | String | optional,
    attestations
      | refs.definitions.permissions-level
      | String
      | optional,
    checks | refs.definitions.permissions-level | String | optional,
    contents | refs.definitions.permissions-level | String | optional,
    deployments
      | refs.definitions.permissions-level
      | String
      | optional,
    discussions
      | refs.definitions.permissions-level
      | String
      | optional,
    id-token | refs.definitions.permissions-level | String | optional,
    issues | refs.definitions.permissions-level | String | optional,
    models | std.enum.TagOrString | [| 'none, 'read |] | optional,
    packages | refs.definitions.permissions-level | String | optional,
    pages | refs.definitions.permissions-level | String | optional,
    pull-requests
      | refs.definitions.permissions-level
      | String
      | optional,
    repository-projects
      | refs.definitions.permissions-level
      | String
      | optional,
    security-events
      | refs.definitions.permissions-level
      | String
      | optional,
    statuses | refs.definitions.permissions-level | String | optional
  },
  definitions.permissions-level =
    std.contract.Sequence [std.enum.TagOrString, [| 'none, 'write, 'read |]],
  definitions.ref =
    std.contract.Sequence [
      js2n.Nullable {
        branches | refs.definitions.branch | Array Dyn | optional,
        branches-ignore
          | refs.definitions.branch
          | Array Dyn
          | optional,
        paths | refs.definitions.path | Array Dyn | optional,
        paths-ignore | refs.definitions.path | Array Dyn | optional,
        tags | refs.definitions.branch | Array Dyn | optional,
        tags-ignore | refs.definitions.branch | Array Dyn | optional,
        ..
      },
      js2n.Nullable (
        std.contract.Sequence [
          std.contract.not (
            std.contract.any_of [
              js2n.Null,
              Bool,
              Number,
              String,
              js2n.record.Required ["branches", "branches-ignore"],
              Array Dyn
            ]
          ),
          std.contract.not (
            std.contract.any_of [
              js2n.Null,
              Bool,
              Number,
              String,
              js2n.record.Required ["tags", "tags-ignore"],
              Array Dyn
            ]
          ),
          std.contract.not (
            std.contract.any_of [
              js2n.Null,
              Bool,
              Number,
              String,
              js2n.record.Required ["paths", "paths-ignore"],
              Array Dyn
            ]
          ),
          { _ | Dyn }
        ]
      )
    ],
  definitions.reusableWorkflowCallJob = {
    concurrency
      | std.contract.any_of [
        String,
        std.contract.Sequence [refs.definitions.concurrency, { _ | Dyn }]
      ]
      | optional,
    "if" | std.contract.any_of [ Bool, Number, String ] | optional,
    name | String | optional,
    needs | refs.definitions.jobNeeds | optional,
    permissions | refs.definitions.permissions | optional,
    secrets
      | js2n.OneOf [
        refs.definitions.env,
        std.contract.Sequence [std.enum.TagOrString, [| 'inherit |]]
      ]
      | optional,
    strategy
      | {
        fail-fast | std.contract.any_of [ Bool, String ] | optional,
        matrix | refs.definitions.matrix,
        max-parallel | std.contract.any_of [ Number, String ] | optional
      }
      | optional,
    uses | std.string.Matches "^(.+\\/)+(.+)\\.(ya?ml)(@.+)?$",
    with | refs.definitions.env | optional
  },
  definitions.shell =
    std.contract.any_of [
      String,
      std.contract.Sequence [
        std.enum.TagOrString,
        [| 'powershell, 'cmd, 'sh, 'python, 'pwsh, 'bash |]
      ]
    ],
  definitions.step =
    std.contract.Sequence [
      js2n.record.DependentFields { shell = ["run"], working-directory = ["run"] },
      js2n.OneOf [
        std.contract.any_of [
          js2n.Null,
          Bool,
          Number,
          String,
          js2n.record.Required ["uses"],
          Array Dyn
        ],
        std.contract.any_of [
          js2n.Null,
          Bool,
          Number,
          String,
          js2n.record.Required ["run"],
          Array Dyn
        ]
      ],
      {
        continue-on-error
          | std.contract.any_of [
            Bool,
            std.contract.Sequence [refs.definitions.expressionSyntax, String ]
          ]
          | optional,
        env | refs.definitions.env | optional,
        id | String | optional,
        "if" | std.contract.any_of [ Bool, Number, String ] | optional,
        name | String | optional,
        run | String | optional,
        shell | refs.definitions.shell | String | optional,
        timeout-minutes
          | std.contract.any_of [
            Number,
            std.contract.Sequence [refs.definitions.expressionSyntax, String ]
          ]
          | optional,
        uses | String | optional,
        with
          | std.contract.any_of [
            js2n.Null,
            Bool,
            Number,
            String,
            {
              args | String | optional,
              entrypoint | String | optional,
              ..
            },
            Array Dyn
          ]
          | refs.definitions.env
          | optional,
        working-directory
          | refs.definitions.working-directory
          | String
          | optional
      }
    ],
  definitions.stringContainingExpressionSyntax =
    std.string.Matches "^.*\\$\\{\\{(.|[\r\n])*\\}\\}.*$",
  definitions.types = std.contract.any_of [js2n.array.MinItems 1, String ],
  definitions.workflowDispatchInput =
    std.contract.Sequence [
      js2n.IfThenElse
        (
          std.contract.any_of [
            js2n.Null,
            Bool,
            Number,
            String,
            js2n.record.Record {
              properties = { type = js2n.Const "string" },
              required = { type = true },
              patterns = {},
              additional = 'Some Dyn
            },
            Array Dyn
          ]
        )
        (
          std.contract.any_of [
            js2n.Null,
            Bool,
            Number,
            String,
            { "default" | String | optional, .. },
            Array Dyn
          ]
        )
        js2n.Always,
      js2n.IfThenElse
        (
          std.contract.any_of [
            js2n.Null,
            Bool,
            Number,
            String,
            js2n.record.Record {
              properties = { type = js2n.Const "boolean" },
              required = { type = true },
              patterns = {},
              additional = 'Some Dyn
            },
            Array Dyn
          ]
        )
        (
          std.contract.any_of [
            js2n.Null,
            Bool,
            Number,
            String,
            { "default" | Bool | optional, .. },
            Array Dyn
          ]
        )
        js2n.Always,
      js2n.IfThenElse
        (
          std.contract.any_of [
            js2n.Null,
            Bool,
            Number,
            String,
            js2n.record.Record {
              properties = { type = js2n.Const "number" },
              required = { type = true },
              patterns = {},
              additional = 'Some Dyn
            },
            Array Dyn
          ]
        )
        (
          std.contract.any_of [
            js2n.Null,
            Bool,
            Number,
            String,
            { "default" | Number | optional, .. },
            Array Dyn
          ]
        )
        js2n.Always,
      js2n.IfThenElse
        (
          std.contract.any_of [
            js2n.Null,
            Bool,
            Number,
            String,
            js2n.record.Record {
              properties = { type = js2n.Const "environment" },
              required = { type = true },
              patterns = {},
              additional = 'Some Dyn
            },
            Array Dyn
          ]
        )
        (
          std.contract.any_of [
            js2n.Null,
            Bool,
            Number,
            String,
            { "default" | String | optional, .. },
            Array Dyn
          ]
        )
        js2n.Always,
      js2n.IfThenElse
        (
          std.contract.any_of [
            js2n.Null,
            Bool,
            Number,
            String,
            js2n.record.Record {
              properties = { type = js2n.Const "choice" },
              required = { type = true },
              patterns = {},
              additional = 'Some Dyn
            },
            Array Dyn
          ]
        )
        (
          std.contract.any_of [
            js2n.Null,
            Bool,
            Number,
            String,
            js2n.record.Required ["options"],
            Array Dyn
          ]
        )
        js2n.Always,
      {
        "default"
          | std.contract.any_of [js2n.Null, Bool, Number, String, { _ | Dyn }, Array Dyn ]
          | optional,
        deprecationMessage | String | optional,
        description | String,
        options | js2n.array.MinItems 1 | Array String | optional,
        required | Bool | optional,
        type
          | std.enum.TagOrString
          | [| 'environment, 'number, 'boolean, 'choice, 'string |]
          | optional
      }
    ],
  definitions.working-directory = String
}
in
{
  concurrency
    | std.contract.any_of [ String, refs.definitions.concurrency]
    | optional,
  defaults | refs.definitions.defaults | { _ | Dyn } | optional,
  env | refs.definitions.env | optional,
  jobs
    | js2n.record.MinProperties 1
    | {
      _ | js2n.OneOf [
        refs.definitions.normalJob,
        refs.definitions.reusableWorkflowCallJob
      ]
    }
    | std.record.FieldsMatch "^[_a-zA-Z][a-zA-Z0-9_-]*$",
  name | String | optional,
  on
    | std.contract.any_of [
      refs.definitions.event,
      std.contract.Sequence [js2n.array.MinItems 1, Array refs.definitions.event],
      {
        branch_protection_rule
          | js2n.Nullable {
            types
              | std.contract.any_of [
                String,
                Array (
                  std.contract.Sequence [
                    std.enum.TagOrString,
                    [| 'deleted, 'edited, 'created |]
                  ]
                )
              ]
              | refs.definitions.types
              | optional,
            ..
          }
          | refs.definitions.eventObject
          | optional,
        check_run
          | js2n.Nullable {
            types
              | std.contract.any_of [
                String,
                Array (
                  std.contract.Sequence [
                    std.enum.TagOrString,
                    [|
                      'requested_action,
                      'completed,
                      'rerequested,
                      'created
                    |]
                  ]
                )
              ]
              | refs.definitions.types
              | optional,
            ..
          }
          | refs.definitions.eventObject
          | optional,
        check_suite
          | js2n.Nullable {
            types
              | std.contract.any_of [
                String,
                Array (
                  std.contract.Sequence [
                    std.enum.TagOrString,
                    [| 'rerequested, 'requested, 'completed |]
                  ]
                )
              ]
              | refs.definitions.types
              | optional,
            ..
          }
          | refs.definitions.eventObject
          | optional,
        create | refs.definitions.eventObject | optional,
        delete | refs.definitions.eventObject | optional,
        deployment | refs.definitions.eventObject | optional,
        deployment_status | refs.definitions.eventObject | optional,
        discussion
          | js2n.Nullable {
            types
              | std.contract.any_of [
                String,
                Array (
                  std.contract.Sequence [
                    std.enum.TagOrString,
                    [|
                      'unanswered,
                      'answered,
                      'category_changed,
                      'unlocked,
                      'locked,
                      'unlabeled,
                      'labeled,
                      'unpinned,
                      'pinned,
                      'transferred,
                      'deleted,
                      'edited,
                      'created
                    |]
                  ]
                )
              ]
              | refs.definitions.types
              | optional,
            ..
          }
          | refs.definitions.eventObject
          | optional,
        discussion_comment
          | js2n.Nullable {
            types
              | std.contract.any_of [
                String,
                Array (
                  std.contract.Sequence [
                    std.enum.TagOrString,
                    [| 'deleted, 'edited, 'created |]
                  ]
                )
              ]
              | refs.definitions.types
              | optional,
            ..
          }
          | refs.definitions.eventObject
          | optional,
        fork | refs.definitions.eventObject | optional,
        gollum | refs.definitions.eventObject | optional,
        issue_comment
          | js2n.Nullable {
            types
              | std.contract.any_of [
                String,
                Array (
                  std.contract.Sequence [
                    std.enum.TagOrString,
                    [| 'deleted, 'edited, 'created |]
                  ]
                )
              ]
              | refs.definitions.types
              | optional,
            ..
          }
          | refs.definitions.eventObject
          | optional,
        issues
          | js2n.Nullable {
            types
              | std.contract.any_of [
                String,
                Array (
                  std.contract.Sequence [
                    std.enum.TagOrString,
                    [|
                      'demilestoned,
                      'milestoned,
                      'unlocked,
                      'locked,
                      'unlabeled,
                      'labeled,
                      'unassigned,
                      'assigned,
                      'reopened,
                      'closed,
                      'unpinned,
                      'pinned,
                      'transferred,
                      'deleted,
                      'edited,
                      'opened
                    |]
                  ]
                )
              ]
              | refs.definitions.types
              | optional,
            ..
          }
          | refs.definitions.eventObject
          | optional,
        label
          | js2n.Nullable {
            types
              | std.contract.any_of [
                String,
                Array (
                  std.contract.Sequence [
                    std.enum.TagOrString,
                    [| 'deleted, 'edited, 'created |]
                  ]
                )
              ]
              | refs.definitions.types
              | optional,
            ..
          }
          | refs.definitions.eventObject
          | optional,
        merge_group
          | js2n.Nullable {
            types
              | std.contract.any_of [
                String,
                Array (
                  std.contract.Sequence [std.enum.TagOrString, [| 'checks_requested |]]
                )
              ]
              | refs.definitions.types
              | optional,
            ..
          }
          | refs.definitions.eventObject
          | optional,
        milestone
          | js2n.Nullable {
            types
              | std.contract.any_of [
                String,
                Array (
                  std.contract.Sequence [
                    std.enum.TagOrString,
                    [|
                      'deleted,
                      'edited,
                      'opened,
                      'closed,
                      'created
                    |]
                  ]
                )
              ]
              | refs.definitions.types
              | optional,
            ..
          }
          | refs.definitions.eventObject
          | optional,
        page_build | refs.definitions.eventObject | optional,
        project
          | js2n.Nullable {
            types
              | std.contract.any_of [
                String,
                Array (
                  std.contract.Sequence [
                    std.enum.TagOrString,
                    [|
                      'deleted,
                      'edited,
                      'reopened,
                      'closed,
                      'updated,
                      'created
                    |]
                  ]
                )
              ]
              | refs.definitions.types
              | optional,
            ..
          }
          | refs.definitions.eventObject
          | optional,
        project_card
          | js2n.Nullable {
            types
              | std.contract.any_of [
                String,
                Array (
                  std.contract.Sequence [
                    std.enum.TagOrString,
                    [|
                      'deleted,
                      'edited,
                      'converted,
                      'moved,
                      'created
                    |]
                  ]
                )
              ]
              | refs.definitions.types
              | optional,
            ..
          }
          | refs.definitions.eventObject
          | optional,
        project_column
          | js2n.Nullable {
            types
              | std.contract.any_of [
                String,
                Array (
                  std.contract.Sequence [
                    std.enum.TagOrString,
                    [| 'deleted, 'moved, 'updated, 'created |]
                  ]
                )
              ]
              | refs.definitions.types
              | optional,
            ..
          }
          | refs.definitions.eventObject
          | optional,
        public | refs.definitions.eventObject | optional,
        pull_request
          | js2n.Nullable {
            branches | Array Dyn | optional,
            branches-ignore | Array Dyn | optional,
            paths | Array Dyn | optional,
            paths-ignore | Array Dyn | optional,
            tags | Array Dyn | optional,
            tags-ignore | Array Dyn | optional,
            types
              | std.contract.any_of [
                String,
                Array (
                  std.contract.Sequence [
                    std.enum.TagOrString,
                    [|
                      'dequeued,
                      'enqueued,
                      'auto_merge_disabled,
                      'auto_merge_enabled,
                      'review_request_removed,
                      'review_requested,
                      'demilestoned,
                      'milestoned,
                      'unlocked,
                      'locked,
                      'ready_for_review,
                      'converted_to_draft,
                      'synchronize,
                      'reopened,
                      'closed,
                      'edited,
                      'opened,
                      'unlabeled,
                      'labeled,
                      'unassigned,
                      'assigned
                    |]
                  ]
                )
              ]
              | refs.definitions.types
              | optional
          }
          | refs.definitions.ref
          | optional,
        pull_request_review
          | js2n.Nullable {
            types
              | std.contract.any_of [
                String,
                Array (
                  std.contract.Sequence [
                    std.enum.TagOrString,
                    [| 'dismissed, 'edited, 'submitted |]
                  ]
                )
              ]
              | refs.definitions.types
              | optional,
            ..
          }
          | refs.definitions.eventObject
          | optional,
        pull_request_review_comment
          | js2n.Nullable {
            types
              | std.contract.any_of [
                String,
                Array (
                  std.contract.Sequence [
                    std.enum.TagOrString,
                    [| 'deleted, 'edited, 'created |]
                  ]
                )
              ]
              | refs.definitions.types
              | optional,
            ..
          }
          | refs.definitions.eventObject
          | optional,
        pull_request_target
          | js2n.Nullable {
            branches | js2n.Always | optional,
            branches-ignore | js2n.Always | optional,
            paths | js2n.Always | optional,
            paths-ignore | js2n.Always | optional,
            tags | js2n.Always | optional,
            tags-ignore | js2n.Always | optional,
            types
              | std.contract.any_of [
                String,
                Array (
                  std.contract.Sequence [
                    std.enum.TagOrString,
                    [|
                      'auto_merge_disabled,
                      'auto_merge_enabled,
                      'review_request_removed,
                      'review_requested,
                      'unlocked,
                      'locked,
                      'ready_for_review,
                      'converted_to_draft,
                      'synchronize,
                      'reopened,
                      'closed,
                      'edited,
                      'opened,
                      'unlabeled,
                      'labeled,
                      'unassigned,
                      'assigned
                    |]
                  ]
                )
              ]
              | refs.definitions.types
              | optional
          }
          | refs.definitions.ref
          | optional,
        push
          | js2n.Nullable {
            branches | Array String | optional,
            branches-ignore | Array String | optional,
            paths | Array String | optional,
            paths-ignore | Array String | optional,
            tags | Array String | optional,
            tags-ignore | Array String | optional
          }
          | refs.definitions.ref
          | optional,
        registry_package
          | js2n.Nullable {
            types
              | std.contract.any_of [
                String,
                Array (
                  std.contract.Sequence [
                    std.enum.TagOrString,
                    [| 'updated, 'published |]
                  ]
                )
              ]
              | refs.definitions.types
              | optional,
            ..
          }
          | refs.definitions.eventObject
          | optional,
        release
          | js2n.Nullable {
            types
              | std.contract.any_of [
                String,
                Array (
                  std.contract.Sequence [
                    std.enum.TagOrString,
                    [|
                      'released,
                      'prereleased,
                      'deleted,
                      'edited,
                      'created,
                      'unpublished,
                      'published
                    |]
                  ]
                )
              ]
              | refs.definitions.types
              | optional,
            ..
          }
          | refs.definitions.eventObject
          | optional,
        repository_dispatch | refs.definitions.eventObject | optional,
        schedule
          | js2n.array.MinItems 1
          | Array { cron | String | optional }
          | optional,
        status | refs.definitions.eventObject | optional,
        watch | refs.definitions.eventObject | optional,
        workflow_call
          | std.contract.any_of [
            js2n.Null,
            Bool,
            Number,
            String,
            {
              inputs
                | {
                  _ | {
                    "default"
                      | std.contract.any_of [ Bool, Number, String ]
                      | optional,
                    description | String | optional,
                    required | Bool | optional,
                    type
                      | std.enum.TagOrString
                      | [| 'string, 'number, 'boolean |]
                  }
                }
                | std.record.FieldsMatch "^[_a-zA-Z][a-zA-Z0-9_-]*$"
                | optional,
              outputs
                | {
                  _ | {
                    description | String | optional,
                    value | String
                  }
                }
                | std.record.FieldsMatch "^[_a-zA-Z][a-zA-Z0-9_-]*$"
                | optional,
              secrets
                | std.contract.any_of [
                  js2n.Null,
                  Bool,
                  Number,
                  String,
                  std.contract.Sequence [
                    {
                      _ | std.contract.any_of [
                        js2n.Null,
                        Bool,
                        Number,
                        String,
                        {
                          description | String | optional,
                          required | Bool | optional
                        },
                        Array Dyn
                      ]
                    },
                    std.record.FieldsMatch "^[_a-zA-Z][a-zA-Z0-9_-]*$"
                  ],
                  Array Dyn
                ]
                | optional,
              ..
            },
            Array Dyn
          ]
          | optional,
        workflow_dispatch
          | std.contract.any_of [
            js2n.Null,
            Bool,
            Number,
            String,
            {
              inputs
                | {
                  _ | std.contract.Sequence [
                    refs.definitions.workflowDispatchInput,
                    { _ | Dyn }
                  ]
                }
                | std.record.FieldsMatch "^[_a-zA-Z][a-zA-Z0-9_-]*$"
                | optional
            },
            Array Dyn
          ]
          | optional,
        workflow_run
          | js2n.Nullable {
            branches | js2n.Always | optional,
            branches-ignore | js2n.Always | optional,
            types
              | std.contract.any_of [
                String,
                Array (
                  std.contract.Sequence [
                    std.enum.TagOrString,
                    [| 'in_progress, 'completed, 'requested |]
                  ]
                )
              ]
              | refs.definitions.types
              | optional,
            workflows | js2n.array.MinItems 1 | Array String | optional,
            ..
          }
          | refs.definitions.eventObject
          | optional
      }
    ],
  permissions | refs.definitions.permissions | optional,
  run-name | String | optional
}
