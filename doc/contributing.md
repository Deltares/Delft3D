# Contributing

## Branch naming
For branches aimed to be merged into the main line the following naming convention should be used:
\<kernel\>`/`\<type\>`/`\<ISSUENR\>_short_description
with:
- \<kernel\>  : one of: `all`, `d3d4`, `fm`, `none`, `part`, `rr`, `swan`, `waq`, `wave`, `tc`
  -> The kernel selected determines the test cases being run as part of the integration pipeline; if you're unsure about the scope, use `all`.
- \<type\>    : one of: `bugfix`, `doc`, `feature`, `poc`, `release`, `task`
- \<ISSUENR\> : JIRA issue number associated with the activity

Example:
- `fm/feature/UNST-1234_improve_partition_file`

For longer lasting research branches, the following naming convention should be used:
`research/`\<organisation\>`/`short_description
with:
- \<organisation\> : short name of the lead organisation in the development

Example:
- `research/Deltares/improve_flow_scheme`
