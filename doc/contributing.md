# Contributing
In addition to read-only access to the source code, we encourage contributions from our user community. Below are the specifics about our contributing process.

If you are entirely new to contributing to open source, [this generic guide](https://opensource.guide/how-to-contribute/) also helps explain why, what, and how to successfully get involved in open source projects.

## Workflow
 - Request for access on https://github.com/Deltares/Delft3D
 - Create an issue in https://issuetracker.deltares.nl
   If an issue is not created, you have to create a branch of type "research"
 - Clone the repository
 - Create a branch using the naming convention below.
   The frequency of updating your branch from main is up to personal taste.
   Yet, merge from main as often as possible, and merge back to main as early as possible.
 - Create a Pull request (not required for "research" branches):
   - Continuous Integration pipelines will be triggered: these consist of (Deltares-internal) TeamCity projects to build the source code (Windows and Linux) and subsequently a set of model simulation testbenches. Continuation is only possible when all checks succeed. This will take at least 30 minutes.
   - You have to assign the Pull request to a core developer for reviewing and testing. When succeeded, the tester/reviewer is allowed to merge into main.
 - Official binary deliveries are only allowed using Deltares TeamCity server

## Development workflow
- Request for access on https://github.com/Deltares/Delft3D
- Create an issue in https://issuetracker.deltares.nl
  For 3rd party developers: create a branch of type research, or communicate with a Deltares contact person
- Clone the repository
- Create a branch using the naming convention below
  The frequency of updating your branch from main is up to personal taste.
  Yet, merge from main as often as possible, and merge back to main as early as possible.
- Create a Pull Request (not for research branches):
  - TeamCity projects will be automatically triggered to build the source code (Windows and Linux). Continuation is only possible when it succeeds. This will take at least 30 minutes.
  - A small set of QuickTests will be triggered on TeamCity. Continuation is only possible when it succeeds. This will take at least 30 minutes.
  - You have to assign the Pull Request to a core developer for reviewing and testing. When succeeded, the tester/reviewer is allowed to merge into trunk.
- Official binary deliveries are only allowed using Deltares TeamCity server

## Branch naming
For each issue or feature, a separate branch should be created from the main.
To keep the branches organized each branch should be adhere to the following naming conventions.

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

## Pull requests
When developments on a branch are ready for review and testing, a pull request should be created.
In the description text area on GitHub, use a closing keyword such that this PR will be automatically linked to the JIRA issue, if available. For example: Fixes #160.
