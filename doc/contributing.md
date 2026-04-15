# Contributing
In addition to read-only access to the source code, we encourage contributions from our user community.
Below are the specifics about our contributing process.

If you are entirely new to contributing to open source, [this generic guide](https://opensource.guide/how-to-contribute/) also helps explain why, what, and how to successfully get involved in open source projects.

## Workflow
### External contributors
 - Create a fork of the Delft3D repository.
 - Reach out to Deltares as early as possible if you intend to contribute changes back to the main Delft3D version.
   This helps us with keeping track of what developments are ongoing.
   This helps avoid starting work on something that other people are already implementing, and we can give guidance on how best to implement the intended changes.
 - Checkout/Clone the repository locally.
 - Create a branch, ideally using the naming convention below.
   The frequency of updating your fork/branch from the Deltares main is up to personal taste.
   Yet, merge from our main as often as possible, and contribute back to us as early as possible.
 - Implement, test and document the modifications.
 - Provide a patch-file, or reach out to Deltares to create a pull request:
   - Although anyone can create a pull request on our repository, our pipelines will only be triggered if the pull request is created by a Deltares employee.
   - Merging back to our main will typically include the following steps: transfer code changes to a branch in our Delft3D repository, security scan of the changes made, creation of a pull request, review of code, documentation and test cases, and automated code testing.
     Obviously, with some iterations if one of the steps identifies issues to be resolved before the merge.
 - To keep legal representation of the Delft3D software indisputable, we ask you to sign a Fiduciary License Agreement (FLA) before the final merge into main.
   For an explanation why, see [this page](https://fsfe.org/activities/fla/fla.en.html) by the Free Software Foundation Europe.
   The FLA can be obtained via the Deltares contact person who handles the merging process.
   Signing the FLA makes sense for code contributions of significant size.
   For small bug fixes, it's better to send an email with a test case and a description of the recommended code changes than following the formal procedure described above.

### Deltares employees
 - Checkout/Clone the repository locally.
 - Create a JIRA issue ticket at https://issuetracker.deltares.nl describing the bug to be fixed or functionality to be developed.
   The issue number is required for naming the development branch (see below).
 - Create a branch using the naming convention below.
   The frequency of updating your branch from main is up to personal taste.
   Yet, merge from main as often as possible, and merge back to main as early as possible.
 - Implement, test and document the modifications.
   In case of changes by external contributors, this step will include pulling the changes from the external repository into the local branch and at least a security scan of the changes made by the external contributor.
 - Create a pull request:
   - Our Continuous Integration pipelines will be triggered automatically by a pull request created by Deltares employees.
     These pipelines consist of (Deltares-internal) TeamCity projects to build the source code (Windows and Linux) and subsequently a set of model simulation testbenches.
     A merge is only possible when all checks succeed.
     The projects will take at least 30 minutes to complete.
   - You have to assign the pull request to a core developer for review.
     If review and all tests pass, the tester/reviewer is allowed to merge into main (signed Fiduciary License Agreement required in case of external contributor).
 - Official binary deliveries are only allowed using the Deltares TeamCity server.

## Branch naming
For each issue or feature, a separate branch should be created from the main.
To keep the branches organized each branch should adhere to the following naming conventions.

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
