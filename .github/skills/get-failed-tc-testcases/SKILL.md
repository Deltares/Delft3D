---
name: get-failed-tc-testcases
description: 'Get failed test cases (with configs) from TeamCity by branch name'
argument-hint: '[branch-name] [platform]'
---

# Get failed test cases from TeamCity

## When to use
- "Get the failed test cases from TeamCity for `platform`"
- "Get the failed test cases from branch `branch-name`"

## What this skill does
Our CI runs the `TestBench.py` on a selection of `TestBench` XML configs. This skill tries
to find the latest _build_ for the given `branch-name` in our TeamCity project (or the current
branch if `branch-name` is not specified) and makes a list test cases that failed, along with
their config file.

In our TeamCity server there is a project `Delft3D` with subprojects `Linux` and `Windows`.
Both subprojects have a build configuration called `Test`. In both cases, this is a _matrix build_.
Each element of the matrix runs `TestBench.py` on a single `TestBench` config file. Each of these
builds has _test results_ associated with it that TeamCity server API exposes.

If the `platform` is not specified, get the failed tests for both Linux and Windows.

## Preconditions
1. The _TeamCity CLI tool_ `teamcity` *must* be installed. If not ask users to install it.
2. The _GitHub CLI tool_ `gh` *must* be installed. If not ask users to install it.
3. `jq` is used in the steps below. This is to avoid having to read very large responses from the cli tools.

## Steps:
1. Look up if the given `branch-name` has an associated GitHub PR with failed checks.
   Use the `gh` to list the required checks for the PR:

   ```gh pr checks <branch-name> --required --json name,link,state```

   Look for the `Test (Linux)` and `Test (Windows)` checks. If the `state` of these checks is `FAILURE`,
   there might be failing test cases. The `url` points to the _matrix build_ in TeamCity. The number at
   the end of the `url` is the `build-id`. You need the `build-id` to query the failing tests.
2. Find the failing tests of the matrix build `Test (Linux)` and/or `Test (Windows)` with `teamcity`.
   In a _matrix build_, the tests of all of the sub-builds of the matrix are accumulated. Each test occurrence
   in the _matrix build_ has an `id` and a `name` property. The `id` property includes the `sub-build-id` that
   we're looking for, but it is buried inside the `id` property. Use `jq` to extract the information we need
   from `teamcity` output:

   ```teamcity build tests <build-id> --failed --json | jq '[.testOccurrence[] | { (.name) : (.id | capture("^build:[(]id:(?<sub_id>[0-9]+)[)],.*$").sub_id) }] | add'```

   The result is a map from the failing test case names to the sub-build ids.
3. Find the failing sub-builds of the matrix build `Test (Linux)` and/or `Test (Windows)` with `teamcity`.
   Use `jq` to filter out the successful sub-builds if `jq` is installed:

   ```teamcity build tree <build-id> --depth 1 --json | jq '[.dependencies[] | select(.status == "FAILURE") | { (.id | tostring) : .name }] | add'```

   The result is a map from the sub-build ids to the names of the sub-builds.
4. This skill includes the `get_configs_by_branch.py` python script. Use it as follows:
   
   ```python get_configs_by_branch.py --branch-name <branch-name> --platform {lnx64,win64}```

   The result is a map from the sub-build names to the paths to the actual `TestBench` config XMLs,
   relative to the `/test/deltares_testbench` directory.
5. Join the results of step 2 and step 3 by `build-id` to produce a map from the failing test case names to _sub build_ names.
   Then join that map with the result of step 4 by `sub-build-name` to produce a map from failing _test case names_ to _config files_.
   Report the results to the user and ask if they want to write the results as a _test case file_. If so, write the
   `(testcase-name, config-file)` pairs as a CSV file in the `/test/deltares_testbench/data/` directory, and mention the JIRA issue id 
   (from the branch) in the filename, if present.
