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
1. The TeamCity MCP server must be installed, and it must have read access to read the builds
   from the `Delft3D / Windows / Test` and `Delft3D / Linux / Test` build configurations.
   If not, ask users to install it.
2. The `gh` GitHub command line tool must be installed. If not ask users to install it.

## Steps:
1. Look up if the given `branch-name` has an associated GitHub PR with failed checks.
   Use the following command to list all the required checks for the PR:
   `gh pr checks <branch-name> --required --json name,link,state`
   Look for the `Test (Linux)` and `Test (Windows)` checks. If the `state` of these checks is `FAILURE`,
   there might be failing test cases. The `url` points to the _matrix build_ in TeamCity. The number at
   the end of the `url` is the _build number_.
2. For the failing _checks_ found in the previous step, use the TeamCity MCP to fetch the failing tests of
   the matrix build. Each failing test should mention the name of the _sub-build_, formatted like 
   `[<sub-build-name>]` along with the _build id_ of the sub-build.
3. This skill includes the `get_configs_by_branch.py` python script. Use this scrip to generate a
   `JSON` object that maps the `sub-build-name` (without square brackets) to the `TestBench` config
   XML files.
4. Report the list of failing test cases along with their config files to the user.
