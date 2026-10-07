---
name: download-testbench-testcase
description: 'Download `TestBench` test case data from MinIO'
argument-hint: '[partial-testcase-name ...] [config-path] [target-type]'
---

# Download `TestBench` test case(s)

## When to use

- "Download the test case input for X"
- "Get the test case documentation for test case Y"
- "Get test cases X, Y, Z, ..."
- "Download the references files for test case X"

## What this skill does

It uses the _DVC_ to download test case data from our
[MinIO bucket](https://s3.deltares.nl/delft3d-testbench)
to the local filesystem. It can download the _input files_, the _reference output files_
and the _documentation_ of a test case to the `/test/deltares_testbench/data/cases`
directory.

## Preconditions

1. The TestBench must be installed properly. *Always* check if the "venv" exists in
`/test/deltares_testbench/.venv` and run `dvc` in the activated "venv". If
there is any errors finding `dvc`, activating the "venv" or auth errors: Please use the
`install-testbench` skill to troubleshoot and install the TestBench properly.

## Command anatomy

```bash
dvc pull [${CASES_PATH}/${TEST_CASE_PATH}/{input,reference_win64,reference_lnx64,doc}.dvc ...]
```

The `dvc pull` command accepts a list of target `.dvc` files. These `.dvc` files are stored in the
directory structure under the _cases path_: `/test/deltares_testbench/data/cases`.
The user may mention a `target-type` as an argument to this skill. This can be one `input`, 
`reference` or `doc`. If the `target-type` is not mentioned please use the `input` targets, so
only the test case input files are downloaded. If the `reference` target type is passed and both
`reference_win64.dvc` and `reference_lnx64.dvc` are available, download the appropriate one for
the platform. Most of the time only one of these files is available. In that case just download
the available one. Even if you're on Linux and the only target is `reference_win64.dvc`. This
is a common occurence when both platforms are using the same set of reference files.