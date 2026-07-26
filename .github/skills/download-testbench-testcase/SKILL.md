---
name: download-testbench-testcase
description: 'Download `TestBench` test case data from MinIO'
argument-hint: '[partial-testcase-name ...] [config-path]'
---

# Download `TestBench` test case(s)

## When to use

- "Download test case X"
- "Get test cases X, Y, Z, ..."

## What this skill does

It uses the _MinIO tools_ to download test cases from our [MinIO bucket](https://s3.deltares.nl/dsc-testbench)
to the directory `/test/deltares_testbench/data/`. It can download the _test case input_ to the `cases`
subdirectory, and the _test case references_ to the `reference_results` or `references` subdirectory (
depending on what's configured in the config XML file). If not specified by the user, download the
_test case input_.

## Preconditions

Similar pre-conditions to running `TestBench.py`, since the _MinIO tools_ are in the same code-base:

1. The working directory must be `/test/deltares_testbench/`. *Always* run `TestBench.py` from this directory.
2. Virtual environment `.venv` exists and must be activate. *Always* activate it before running the MinIO tools.
3. Python dependencies must be installed. If not, run `uv pip sync pip/win-requirements.txt`
   in an activated venv. Or if `uv` is not installed: `pip install -r pip/win-requirements.txt`.
4. The credentials for downloading the test case data in our `minio` bucket are installed in the user's
   home directory in the file `~/.aws/credentials`. If there are `minio`/`s3` auth errors please direct
   the user towards the `minio` 
   [UI page where they can create access keys](https://s3-console.deltares.nl/access-keys) and suggest
   the format of the credentials file. It is a standard AWS credentials file.

## Command anatomy

One test case:
```bash
python -m tools.minio pull --case|--reference --config <config-path> --test-case-name <test-case-name>
```

Multiple test cases:
```bash
python -m tools.minio pull --case|--reference --test-case-file <testcase-file-path>
```

Use `--case` to download just the _test case input_, and `--reference` to download the _test case references_.
The user may not actually supply a `config-path`, which is a required argument of `pull`. In this case,
use the `find-testbench-configs` skill to find a suitable config. `find-testbench-configs` may return multiple
configs. Let the user select one to use.
There is a way to download multiple test cases from multiple configs with the MinIO tools, but it requires
writing a _test case file_. It is a CSV file with two columns: `partial-testcase-name` and `config-path`.
Note that the `config-path` is relative to the `/test/deltares_testbench` and it supports glob patterns.
