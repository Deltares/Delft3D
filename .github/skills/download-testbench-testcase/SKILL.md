---
name: download-testbench-testcase
description: 'Download `TestBench` test case data from MinIO'
argument-hint: '[partial-testcase-name ...] [config-path] [timestamp]'
---

# Download `TestBench` test case(s)

## When to use

- "Download test case X"
- "Get test cases X, Y, Z, ..."

## What this skill does

It uses the _DVC_ to download test case data from our [MinIO bucket](https://s3.deltares.nl/delft3d-testbench)
to the local filesystem. It can download the _input files_, the _reference output files_ and the
_documentation_ of a test case to the `/test/deltares_testbench/data/cases` directory.

## Preconditions

1. The virtual environment `.venv` exists in `/test/deltares_testbench` with the python dependencies
   installed. The `dvc` cli tool is installed in this venv so you *must* activate the venv to use it.
   Only if the create the venv and install the dependencies using the following steps:
   If `uv` is installed:
   On Windows:
```powershell
# From working directory `/test/deltares_testbench`
uv venv --python 3.12
.venv/Scripts/activate
uv pip install pip/win-requirements.txt
```
   On Linux:
```bash
# From working directory `/test/deltares_testbench`
uv venv --python 3.12
source .venv/bin/activate
uv pip install pip/lnx-requirements.txt
```
3. The credentials for downloading the test case data in our `minio` bucket are installed in the user's
   home directory in the file `~/.aws/credentials`. If there are `minio`/`s3` auth errors please direct
   the user towards the `minio` 
   [UI page where they can create access keys](https://s3-console.deltares.nl/access-keys) and suggest
   the format of the credentials file. It is a standard AWS credentials file.

## Command anatomy

One test case:
```bash
python -m tools.minio pull --case|--reference --config <config-path> --test-case-name <test-case-name> [--timestamp <timestamp>] --batch [--force]
```

Multiple test cases:
```bash
python -m tools.minio pull --case|--reference --test-case-file <testcase-file-path> --batch [--force]
```

Use `--case` to download just the _test case input_, and `--reference` to download the _test case references_.
The user may not actually supply a `config-path`, which is a required argument of `pull`. In this case,
use the `find-testbench-configs` skill to find a suitable config. `find-testbench-configs` may return multiple
configs. Let the user select one to use.
The MinIO tool is an interactive program, and so it sometimes prompts the user for what they want to do. Use the
`--batch` flag to turn off interactive input and always go with the default option for the prompts. Use in combination
with `--force` to force a `yes` to every `yes/no` prompts.
If the user provides a time or timestamp to download the test case data, you can pass the `--timestamp <timestamp>`
argument to the `pull` command. In this case, the timestamp will not be read from the config file. The `timestamp`
should be an ISO 8601 formatted timestamp (e.g. 2026-01-02T12:13:14.000Z) and take care to use the UTC timezone.
There is a way to download multiple test cases from multiple configs with the MinIO tools, but it requires
writing a _test case file_. It is a CSV file with two columns: `partial-testcase-name` and `config-path`.
Note that the `config-path` is relative to the `/test/deltares_testbench` and it supports glob patterns.
