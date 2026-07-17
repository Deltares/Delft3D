---
name: run-testbench
description: 'Run the Deltares Testbench (`TestBench.py`) on one or more test cases.'
argument-hint: '[config-path] [testcase-filter1, testcase-filter2, ...]'
---

# Run Deltares Testbench

## What this skill does

Runs `test/deltares_testbench/TestBench.py` to verify that the output of a testcase model is still
within tolerance of the _reference output_.

## When to Use

- "Run the testbench" / "run a testbench case"
- "Run config X" / "run testcase Y"

The testbench `config-path` is mandatory and it should be a path to an `.xml` file. They are stored
somewhere in `/test/deltares_testbench/configs/` (usually in `/test/deltares_testbench/configs/dimr/`). 
The config files are not platform agnostic. There are separate files for Windows and Linux. The filename
of the config usually mentions `win64` or `lnx64` which indicates the platform. In addition, these configs
usually have _include_ tags, to include config snippets from other files. The config snippets are
usually stored in `/test/deltares_testbench/configs/include/`. By far the most common use case for this
is to let two separate configs (one for Windows, one for Linux) share the same set of test cases. The
included file contains the full list of test cases in this case.

If the user only specifies a test case name, and not a config-path, then please try to figure out which
config file the test case is in. Usually the test case name is somewhere in a config snippet in the
_include_ directory. And the config snippet is included in one of the configs in `configs/dimr`. Use
caution to use the right platform. And if there are multiple configs containing the test case, give the
user the option to select one.

If there is no `testcase-filter` specified, then `TestBench.py` will run all of the test cases in the
config file. Each test case has a corresponding `<testcase>` tag with a `name` attribute. To run only a 
subset of the test cases (or a single test case) you can specify a `testcase-filter`. You can specify
multiple testcase filters. The filtered test case names will be unioned together.

The testcase data is stored in our `minio` bucket. This data, including the test case input and
the reference output, will be downloaded as needed by `TestBench.py`.

## Preconditions

1. The working directory must be `/test/deltares_testbench/`. All paths in configs are relative to it.
2. Python 3.11 or higher must be installed. Some developers reported problems installing dependencies with
   Python 3.13 or higher. Prefer Python 3.12.
3. Virtual environment `.venv` exists and must be activate. *Always* activate it before running `TestBench.py`
4. Python dependencies must be installed. If not, run `uv pip sync pip/win-requirements.txt`
   in an activated venv. Or if `uv` is not installed: `pip install -r pip/win-requirements.txt`.
5. The credentials for downloading the testcase data in our `minio` bucket are installed in the user's
   home directory in the file `~/.aws/credentials`. If there are `minio`/`s3` auth errors please direct
   the user towards the `minio` 
   [UI page where they can create access keys](https://s3-console.deltares.nl/access-keys) and suggest
   the format of the credentials file. It is a standard AWS credentials file.
6. The Delft3D binaries must be installed. `TestBench.py` uses these to run the test cases. It expects to
   find them in the _engines directory_:
   - Windows: `/test/deltares_testbench/data/engines/teamcity_artifacts/x64/`
   - Linux: `/test/deltares_testbench/data/engines/teamcity_artifacts/lnx64/`
   If the binaries are missing, use the `build-delft3d` skill to build them. Unless explicitly specified, 
   build config `fm-suite` build-type `Release` mode and do a full _build_ including an _install_. 
   After the build finishes the _install directory_ will be here:
   - Windows: `/install_fm-suite`
   - Linux: `/build_fm-suite_release/install`
   If it doesn't already exist: Create a symbolic link from the _engines directory_ to the _install directory_. 
   Notice that the name of the sym-link is `x64` on Windows and `lnx64` on Linux. Prefer absolute paths when
   creating the link. On Windows creating a sym-link requires elevated privileges, so users need to run the 
   command as administrator.
7. The `config-path` must be a valid `.xml` file, and if a `testcase-filter` is specified it should match
   at least one test case in the config.

## Command anatomy

```bash
python TestBench.py --compare --config <config-file> [--filter testcase=<testcase-filter1,testcase-filter2>] [--parallel] [--skip-download {cases,references,dependency,all}]
```

When running more then one test case, use the `--parallel` flag. It is not necessary when running
a single test case.


