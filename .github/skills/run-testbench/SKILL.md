---
name: run-testbench
description: 'Run the Deltares Testbench (`TestBench.py`) on one or more test cases and inspect results.'
argument-hint: '[config-path] [testcase-filter1, testcase-filter2, ...]'
---

# Run Deltares Testbench

## When to Use

- "Run the testbench on config X"
- "Run the testbench on testcase(s) X [, Y, Z, ...]"

## What this skill does

Runs `test/deltares_testbench/TestBench.py` to verify that the output of a testcase model is still
within tolerance of the _reference output_.

The testcase data of the test cases is stored in our `minio` bucket. This data, which includes the 
_test case input_ and the _reference output_, will be downloaded by `TestBench.py` before running a test case.

A `TestBench.py` comparison run performs roughly follows these steps for each test case:
1. Download the _test case input_ and stores it in `/test/deltares_testbench/data/cases/<test-case-name>`.
2. Download the _test case references_ and stores them in
   `/test/deltares_testbench/data/{reference_results,reference}/{win64,lnx64}/<test-case-name>`.
   It is one of the four combinations, depending on what's configured in the XML config.
   Most test cases only use one set of reference data for both platforms. But there are test cases
   that have a separate set for Windows and Linux.
3. Make a copy of the input data to `/test/deltares_testbench/data/cases/<test-case-name>_work`
   and runs the test case's list of _programs_ on the input data. This produces output files in the
   work directory.
4. Run the _checks_ for this test case. The checks are comparisons between the files in the _work
   directory_ and the files in the _references_. Based on file type, `TestBench.py` supports many
   types of comparisons. But the most common comparisons are between _NetCDF_ files. Most notably
   the _his_ and _map_ files that "DflowFM" produces as output. The results are most often numerical,
   and the results can differ up to a specified absolute or relative tolerance.

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
   If it doesn't already exist: Create a sym-link from the _engines directory_ to the _install directory_. 
   Notice that the name of the sym-link is `x64` on Windows and `lnx64` on Linux. Prefer absolute paths when
   creating the link. On Windows creating a sym-link requires elevated privileges, so users need to run the 
   command as administrator.
7. The `config-path` must be a valid `.xml` file, and if a `testcase-filter` is specified it should match
   at least one test case in the config.

## Command anatomy

```bash
python TestBench.py --compare --config <config-path> [--filter testcase=<testcase-filter1,testcase-filter2>] [--parallel]
```

The user may not actually supply a `config-path`, which is a required argument. In this case, use the 
`find-testbench-testcase` skill to find a suitable config. `find-testbench-testcase` may return multiple
configs. Let the user select one to use. If there are multiple testcases to run, simply separate their names
by commas and use the `--filter testcase=<comma-sep-list>` argument. Use the `--parallel` flag when running 
more then one testcase in a single config.

## Interpreting `TestBench.py` results.
The _result table_ of the most recent `TestBench.py` run is stored in `/test/deltares_testbench/logs/testbench.log`.
Users are most interested in the test cases that contain `NOK` results, or `ERROR` results. `ERROR` results usually
indicate a crash, or some problem with the model input or config. Look in the _logs_ directory for that test case to
see if you can find any errors or stacktraces.

`NOK` results signify that the comparison failed. The _result table_ will tell you which check failed. In case it's
a _NetCDF_ file (with `.nc` extension), it's usually a difference above tolerance in a certain variable. In Linux you
can use `ncdump` to inspect the values (the `-v` flag is useful for this). `TestBench.py` also has `NetCDF4`, `numpy`
and `matplotlib` installed. So, if requested, you may write a script to plot results, provided you run it in the 
activated venv.
