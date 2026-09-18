---
name: run-testbench
description: 'Run the Deltares Testbench (`TestBench.py`) on one or more test cases and inspect results.'
argument-hint: '[config-path] [testcase-filter1, testcase-filter2, ...]'
---

# Run Deltares Testbench

## When to use

- "Run the testbench on config X"
- "Run the testbench on test case(s) X [, Y, Z, ...]"

## What this skill does

Runs `test/deltares_testbench/TestBench.py` to verify that the output of a 
_test case model_ is still within tolerance of the _reference output_.

The test case data of the test cases is stored in our `delft3d-testbench` bucket on 
MinIO. This data includes the _test case input_ and the _reference output_. Both are
downloaded by `TestBench.py` before running a test case.

The `config-path` is a path to an XML file containing the test cases. Each test case
in the config has a `name`, `path`, `programs` and `checks`. The `path` of a test case
is stored in the `<path>` element. It is relative to
`/test/deltares_testbench/data/cases`. This is where the _test case data_ is downloaded.

A `TestBench.py` comparison run performs roughly follows these steps for each test case:

1. Download the _test case input_ and stores it in `/test/deltares_testbench/data/cases/${TEST_CASE_PATH}/input/`.

2. Download the _test case references_ and stores them in
`/test/deltares_testbench/data/cases/${TEST_CASE_PATH}/reference_{win,lnx}64/`.
Most test cases only use one set of reference data for both platforms. But there are test cases that have a separate set of reference files for Windows and Linux.

3. Make a copy of the input data to 
`/test/deltares_testbench/data/cases/${TEST_CASE_PATH}/input_work`
(the _work directory_) and runs the test case's list of _programs_ in the
_work directory_. This produces the output files in the work directory and leaves the
_input directory_ in a clean state.

4. Run the _checks_ for this test case. The checks are comparisons between the files
in the _work directory_ and the files in the _references_. `TestBench.py` supports many
types of comparisons. But the most common comparisons are between _NetCDF_ files. notably
the _his_ and _map_ files that `DFlowFM` produces as output. The checks on `.nc` are done
by taking the difference of one _variable_ present in both the _reference_ and _case_ 
output. The variables are (possibly multidimensional) arrays of numbers. The difference
not exceed the configured maximum allowed _absolute_ or _relative_ difference.

After running all the tests, `TestBench.py` prints the _result table_ with the result of
all checks in all test cases.

## Preconditions

1. The TestBench must be installed properly. *Always* check if the "venv" exists in
`/test/deltares_testbench/.venv` and run `TestBench.py` in the activated "venv". If
there is any errors finding `python`, activating the "venv", auth errors or if programs
can't be found: Please use the `install-testbench` skill to troubleshoot and install the
TestBench properly.

2. *Always* run `TestBench.py` from the `/test/deltares_testbench` directory. Otherwise it
runs into problems with relative paths.

## Command anatomy

```bash
python TestBench.py --compare --config <config-path> [--filter testcase=<testcase-filter1,testcase-filter2>] [--parallel]
```

The user may not actually supply a `config-path`, which is a required argument of
`TestBench.py`. In this case, use the `find-testbench-configs` skill to find a suitable
config. `find-testbench-configs` may return multiple configs. Let the user select one to
use. If there are multiple test cases to run, simply separate their names by commas and
use the `--filter testcase=<comma-sep-list>` argument. Use the `--parallel` flag when
running more then one test case in a single config.

## Interpreting `TestBench.py` results.
The _result table_ of the most recent `TestBench.py` run is stored in 
`/test/deltares_testbench/logs/testbench.log`.
Users are most interested in the test cases that contain `NOK` results, or `ERROR`
results. `ERROR` results usually indicate a crash, or some problem with the model input
or config. Look in the `/test/deltares_testbench/logs/${TEST_CASE_NAME}/` directory to
see if you can find any errors or stacktraces and let the user know if you find a problem
there. There may also be logs left in the _work directory_ of the test case.

`NOK` results signify that the comparison failed. The _result table_ will tell you which
check failed. In case it's a _NetCDF_ file (with `.nc` extension), it's usually a
difference above tolerance in a certain variable. You can use `ncdump` to inspect the
values (the `-v` flag is useful for this). `TestBench.py` also has `NetCDF4`, `numpy`
and `matplotlib` installed. So, if requested, you may write a script to plot results, 
provided you run it in the activated "venv".
