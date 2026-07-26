---
name: find-testbench-configs
description: 'Find which `TestBench.py` config(s) contains a test case'
argument-hint: '[partial-testcase-name ...] [platform]'
---

# Find testbench config(s) containing test case(s)

## What this skill does

Look up one or more configs containing one or multiple test cases.

## When to use

- "What config contains test case X?"
- "Please find the configs for these test cases: X, Y, Z"

## Finding test cases and configs

Each `TestBench` test case has a name with the following convention:
```e<engine_nr>_f<functionality_nr>_c<case_nr><trailer>```
Example: `e02_f001_c220_drypoints_pol`

The `engine_nr` identifies the simulation engine, the `functionality_nr` groups test cases that test
the same functionality, and each individual test case should have a `case_nr`. The trailer is a brief
description of the test case.

On Linux, use `rg` in `/test/deltares_testbench/configs` to find an XML file containing the name. The
`partial-testcase-name` may match multiple test cases in a single config. Let the user know about all
of the matches. If `rg` isn't installed, you need to fall back to `grep` or `powershell`.

You'll most likely find matches in `/test/deltares_testbench/configs/include/<include-filename>.xml`. 
These XML files are not complete configs, but config snippets included with `<xi:include>`. 
To find the actual config file, search again in `/test/deltares_testbench/configs/dimr/` with the 
`<include-filename>` found earlier. You will most likely find multiple configs. If the `platform` is not
specified, choose the config with `lnx64` on Linux, and the one with `win64` on Windows. 
You may still find more than two configs, because some test cases are included in multiple configs. 
In this case report all configs you find to the user.
