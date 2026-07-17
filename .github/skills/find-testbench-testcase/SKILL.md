---
name: find-testbench-testcase
description: 'Find which testbench config(s) contains a testcase'
argument-hint: '[partial-testcase-name ...] [platform]'
---

# Find testbench test cases

## What this skill does

Look up one or more configs containing a test case.

## When to Use

- "What config contains testcase X?"
- "Please find the configs for these testcases: X, Y, Z"

## Finding test cases and configs

Each `TestBench` testcase has a name with the following convention:
```e<engine_nr>_f<functionality_nr>_c<case_nr><trailer>```
Example: `e02_f001_c220_drypoints_pol`

On Linux, use `rg` in `/test/deltares_testbench/configs` to find an XML file containing the name. The
`partial-testcase-name` may match multiple test cases in a single file. Let the user know about all
of the matches. If `rg` isn't installed, you need to fall back to `grep` or `powershell`.

You'll most likely find matches in `/test/deltares_testbench/configs/include/<include-filename>.xml`. 
These XML files are not complete configs, but config snippets included with `<xi:include>`. To find the
actual config file, search again in `/test/deltares_testbench/configs/dimr/` with the `<include-filename>` 
found earlier. You will most likely find multiple configs. If the `platform` is not specified, choose the 
config with `lnx64` on Linux, and the one with `win64` on Windows. You may find more than two configs, because some
testcases have duplicates. Show all configs you find to the user.
