---
name: run-delft3d-unit-tests
description: 'Run the unit tests with CTest'
argument-hint: '[build-directory] [build-type] [test-name-pattern]'
---

# Run the Delft3D unit tests

## What this skill does
It runs the unit tests registered in the CMake configuration with CTest.

## When to use
- The source code has been changed. You happen to know that the code is covered by a few unit tests,
  and you want to know if these still pass.

## Preconditions
1. There should be a _build directory_ created by `cmake`. If not specified these can be found in the
   repository root and have the following naming convention:
   - Linux: `/build_<config>_<build-type>`
   - Windows: `/build_<config>`
   Where `config` is configuration that was specified when running the CMake _configure_. The default is
   `fm-suite` (which covers most products). If a build directory does not exist, use the `build-delft3d`
   skill to create it. Ask the user what `config` and `build-type` they want in this case.
2. The unit tests should be rebuilt after changing the source code. Otherwise the test results won't tell
   you anything. For faster iteration, you should use the `build-delft3d` skill with the `build-target`
   that contains the unit test executable.

## Command anatomy

```
ctest --test-dir <build-directory> [--build-config <build-type>] [--tests-regex <test-name-pattern>]
```

The `--build-config` is only required on Windows, because the Visual Studio solution files are build-type
agnostic.