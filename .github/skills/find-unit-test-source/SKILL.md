---
name: find-unit-test-source
description: 'Find the source file(s) for a (failing) CTest/GoogleTest unit test by name'
argument-hint: '<test-name> [build-directory]'
---

# Find the source code for a unit test

## What this skill does
Maps a CTest test name (as reported by `ctest`, e.g. from a failing test) to the actual
source file(s) that define it, instead of falling back to a broad repository-wide grep.

## When to use
- A unit test failed and you need to open/edit the source file that implements it.
- You only have the test name from `ctest` or CI output, e.g.
  `test_deltares_common_gtest.test_dp_equal_within_tolerance` or `dsle_test_sealock`.

## Steps

1. **Resolve the test executable/target name.**
   CTest test names come in two shapes:
   - GoogleTest style: `<Target>.<TestCase>`. The part before the first `.` is usually
     the CMake target name.
   - Plain `add_test(NAME ...)` style, e.g. `dsle_test_sealock`, where the `NAME` doesn't
     necessarily match the `COMMAND` executable.

   Resolve the real executable with:
   ```
   ctest --test-dir <build-directory> --tests-regex '^<test-name>$' --show-only=json-v1 \
    | jq '[.tests[] | { name: .name, target: (.command[0] | split("/") | last) }]'
   ```
   This produces a list of test name and target pairs.

2. **Locate the `CMakeLists.txt` that registers this target.**
   ```
   rg -l '<target-name>' src/**/CMakeLists.txt
   ```
   You'll find one of these patterns:
   - `f90twtest(<target> CFILES ... F90FILES ... F2HFILES ...)` — source files are listed
     directly as arguments.
   - `add_executable(<target> ...)` plus a nearby `gtest_discover_tests(<target>)`.
     Sources are listed in the `add_executable` call.
   - `add_test(NAME <test-name> COMMAND <target> ...)`. A separate small test
     executable; find its `add_executable(<target> ...)` in the same file or a sibling
     `tests/`/`test/` directory.

3. **If sources aren't listed inline**, look for a `tests/` or `test/` subdirectory next
   to the `CMakeLists.txt`. this repo's convention is to keep test sources there.

4. **For a specific failing GoogleTest case** (not just the suite), grep the located
   source file(s) for `TEST(<Suite>, <Case>` or `TEST_F(<Suite>, <Case>` to jump straight
   to the failing assertion.

## Example
Failing test: `test_deltares_common_gtest.test_dp_equal_within_tolerance`
- Target: `test_deltares_common_gtest`
- `rg -l test_deltares_common_gtest src/**/CMakeLists.txt` ->
  `src/utils_lgpl/deltares_common/packages/deltares_common/CMakeLists.txt`
- That file's `f90twtest(...)` call lists
  `CFILES ${gtest_path}/test_deltares_common_gtest.cpp` and
  `F90FILES ${gtest_path}/test_deltares_common_gtest.f90`. Open the `.f90` file directly.
