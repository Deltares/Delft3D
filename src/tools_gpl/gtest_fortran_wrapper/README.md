# GoogleTest Fortran wrapper

Write Fortran unit tests using GoogleTest assertions, and run them through CTest
or Visual Studio Test Explorer.

## How to use it

### Write a test

```fortran
module test_example
   use assertions_gtest, only: f90_expect_eq
   implicit none(type, external)
contains
   !$gtest test
   subroutine addition() bind(C)
      call f90_expect_eq(1 + 1, 2, "ordinary Fortran message")
   end subroutine addition
end module test_example
```

Only subroutines marked with `!$gtest test` are registered. They must have no
arguments, use `bind(C)`, and have unique C binding names. Keep markers on their
own lines; conditional preprocessing of marked declarations is not supported.

The default test name is `module_name.procedure_name`. To override it, use
`!$gtest test suite=OtherSuite name=PublicName`. A module-level
`!$gtest suite=PublicSuite` sets the suite for that module's tests.

### Register the source in CMake

Add the file to an existing test target's `SOURCES`, or define a new target:

```cmake
add_fortran_gtest(test_example
    SOURCES test_example.f90
    LIBRARIES library_under_test
    VISUAL_STUDIO_PATH tests/example
)
```

`SOURCES` drives both compilation and test registration; no C++ bootstrap is
needed. Optional arguments are `INCLUDES`, `DEFINITIONS`, `CXX_SOURCES` for C++
helpers, and `TEST_TEARDOWN c_binding_name` for cleanup after every test, including
failures. D-Flow FM's existing target uses teardown to reset model state.

### Check results and guard prerequisites

Use `call f90_expect_<suffix>(...)` for independent result checks. Failures are
nonfatal, so subsequent checks still run.

| Suffix | Types | Ranks |
|---|---|---|
| `eq`, `ne` | integer, `real(sp)`, `real(dp)`, logical | scalar |
| `lt`, `le`, `gt`, `ge` | integer, `real(sp)`, `real(dp)` | scalar |
| `float_eq` | `real(sp)` | scalar, 1, 2 |
| `double_eq` | `real(dp)` | scalar, 1, 2 |
| `near` | `real(sp)`, `real(dp)` | scalar, 1, 2 |
| `true`, `false` | logical, integer | scalar, 1, 2 |
| `streq`, `strne`, `strcaseeq`, `strcasene` | character strings | scalar |

Integers are 32-bit; logicals support both the default kind and `c_bool`.
Import `sp`/`dp` from `precision`, and C kinds from `iso_c_binding` when needed.
`near` takes a scalar absolute tolerance of the same kind as its operands.
Array checks report failing element indices.

All checks accept an optional final message. Ordinary Fortran strings need no
NUL terminator; legacy NUL-terminated strings are also accepted. Use `trim` if
trailing blanks should not be compared.

For prerequisites, use the logical functions `f90_assert_eq` (scalar integers)
or `f90_assert_true` (scalar logicals or integers, where nonzero is true), with an
explicit return on failure:

```fortran
if (.not. f90_assert_true(allocated(values), "values must be allocated")) then
   return
end if
if (.not. f90_assert_eq(size(values), 3, "unexpected size")) then
   return
end if
call f90_expect_near(values(1), 2.0_dp, 1.0e-6_dp)
```

These functions record fatal failures and return whether that comparison passed.
Check allocation before `size` or indexing: Fortran does not guarantee
short-circuit evaluation. Clean up resources and restore state before returning.
A helper's return stops only that helper, so propagate failure to its caller.

Assertions are functions, not subroutines: use their logical result in an
explicit guard rather than `call f90_assert_*`.

### Build and run

On Windows, configure, build, install, and select a test with CTest:

```powershell
python build.py --config fm-suite --build-type Debug --keep-build
cmake --build build_fm-suite --config Debug --parallel
cmake --install build_fm-suite --config Debug
ctest --test-dir build_fm-suite --build-config Debug -R "^test_example[.]addition$" --output-on-failure
```

Run the install step after building and before testing; some tests require
installed DLLs.

On Linux, use `build_fm-suite_debug` as the build directory.

Tests run from their executable's build directory. Run serially because some
tests share files. See [unit testing](../../../doc/unit-testing.md) for broader
test-suite guidance.

## Origins and how it works

This Delft3D-owned wrapper replaces the former F90TW integration. Its Fortran
assertion module and C++ GoogleTest adapter are maintained directly, without
preprocessor-generated assertion code. Retained F90TW attribution is in
[`LICENSE.f90tw`](LICENSE.f90tw).

Python reads the `!$gtest` markers from `SOURCES` and generates C++ test
registrations and `main`. The Fortran module calls GoogleTest through a small C
interface; GoogleTest handles execution, diagnostics, and test discovery.
On Windows, each executable has its own output directory to avoid DLL-copy races.
