# Deltares
This is an adapted and stripped-down version of [f90tw](https://github.com/loudove/f90tw). The original library did not work on Windows because the PowerShell commands failed. Furthermore, since we only use Google Test, Boost Test was removed as an option.
The original library provided two options for implementing tests: either through direct use of preprocessor macros in a .fpp file or via !$f90tw directives in a .f90 file. The latter implementation follows standard Fortran more closely and allows for easier use of Fortran formatters and library include statements. Therefore, the macro implementation was removed from this version.
The CMake code was simplified to make it easier for users of the library. The CMake function `gtest_discover_tests` is used to automatically register tests in CTest.

# f90tw

The f90tw project provides Fortran wrappers for a limited subset of the [Google Test](https://github.com/google/googletest) framework functionality. It offers a simple mechanism for setting up and managing test suites in Fortran projects. The f90tw implementation follows a preprocessor-based approach, with the goal of utilizing already available test frameworks and assertion implementations with minimal effort.

## Usage

Tests are implemented using standard Fortran with the `!$f90tw` directive (`[!cC]\$[fF]90[tT][wW]` in regular expression) to indicate the test cases (i.e., the subroutines to be wrapped). The lines with the directives are extracted and included in the C/C++ counterpart to create the test suite.

### Example

```fortran
module test_example
   use assertions_gtest
   use my_module_to_test
   implicit none

contains

   !$f90tw TESTCODE(TEST, test_suite, test_addition, test_addition,
   subroutine test_addition() bind(C)
      use precision, only: dp
      real(kind=dp), parameter :: tolerance = 1e-8_dp

      call f90_assert_eq(1 + 1, 2)
      call f90_expect_near(1.0_dp, 1.1_dp, 0.2_dp, "values should be close")
   end subroutine test_addition
   !$f90tw)

end module test_example
```

### Google Test Assertions

The following subset of the Google Test framework is supported with `<level>` : (ASSERT|EXPECT):

| Fortran (F90_`<level>`_`<operator>`) | operator | argument types |
|---------------------------------------|----------|----------------|
| F90_`<level>`_`<operator>` | (EQ\|NE) | logical(KIND=(C_BOOL\|4)), integer(KIND=C_INT), real(KIND=C_FLOAT), real(KIND=C_DOUBLE) |
| F90_`<level>`_`<operator>` | (GT\|GE\|LT\|LE) | integer(KIND=C_INT), real(KIND=C_FLOAT), real(KIND=C_DOUBLE) |
| F90_`<level>`_FLOAT_EQ | - | real(KIND=C_FLOAT) |
| F90_`<level>`_DOUBLE_EQ | - | real(KIND=C_DOUBLE) |
| F90_`<level>`_NEAR | - | real(KIND=C_FLOAT), real(KIND=C_DOUBLE) |
| F90_`<level>`_`<operator>` | (TRUE\|FALSE) | logical(KIND=(C_BOOL\|4)) |
| F90_`<level>`_`<operator>` | (STREQ\|STRNE\|STRCASEEQ\|STRCASENE) | character(KIND=C_CHAR,LEN=*) |

**Note:** For logical types, you can use either `logical(KIND=C_BOOL)` or the default Fortran `logical` type. F90TW automatically provides type conversion when needed.

All assertion methods can be used with a message as the last argument (of type `character(KIND=C_CHAR,LEN=*)`) to override the default assertion message.

## CMake Integration

To use F90TW in your project, use the `F90TWTEST` function:

```cmake
F90TWTEST(test_name
    CFILES test_main.cpp
    F90FILES test_module.f90
    F2HFILES test_module.f90
    LIBRARIES my_library
)
```

For more details, please check the [Google Test](https://github.com/google/googletest) documentation.
