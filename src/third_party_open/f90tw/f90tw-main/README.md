# Deltares
This is an adapted and stripped down version of f90tw. The original library did not work on windows, because the powershell commands failed. Furthermore, we only use googletest, so boost test was removed as an option.
Then, the library provided two options to provide the tests, either through direct use of preprocessor macros in a .fpp file or via !$f90tw directives in a .f90 file. The latter implementation follows standard Fortran
more closely and allows for the easy use of Fortran formatters and include statements of libraries. Therefore, the macro implementation was removed from the library.
The CMake code was simplified so that it is simpler for the user of the library. The CMake function `gtest_discover_tests` was used to automatically register the tests in CTest.

# f90tw

f90tw project provides fortran wrappers for a limited subset of the [google](https://github.com/google/googletest) test framework functionality. At the same time, offers a rather simple mechanism for setting up and managing test suites in fortran projects. All the way down, f90tw implementation follows a preprocessor-based approach. The motivation was to utilize already available test frameworks and assertions implementation with minimal effort.

### Preprocessor macros

In the fortran code file, we first include the header [`"f90tw_test.h"`](./f90tw/f90tw_test.h) and then, we implemented the test module with the help of f90tw preprocessor macros:

- **TESTMODULE**( MODULENAME, MODULETOTEST): (*optional*) prepares the initial statements of the module(module declaration, use of the module to be tested and set `implicit none`).
  - MODULENAME: the name of the module
  - MODULETOTEST: the name of the module to test

- **TESTCONTAINS**: (*optional*) expands to fortran `contain` statement. It is
separated from TESTMODULE macro to allow for the declaration of module resources common to all tests.

- **TESTCODE**( *TESTTYPE*, *TESTSUITENAME*, *TESTNAME*, *SUBNAME*, **...** ): prepares the specific test.
  - *TESTTYPE*: the framework macro to be used for test declaration
  - *TESTSUITENAME*: the name of the test suite
  - *TESTNAME*: the name of the test
  - *SUBNAME*: the name of the fortran subroutine to be implemented
  - **...** : the rest of the arguments which are essentially, the fortran source code implementing the test.
    **PLEASE NOTE** that each code line should be terminated with `";"`. The use of pound character (`"#"`) in fortran string renders preprocessing impossible, while, in order to avoid similar catastrophic errors, the string concatenation operator (`//`) should be replaced with the `F90CONCAT` macro and the line continuation operator (`&`) with the `F90LNCONT` macro. Moreover, `F90SPOT` macro expands to the `filename:line` string, while `F90SPOTMSG(MSG)` macro appends the MSG string to it. You can simplify the implementation by just calling a fortran method implemented elsewhere. This approach is maybe preferable since you will avoid the drawbacks of preprocessing relevant to code's clarity and debugging.

- **ENDTESTMODULE**(MODULENAME)  : (*optional*) module end statement.
  - MODULENAME: the name of the module (the same with the one used with with **TESTMODULE** macro).

Using this approach, the c++ implementation becomes rather fast and easy since it is based on preprocessing the same fortran file, with different definitions of the macros resulting automatically in a) the declarations of the fortran test methods and b) the framework tests which essentially wrap these methods. A more detailed description of the fortran and c/c++ files is provided [here](./examples/README.md).

### Directives

An alternative approach is to implement the test using standard fortran (see [`test_example_gtest.f90`](./examples/test_example_gtest.f90)) and the `!$f90tw` directive (`[!cC]\$[fF]90[tT][wW]]` in regular expression) to indicate the test cases i.e. the subroutines to be wrapped. Essentially, the lines with the directives will be extracted and included in the c/c++ counterpart ((see [`test_example_gtest.cpp`](./examples/test_example_gtest.cpp))) in order to create the test suite. A more detailed description of the fortran and c/c++ files is provided [here](examples/README.md).

## Assertions tests

In addition to the assertion wrappers for [gtest](./README.md#gtest-assertions-tests), a method for accessing the f90tw version is available:

- f90tw_version(*major*, *minor*, *patch*)
  - *major* : major version
  - *minor* : minor version
  - *patch* : patch version

### Gtest assertions tests

The following subset of gtest framework is supported with &lt;level&gt; : (ASSERT&#124;EXPECT) :

Fortran (F90_&lt;C/C++&gt;) | operator | argumens type
--------------------------- | -------- | -------------
F90_&lt;level&gt;_&lt;operator&gt; |  (EQ&#124;NE) | logical(KIND=(C_BOOL&#124;4)),<br> integer(KIND=C_INT),<br> real(KIND=C_FLOAT),<br> real(KIND=C_DOUBLE)
F90_&lt;level&gt;_&lt;operator&gt; |  (GT&#124;GE&#124;LT&#124;LE) | integer(KIND=C_INT),<br> real(KIND=C_FLOAT),<br> real(KIND=C_DOUBLE)
F90_&lt;level&gt;_FLOAT_EQ <sup>(*)</sup> |  - | real(KIND=C_FLOAT)
F90_&lt;level&gt;_DOUBLE_EQ <sup>(*)</sup> |  - | real(KIND=C_DOUBLE)
F90_&lt;level&gt;_NEAR <sup>(*)</sup> |  - | real(KIND=C_FLOAT),<br> real(KIND=C_DOUBLE)
F90_&lt;level&gt;_&lt;operator&gt; |  (TRUE&#124;FALSE) <sup>(*)</sup> | logical(KIND=(C_BOOL&#124;4))
F90_&lt;level&gt;_&lt;operator&gt; |  (STREQ&#124;STRNE&#124;<br>STRCASEEQ&#124;STRCASENE) | character(KIND=C_CHAR,LEN=*)

All the assertion methods can be used with a message as the last argument (of type character(KIND=C_CHAR,LEN=*) ) in order to overwrite the default assertion message.

For more details please check the [gtest](https://github.com/google/googletest) documentation.
