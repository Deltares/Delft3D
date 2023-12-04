# Fortran Unit Test

## 1. Running Unit tests

- Windows: After building the source code, execute ctest in the build directory.
Then run ctest followed by the config.
```
cd build_all
ctest -C debug
```
- Linux: After building, it is necessary to extend the $LD_LIBRARY_PATH to include the install/lib directory, so the test executables can find the compiled *.so's.
Here the `build_dir` is the directory where the source code is built (i.e. build_all, build_waq, etc.).
```
export LD_LIBRARY_PATH=/<path-where-repo-exist>/<build-dir>/install/lib:$LD_LIBRARY_PATH
cd build_all
ctest -C debug
```

## 2. Create Unit tests
- The unit tests are located in the `test/unit_test` directory. The subdirectories are structured in the
same way as the src directory.
- To test a certain subroutine or function that is located in a module, create a file with the same name as the
subroutine or function in the `test/unit_test` directory.
```
    test/unit_test/engines_gpl
    ├── engine-1
    │   ├── CMakeLists.txt
    │   ├── package-1
    │   └── package-2
    │       ├── CMakeLists.txt
    │       ├── data
    │       │   ├── file_1.txt
    │       │   └── file_2.txt
    │       ├── tests_sub_routine_1.f90
    │       └── tests_sub_routine_2.f90
    └── engine-2
        ├── package-1
        │   ├── CMakeLists.txt
        │   ├── data
        │   │   ├── file_1.txt
        │   │   └── file_2.txt
        │   ├── tests_sub_routine_1.f90
        │   └── tests_sub_routine_2.f90
        └── package-2
            ....
            └── tests_sub_routine_1.f90
```
### 2.1 CMakelists.txt
- In the package folder.
```
    test/unit_test/engines_gpl
    ├── waq
    │   ├── CMakeLists.txt
    │   ├── package-1
    │   │   ├── CMakeLists.txt  <------(Here)
    │   │   ....
    │   ├── package-2
    │   │   ├── CMakeLists.txt
            ...
```
- Create a CMakelists.txt file that contains the following lines:

```cmake
# test dependencies
set(dependencies target1 target2 ftnunit)

create_test(
    tests_sub_routine_1
    dependencies ${dependencies}
    visual_studio_folder tests/unit_test/engines_gpl/waq/package_1 
    test_files tests_sub_routine_1.f90
    include_dir ${CMAKE_CURRENT_SOURCE_DIR}/data/
)

set(dependencies target1 target2 ftnunit)

create_test(
    tests_sub_routine_2
    dependencies ${dependencies}
    visual_studio_folder tests/unit_test/engines_gpl/waq/package_1
    test_files tests_sub_routine_2.f90
    include_dir ${CMAKE_CURRENT_SOURCE_DIR}/data/
)
```

First, define the dependencies of the test, these are the targets that are needed to build the test.
Use the `create_test` cmake function to create the test by providing the following arguments:
- The test name should include be descriptive that it is easy to identify what is the purpose of the test and what 
  it is testing.
- The second argument is the dependencies.
- The `visual_studio_folder` argument defines the folder in which the test will be located in the visual studio 
  solution.
- The `test_files` argument defines the files that will be compiled to create the test.
- The `include_dir` argument defines the directory that contains the files that are needed for the test. The 
  `include_dir` argument is optional, if the test does not depend on external data, the argument does not have to be provided. 

- The test name does not have to contain the module name it is testing however can do so in order to be able to filter the tests using regex.

### 2.2 Data

In the data folder put all the files that are needed for the test.
2.3 Test file
In the Fortran code to access the data you stored in the `data/` folder, you can use the
`get_environment_variable` function. This function will return the absolute path to the directory where the data will be during test runtime.

```fortran
program tests_sub_routine_1
	use ftnunit
	use m_dlwq13, only: dlwq13

	implicit none
	character(len=200) :: file_path, data_path


	! Get the DATA_PATH environment variable
	call get_environment_variable("DATA_PATH", data_path)
	file_path = trim(data_path)//'/test_data.ref'
	open ( 10, file = file_path)
	! ....
end program tests_sub_routine_1
```

end program tests_waq_sub_dir_2_module_1
# 3. Module CMakeLists
In the CMakeLists.txt that is in the main directory of the delft3D engine (i.e. waq/part/...) `test/unit_test/waq/CMakeLists.txt`.
```
    test/unit_test/engines_gpl
    ├── engine-1
    │   ├── CMakeLists.txt <------(This one)
    │   ├── package-1
    │   │   ├── CMakeLists.txt
    │   │   ....
    │   ├── package-2
    │   │   ├── CMakeLists.txt
            ...
```
First, you have to insert the include statement referring to the CMakeLists.txt file where the targets/dependencies are that you need in the test.
Second, if you are adding tests to a package that was not tested before then use the add_subdirectory command to refer to the sub-dir you want to test.
```cmake
if(NOT TARGET ftnunit)
    add_subdirectory(${checkout_src_root}/${ftnunit_module} ftnunit)
endif()

# D-Waq kernel
include(${dwaq_dir}/dwaq_kernel.cmake)

# D-Waq tools
include(${dwaq_dir}/dwaq_tools.cmake)

# Here place the 
add_subdirectory(package-1 test_package_1) #package-1 referes to the name of the folder of package-1
add_subdirectory(package-2 test_package_2) #package-2 referes to the name of the folder of package-2
```