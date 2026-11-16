find_package(GTest CONFIG REQUIRED)
include(GoogleTest)

add_library(gtest_fortran_assertions OBJECT src/assertions_gtest.f90)
add_library(gtest_fortran_wrapper STATIC src/gtest_fortran_wrapper.cpp)
set_target_properties(gtest_fortran_assertions gtest_fortran_wrapper PROPERTIES
    POSITION_INDEPENDENT_CODE ON
    FOLDER tools_gpl/gtest_fortran_wrapper
)
target_include_directories(gtest_fortran_wrapper PRIVATE include)
target_link_libraries(gtest_fortran_wrapper PUBLIC gtest_fortran_assertions GTest::gtest)

include(cmake/GTestFortranWrapper.cmake)
add_subdirectory(test)
