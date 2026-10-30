find_package(Python COMPONENTS Interpreter REQUIRED)

if(NOT TARGET gtest_fortran_wrapper)
    add_subdirectory(${checkout_src_root}/tools_gpl/gtest_fortran_wrapper gtest_fortran_wrapper)
endif()
