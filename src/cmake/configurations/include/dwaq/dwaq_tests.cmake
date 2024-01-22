if(NOT TARGET test_delwaq_dimr)
    add_subdirectory(${delwaq_tests_module} tests_delwaq)
    add_subdirectory(${utils_lgpl_tests_module} tests_utils_lgpl)
endif()