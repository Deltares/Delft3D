enable_testing()

cmake_path(SET PYTHON_TEST_DIR NORMALIZE "${CMAKE_CURRENT_SOURCE_DIR}/../wrappers/python/tests")
find_package(Python3 COMPONENTS Interpreter)
if(Python3_FOUND)
    add_test(
        NAME dflowfm_io_python_tests
        COMMAND ${Python3_EXECUTABLE} -m unittest discover -s ${PYTHON_TEST_DIR} -p "test_*.py" -v
    )
    set_tests_properties(dflowfm_io_python_tests PROPERTIES
        ENVIRONMENT "DFLOWFM_IO_LIB_DIR=$<TARGET_FILE_DIR:dflowfm_io_api>"
    )
endif()
