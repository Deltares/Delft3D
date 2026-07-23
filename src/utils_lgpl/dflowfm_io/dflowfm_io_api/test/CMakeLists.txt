enable_testing()

cmake_path(SET PYTHON_TEST_DIR NORMALIZE "${CMAKE_CURRENT_SOURCE_DIR}/../wrappers/python/tests")
find_package(Python3 COMPONENTS Interpreter)
if(Python3_FOUND)
    # The wrapper loads the DLL from its bundled _lib/, which the dflowfm_io_api POST_BUILD step
    # stages after every compile, so the test needs no library-location hint.
    add_test(
        NAME dflowfm_io_python_tests
        COMMAND ${Python3_EXECUTABLE} -m unittest discover -s ${PYTHON_TEST_DIR} -p "test_*.py" -v
    )
endif()
