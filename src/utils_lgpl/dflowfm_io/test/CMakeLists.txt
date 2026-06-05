if(NOT TARGET gtest)
    include(FetchContent)
    FetchContent_Declare(
        googletest
        GIT_REPOSITORY https://github.com/google/googletest.git
        GIT_TAG v1.14.0
    )
    set(INSTALL_GTEST OFF CACHE BOOL "" FORCE)
    FetchContent_MakeAvailable(googletest)
endif()

enable_testing()

add_executable(dflowfm_io_tests
    MduModelTest.cpp
    ini/IniParserTest.cpp
    ini/IniValueConverterTest.cpp
    ini/IniDataTest.cpp
    ini/IniFormatterTest.cpp
    ini/IniSectionTest.cpp
    ini/IniPropertyTest.cpp
    ini/IniFileTest.cpp
)

if(MSVC)
    target_compile_options(dflowfm_io_tests PRIVATE /bigobj)
endif()

target_link_libraries(dflowfm_io_tests PRIVATE dflowfm_io GTest::gtest_main)

# Copy the shared library next to the test executable so it can be found at runtime
add_custom_command(TARGET dflowfm_io_tests POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
        $<TARGET_FILE:dflowfm_io>
        $<TARGET_FILE_DIR:dflowfm_io_tests>
)

include(GoogleTest)
gtest_discover_tests(dflowfm_io_tests
    DISCOVERY_MODE PRE_TEST
)

# Python tests
cmake_path(SET PYTHON_TEST_DIR NORMALIZE "${CMAKE_CURRENT_SOURCE_DIR}/../wrappers/python/test")
find_package(Python3 COMPONENTS Interpreter)
if(Python3_FOUND)
    add_test(
        NAME dflowfm_io_python_tests
        COMMAND ${Python3_EXECUTABLE} -m unittest discover -s ${PYTHON_TEST_DIR} -p "test_*.py" -v
    )
    set_tests_properties(dflowfm_io_python_tests PROPERTIES
        ENVIRONMENT "DFLOWFM_IO_LIB_DIR=$<TARGET_FILE_DIR:dflowfm_io>"
    )
endif()
