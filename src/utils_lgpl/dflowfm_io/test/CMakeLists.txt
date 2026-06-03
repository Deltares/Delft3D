include(FetchContent)
FetchContent_Declare(
    googletest
    GIT_REPOSITORY https://github.com/google/googletest.git
    GIT_TAG v1.14.0
)
FetchContent_MakeAvailable(googletest)

enable_testing()

add_executable(dflowfm_io_tests
    MduModelTest.cpp
)

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
