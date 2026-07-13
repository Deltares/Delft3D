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
    MduTestData.h

    IssueReportTest.cpp
    MduDataConverterTest.cpp
    MduDataTest.cpp
    MduDocumentTest.cpp
    MduSchemaTest.cpp
    MduValidatorTest.cpp
    MduValueConverterTest.cpp
    StringUtilsTest.cpp
)

if(MSVC)
    target_compile_options(dflowfm_io_tests PRIVATE /bigobj)
endif()

target_link_libraries(dflowfm_io_tests PRIVATE dflowfm_io dflowfm_ini_parser GTest::gtest_main)

include(GoogleTest)
gtest_discover_tests(dflowfm_io_tests
    DISCOVERY_MODE PRE_TEST
)
