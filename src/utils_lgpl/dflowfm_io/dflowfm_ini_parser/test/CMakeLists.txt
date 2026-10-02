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

add_executable(dflowfm_ini_parser_tests
    IniParserTest.cpp
    IniValueConverterTest.cpp
    IniDataTest.cpp
    IniFormatterTest.cpp
    IniSectionTest.cpp
    IniPropertyTest.cpp
    IniFileTest.cpp
)

if(MSVC)
    target_compile_options(dflowfm_ini_parser_tests PRIVATE /bigobj)
endif()

target_link_libraries(dflowfm_ini_parser_tests PRIVATE dflowfm_ini_parser GTest::gtest_main)

include(GoogleTest)
gtest_discover_tests(dflowfm_ini_parser_tests
    DISCOVERY_MODE PRE_TEST
)
