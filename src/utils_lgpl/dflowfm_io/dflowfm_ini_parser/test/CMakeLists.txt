if(NOT TARGET GTest::gtest_main)
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

if(WIN32)
    add_custom_command(TARGET dflowfm_ini_parser_tests POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy -t $<TARGET_FILE_DIR:dflowfm_ini_parser_tests> $<TARGET_RUNTIME_DLLS:dflowfm_ini_parser_tests>
        COMMAND_EXPAND_LISTS
        VERBATIM
    )
endif()

include(GoogleTest)
gtest_discover_tests(dflowfm_ini_parser_tests
    DISCOVERY_MODE PRE_TEST
)
