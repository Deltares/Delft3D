if(WIN32)
    # On Windows, build Google Test from source
    set(BUILD_GMOCK OFF)
    set(INSTALL_GTEST OFF)
    set(gtest_force_shared_crt ON)

    add_subdirectory(googletest-main)

    set_target_properties(gtest PROPERTIES FOLDER third_party_open/googletest)
    set_target_properties(gtest_main PROPERTIES FOLDER third_party_open/googletest)
else()
    # On Linux, use CMake's built-in FindGTest to find system-installed Google Test
    find_package(GTest REQUIRED)
    if(NOT TARGET gtest)
        add_library(gtest ALIAS GTest::gtest)
        add_library(gtest_main ALIAS GTest::gtest_main)
    endif()
endif()
