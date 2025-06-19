set(BUILD_GMOCK OFF)
set(INSTALL_GTEST OFF)
set(gtest_force_shared_crt ON)

add_subdirectory(googletest-main)

set_target_properties(gtest PROPERTIES FOLDER third_party_open/googletest)
set_target_properties(gtest_main PROPERTIES FOLDER third_party_open/googletest)
