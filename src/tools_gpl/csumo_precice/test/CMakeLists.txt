enable_testing()
set(source_files test_csumo_precice_lib.cpp)
set(CMAKE_GTEST_DISCOVER_TESTS_DISCOVERY_MODE PRE_TEST)

add_executable(test_csumo_precice_lib ${source_files})

target_link_libraries(test_csumo_precice_lib PRIVATE compiler_warnings_settings csumo_precice_lib gtest)

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
set_target_properties(test_csumo_precice_lib PROPERTIES FOLDER tools_gpl/csumo_precice/test)

include(GoogleTest)
gtest_discover_tests(test_csumo_precice_lib)
