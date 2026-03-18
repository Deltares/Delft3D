enable_testing()

# Generate cosumo_test_data.hpp from the authoritative XML example file.
file(READ "${CMAKE_CURRENT_SOURCE_DIR}/../docs/full_settings_example.xml" FULL_SETTINGS_XML)
configure_file(
    "${CMAKE_CURRENT_SOURCE_DIR}/cosumo_test_data.hpp.in"
    "${CMAKE_CURRENT_BINARY_DIR}/cosumo_test_data.hpp"
    @ONLY
)

set(source_files
    test_csumo_precice_lib.cpp
    test_csumo_settings_reader.cpp
)

add_executable(test_csumo_precice_lib ${source_files})

target_link_libraries(test_csumo_precice_lib PRIVATE
    compiler_warnings_settings
    csumo_precice_lib
    gtest
)

target_include_directories(test_csumo_precice_lib PRIVATE
    "${CMAKE_CURRENT_BINARY_DIR}"
)

if(WIN32)
    # Copy runtime DLLs to test directory for test discovery
    add_custom_command(TARGET test_csumo_precice_lib POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy -t $<TARGET_FILE_DIR:test_csumo_precice_lib> $<TARGET_RUNTIME_DLLS:test_csumo_precice_lib>
        COMMAND_EXPAND_LISTS
    )
endif()

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
set_target_properties(test_csumo_precice_lib PROPERTIES FOLDER tools_gpl/csumo_precice/test)

include(GoogleTest)
gtest_discover_tests(test_csumo_precice_lib)
