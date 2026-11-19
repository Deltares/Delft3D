enable_testing()

# Generate cosumo_test_data.hpp from the authoritative XML example file.
file(READ "${CMAKE_CURRENT_SOURCE_DIR}/../docs/full_settings_example.xml" FULL_SETTINGS_XML)
configure_file(
    "${CMAKE_CURRENT_SOURCE_DIR}/cosumo_test_data.hpp.in"
    "${CMAKE_CURRENT_BINARY_DIR}/cosumo_test_data.hpp"
    @ONLY
)

set(source_files
    test_pre_c_sumo_lib.cpp
    test_precice_sink_source_connections.cpp
    test_precice_data.cpp
    test_precice_to_csumo.cpp
    test_csumo_to_precice.cpp
    test_csumo_diffuser.cpp
    test_csumo_file_paths.cpp
    test_file_io.cpp
    test_csumo_xml_utils.cpp
    test_csumo_settings_reader.cpp
    test_csumo_ff2nf_writer.cpp
    test_csumo_nf2ff_reader.cpp
    test_xml_parsing_utils.cpp
)

add_executable(test_pre_c_sumo_lib ${source_files})

target_link_libraries(test_pre_c_sumo_lib PRIVATE
    compiler_warnings_settings
    pre_c_sumo_lib
    GTest::gtest
    GTest::gtest_main
    precice::precice
    pugixml::pugixml
    Boost::headers
)

target_include_directories(test_pre_c_sumo_lib PRIVATE
    "${CMAKE_CURRENT_BINARY_DIR}"
)

if(WIN32)
    # Copy runtime DLLs to test directory for test discovery
    add_custom_command(TARGET test_pre_c_sumo_lib POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy -t $<TARGET_FILE_DIR:test_pre_c_sumo_lib> $<TARGET_RUNTIME_DLLS:test_pre_c_sumo_lib>
        COMMAND_EXPAND_LISTS
    )
endif()

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
set_target_properties(test_pre_c_sumo_lib PROPERTIES FOLDER tools_gpl/pre_c_sumo/test)

include(GoogleTest)
gtest_discover_tests(test_pre_c_sumo_lib)
