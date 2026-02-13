enable_testing()
set(source_files test_csumo_precice_lib.cpp)

add_executable(test_csumo_precice_lib ${source_files})

target_link_libraries(test_csumo_precice_lib PRIVATE compiler_warnings_settings csumo_precice_lib gtest)
if(WIN32)
    # Copy runtime DLLs to test directory for test discovery
    set(Boost_USE_STATIC_LIBS OFF)
    find_package(LibXml2 REQUIRED PATHS "${CMAKE_CURRENT_SOURCE_DIR}/../libxml2/libxml2-v2.15.1/lib/cmake/libxml2")
    find_package(Boost REQUIRED COMPONENTS log log_setup program_options thread PATHS "${CMAKE_CURRENT_SOURCE_DIR}/../boost/boost_1_90_0/lib/cmake")
    target_link_libraries(test_csumo_precice_lib PRIVATE Boost::log Boost::log_setup Boost::program_options Boost::thread LibXml2::LibXml2)
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
