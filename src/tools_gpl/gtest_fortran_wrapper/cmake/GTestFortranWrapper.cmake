function(add_fortran_gtest name)
    cmake_parse_arguments(PARSE_ARGV 1 test ""
        "VISUAL_STUDIO_PATH;TEST_TEARDOWN"
        "SOURCES;CXX_SOURCES;LIBRARIES;INCLUDES;DEFINITIONS")
    if(test_UNPARSED_ARGUMENTS OR test_KEYWORDS_MISSING_VALUES OR NOT test_SOURCES)
        message(FATAL_ERROR "Invalid add_fortran_gtest arguments for ${name}")
    endif()

    set(sources)
    foreach(source IN LISTS test_SOURCES)
        cmake_path(ABSOLUTE_PATH source NORMALIZE OUTPUT_VARIABLE absolute_source)
        list(APPEND sources "${absolute_source}")
    endforeach()
    set(registry "${CMAKE_CURRENT_BINARY_DIR}/${name}_registry.cpp")
    set(teardown)
    if(test_TEST_TEARDOWN)
        list(APPEND teardown --teardown "${test_TEST_TEARDOWN}")
    endif()
    set(generator "${CMAKE_CURRENT_FUNCTION_LIST_DIR}/../tools/generate_test_registry.py")
    add_custom_command(
        OUTPUT "${registry}"
        COMMAND "${Python_EXECUTABLE}" "${generator}" --output "${registry}" ${teardown} ${sources}
        DEPENDS "${generator}" ${sources}
        COMMENT "Registering Fortran tests for ${name}"
        VERBATIM
    )

    add_library(${name}_f90 STATIC ${sources})
    set_target_properties(${name}_f90 PROPERTIES
        POSITION_INDEPENDENT_CODE ON
        FOLDER "${test_VISUAL_STUDIO_PATH}"
    )
    target_include_directories(${name}_f90 PRIVATE ${test_INCLUDES})
    target_compile_definitions(${name}_f90 PRIVATE ${test_DEFINITIONS})
    target_link_libraries(${name}_f90 PRIVATE ${test_LIBRARIES} gtest_fortran_wrapper)
    if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
        target_compile_options(${name}_f90 PRIVATE -ffree-line-length-none)
    endif()

    add_executable(${name} "${registry}" ${test_CXX_SOURCES})
    target_include_directories(${name} PRIVATE ${test_INCLUDES})
    target_compile_definitions(${name} PRIVATE ${test_DEFINITIONS})
    target_link_libraries(${name} PRIVATE ${name}_f90 gtest_fortran_wrapper)
    set_target_properties(${name} PROPERTIES FOLDER "${test_VISUAL_STUDIO_PATH}/cpp")
    if(WIN32)
        set_target_properties(${name} PROPERTIES
            RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/${name}"
            ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/${name}"
            LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/${name}"
        )
        set_target_properties(${name}_f90 PROPERTIES
            ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/${name}")
        add_custom_command(TARGET ${name} POST_BUILD
            COMMAND ${CMAKE_COMMAND} -E copy -t $<TARGET_FILE_DIR:${name}> $<TARGET_RUNTIME_DLLS:${name}>
            COMMAND_EXPAND_LISTS
        )
    endif()
    gtest_discover_tests(${name} WORKING_DIRECTORY $<TARGET_FILE_DIR:${name}>)
endfunction()
