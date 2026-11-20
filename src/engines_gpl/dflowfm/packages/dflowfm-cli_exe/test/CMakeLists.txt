add_test(NAME dflowfm_cli_version
    COMMAND ${CMAKE_COMMAND}
        "-DDFLOWFM_EXECUTABLE=$<TARGET_FILE:dflowfm-cli>"
        -P "${CMAKE_CURRENT_SOURCE_DIR}/version/check_version.cmake"
)
set_tests_properties(dflowfm_cli_version PROPERTIES
    WORKING_DIRECTORY "$<TARGET_FILE_DIR:dflowfm-cli>"
)
