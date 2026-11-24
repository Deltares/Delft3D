execute_process(
    COMMAND "${test_executable}" restart missing_changed
    RESULT_VARIABLE exit_code
    OUTPUT_VARIABLE output
    ERROR_VARIABLE output
)
if(exit_code STREQUAL "0" OR NOT output MATCHES "The restart contains a changed 1D bed but no 1D cross sections")
    message(FATAL_ERROR "Expected the incomplete 1D restart to be rejected.\n${output}")
endif()
