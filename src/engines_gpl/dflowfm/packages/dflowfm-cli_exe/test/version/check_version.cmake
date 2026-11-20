execute_process(
    COMMAND "${DFLOWFM_EXECUTABLE}" --version
    RESULT_VARIABLE result
    OUTPUT_VARIABLE output
    ERROR_VARIABLE errors
    TIMEOUT 30
)
if(NOT result STREQUAL "0")
    message(FATAL_ERROR "dflowfm --version failed (${result}):\n${output}${errors}")
endif()

string(REPLACE "\r\n" "\n" output "${output}")
string(REGEX REPLACE
    "^(Deltares, D-Flow FM Version) [0-9][^\n]*\nSource: [^\n]+\n"
    "\\1\n" normalized "${output}")
string(REGEX REPLACE
    "(Compiled with Intel ifx, version) [0-9]+\\.[0-9]+\\.[0-9]+\n"
    "\\1\n" normalized "${normalized}")

file(READ "${CMAKE_CURRENT_LIST_DIR}/expected_version.txt" expected)
string(REPLACE "\r\n" "\n" expected "${expected}")
if(NOT normalized STREQUAL expected OR NOT errors STREQUAL "")
    message(FATAL_ERROR "Unexpected dflowfm --version output:\n${output}${errors}\nExpected (without versions/source):\n${expected}")
endif()
