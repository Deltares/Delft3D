add_library(all_compiler_warnings INTERFACE)
set(intel_fortran_windows_all_warning_flags /stand /warn:all)
set(intel_fortran_linux_all_warning_flags -stand "SHELL:-warn all")
set(gcc_all_warning_flags -Wall -pedantic)
target_compile_options(all_compiler_warnings INTERFACE
                       "$<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<Fortran_COMPILER_ID:Intel,IntelLLVM>>:$<IF:$<BOOL:${WIN32}>,${intel_fortran_windows_all_warning_flags},${intel_fortran_linux_all_warning_flags}>>"
                       "$<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<Fortran_COMPILER_ID:GNU>>:${gcc_all_warning_flags}>"
)

add_library(additional_compiler_warnings INTERFACE)
set(intel_fortran_windows_extra_warning_flags
    /warn:declarations
    /warn:ignore_loc
    #/warn:externals
    /warn:interfaces
    /warn:shape
    /warn:truncated_source
    /warn:unused
)
set(intel_fortran_linux_extra_warning_flags
    "SHELL:-warn declarations"
    "SHELL:-warn ignore_loc"
    #"SHELL:-warn externals"
    "SHELL:-warn interfaces"
    "SHELL:-warn shape"
    "SHELL:-warn truncated_source"
    "SHELL:-warn unused"
)
target_compile_options(additional_compiler_warnings INTERFACE
                       "$<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<Fortran_COMPILER_ID:Intel,IntelLLVM>>:$<IF:$<BOOL:${WIN32}>,${intel_fortran_windows_extra_warning_flags},${intel_fortran_linux_extra_warning_flags}>>"
)

add_library(compiler_warnings_as_errors INTERFACE)
set(intel_windows_warning_error_flag /warn:errors)
set(intel_linux_warning_error_flag "SHELL:-warn errors")
set(gcc_warning_error_flag -Werror)
target_compile_options(compiler_warnings_as_errors INTERFACE
                       "$<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<Fortran_COMPILER_ID:GNU>>:${gcc_warning_error_flag}>"
)

# Turning on warnings as errors in the intel compiler (/warn:errors) also gives errors for warnings that were turned off.
# Parse the output of the build log instead
function(add_warnings_as_errors_post_build_windows target_name)
    if (NOT Python_FOUND)
        find_package(Python COMPONENTS Interpreter REQUIRED)
    endif()
    add_custom_command(TARGET ${target_name}
                       POST_BUILD
                       COMMAND Python::Interpreter fortran_warnings_as_errors.py "${CMAKE_CURRENT_BINARY_DIR}/${target_name}.dir/$<CONFIG>/BuildLog.htm" --print-messages --project-name "${target_name}"
                       WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/../../tools/warnings_as_errors
                       VERBATIM
                       )
endfunction()

add_library(mute_unwanted_compiler_warnings INTERFACE)
# Disable warning 5462, global name too long. The compiler limit of 90 characters is too restrictive, see https://community.intel.com/t5/Intel-Fortran-Compiler/Many-quot-Global-name-too-long-quot-warnings/td-p/1505843
# Disable warning 5268, allow text longer than 132 characters
set(intel_windows_disabled_warning_flags /Qdiag-disable:5462 /Qdiag-disable:5268)
set(intel_linux_disabled_warning_flags "SHELL:-diag-disable 5462" "SHELL:-diag-disable 5268")
target_compile_options(mute_unwanted_compiler_warnings INTERFACE
                       "$<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<Fortran_COMPILER_ID:Intel,IntelLLVM>>:$<IF:$<BOOL:${WIN32}>,${intel_windows_disabled_warning_flags},${intel_linux_disabled_warning_flags}>>"
)

add_library(no_compiler_warnings INTERFACE)
set(intel_fortran_windows_no_warning_flags /warn:none)
set(intel_fortran_linux_no_warning_flags "SHELL:-warn none")
set(linux_no_warning_flags -w)
set(windows_no_warning_flags /W0)
target_compile_options(no_compiler_warnings INTERFACE
                       "$<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<Fortran_COMPILER_ID:Intel,IntelLLVM>>:$<IF:$<BOOL:${WIN32}>,${intel_fortran_windows_no_warning_flags},${intel_fortran_linux_no_warning_flags}>>"
                       "$<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<Fortran_COMPILER_ID:GNU>>:${linux_no_warning_flags}>"
                       "$<$<COMPILE_LANGUAGE:C,CXX>:$<IF:$<BOOL:${WIN32}>,${windows_no_warning_flags},${linux_no_warning_flags}>>"
)
