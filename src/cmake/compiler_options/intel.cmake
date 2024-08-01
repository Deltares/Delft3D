# Set Intel compiler specific flags:
enable_language (Fortran)
set(src_root_dir ${CMAKE_SOURCE_DIR}/..)

add_library(strict_compiler_warnings INTERFACE)
# Disable warning 5268, allow text longer than 132 characters
set(windows_extra_warning_flags /warn:errors /warn:all /warn:stderrors /Qdiag-disable:5268)
set(linux_extra_warning_flags "SHELL:-warn errors" "SHELL:-warn all" "SHELL:-warn stderrors" "SHELL:-diag-disable 5268")
target_compile_options(strict_compiler_warnings INTERFACE "$<$<COMPILE_LANGUAGE:Fortran>:$<IF:$<BOOL:${WIN32}>,${windows_extra_warning_flags},${linux_extra_warning_flags}>>")

add_library(no_warnings INTERFACE)
set(windows_no_warning_flags /warn:none)
set(linux_no_warning_flags "SHELL:-warn none")
target_compile_options(no_warnings INTERFACE "$<$<COMPILE_LANGUAGE:Fortran>:$<IF:$<BOOL:${WIN32}>,${windows_no_warning_flags},${linux_no_warning_flags}>>")

if (WIN32)
    # Set global Fortran compiler flags that apply for each Fortran project
    message(STATUS "Setting global Intel Fortran compiler flags in Windows")
    set(CMAKE_Fortran_FLAGS "/W1 /nologo /libs:dll /threads /MP")

    # Set global C/C++ compiler flags that apply for each C/C++ project
    string(APPEND CMAKE_C_FLAGS " /MP")
    string(APPEND CMAKE_CXX_FLAGS " /MP")

    # Set optional flags:
    message(STATUS "Setting optional Intel Fortran compiler flags in Windows")
    set(file_preprocessor_flag                /fpp)
    set(automatic_local_variable_storage_flag /auto)
    set(extend_source132_flag                 /extend-source:132)
    set(heap_arrays_one_flag                  /heap-arrays:1)
    set(heap_arrays_100_flag                  /heap-arrays:100)
    set(real_size_64_flag                     /real-size:64)

    set(linker_debug_flag                     /debug)
    set(check_bounds_flag                     /check:bounds)
    set(check_nobounds_flag                   /check:nobounds)
    set(check_pointers_flag                   /check:pointers)
    set(check_nopointers_flag                 /check:nopointers)
    set(check_uninit_flag                     /check:uninit)
    set(check_stack_flag                      /check:stack)
    set(openmp_flag                           /Qopenmp)
    set(generate_reentrancy_threaded_flag     /reentrancy:threaded)
    set(floating_point_exception_flag         /fpe:0)
    set(traceback_flag                        /traceback)

    set(codecov_flag                          /Qcov-gen)
    set(profiling_flag                        /Qprof-gen:srcpos)
    set(srcrootdir_code_cov                   /Qprof-src-root ${src_root_dir})

    # Store debug info inside objects instead of pdbs when building, pbds are created by linker after.
    # Since we build with /MP, multiple processes might write into the same PDB otherwise, causing corrupt PDB errors.
    # CMAKE_Fortran_FLAGS_DEBUG contains /debug:full by default, which creates a PDB before linking, but specifying /Z7 after overrides this behavior.
    set(debug_information_flag                /Z7)

    # Set debug flags:
    string(APPEND CMAKE_Fortran_FLAGS_DEBUG " ${check_stack_flag} ${check_bounds_flag} ${traceback_flag} ${debug_information_flag}")
    string(APPEND CMAKE_Fortran_FLAGS_RELWITHDEBINFO " ${debug_information_flag}")

    # To prevent Visual Studio compilation failures when trying to write the manifest file
    # to a blocked .exe
    if (CMAKE_GENERATOR MATCHES "Visual Studio") # for visual studio
        string(APPEND CMAKE_EXE_LINKER_FLAGS " /MANIFEST:NO")
        string(APPEND CMAKE_MODULE_LINKER_FLAGS " /MANIFEST:NO")
        string(APPEND CMAKE_SHARED_LINKER_FLAGS " /MANIFEST:NO")
    endif()
endif(WIN32)

if (UNIX)
    # Set optional flags:
    message(STATUS "Setting Fortran compiler flags in Unix")
    # On Linux preprocessing is on by default, but the flag is inserted for
    # at least one C file as well (netCDF). Use a neutral flag to avoid problems
    set(CMAKE_CXX_FLAGS_RELEASE                  "-O2 -fPIC")
    set(CMAKE_C_FLAGS_RELEASE                    "-O2 -fPIC")
    set(CMAKE_CXX_FLAGS_DEBUG                    "-g -O0 -fPIC")
    set(CMAKE_C_FLAGS_DEBUG                      "-g -O0 -fPIC")
    set(CMAKE_Fortran_FLAGS_RELEASE              "-O2 -fPIC")
    set(CMAKE_Fortran_FLAGS_DEBUG                "-g -O0 -fPIC")
    set(fortran_standard_flag                    "-std")

    set(cpp_compiler_flags                       "-std=c++17")
    set(cxx_compiler_flags......................."-lstdc++")
    set(automatic_local_variable_storage_flag    -auto)
    set(extend_source132_flag                    -extend-source 132)
    set(real_size_64_flag                        -r8)

    set(file_preprocessor_flag                   -fpp)
    set(check_bounds_flag                        "-check bounds")
    set(check_nobounds_flag                      "-check nobounds")
    set(check_pointers_flag                      -check pointers)
    set(check_nopointers_flag                    -check nopointers)
    set(check_uninit_flag                        "-check uninit")
    set(check_stack_flag                         "-check stack")
    set(openmp_flag                              "-qopenmp")
    set(generate_reentrancy_threaded_flag        -reentrancy threaded)
    set(floating_point_exception_flag            -fpe0)
    set(traceback_flag                           -traceback)

    set(CMAKE_POSITION_INDEPENDENT_CODE ON)

    # Set debug flags:
    string(APPEND CMAKE_Fortran_FLAGS_DEBUG " ${check_uninit_flag} ${check_stack_flag} ${check_bounds_flag} ${traceback_flag}")
endif(UNIX)

set(qauto_threaded_flags ${automatic_local_variable_storage_flag} ${generate_reentrancy_threaded_flag})
set(waq_default_flags ${file_preprocessor_flag} ${traceback_flag})

# Define the custom flag about code coverage with a default value of OFF
option(ENABLE_CODE_COVERAGE "Enable the code and profiling coverage" OFF)
if(ENABLE_CODE_COVERAGE)
    message("Code coverage and profiling analysis is enabled")
    list(APPEND waq_default_flags ${codecov_flag} ${profiling_flag} ${srcrootdir_code_cov})
endif(ENABLE_CODE_COVERAGE)
