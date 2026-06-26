# Set NVHPC (nvfortran) compiler specific flags.
#
# Proof-of-concept toolchain for a serial D-Flow FM build as a stepping stone
# towards OpenMP GPU offload. Linux-only (nvfortran is not available on Windows).
#
# Flag-variable names mirror those defined in intel.cmake / gnu.cmake because the
# rest of the source tree consumes them by name (e.g. ${file_preprocessor_flag},
# ${openmp_flag}, ${heap_arrays_*_flag}). Any unset variable would silently expand
# to empty, so we define the full set and map it to nvfortran equivalents.
enable_language (Fortran)
set(src_root_dir ${CMAKE_SOURCE_DIR}/..)

if (WIN32)
    message(FATAL_ERROR "NVHPC/nvfortran is only supported on Linux. CMake will exit.")
endif(WIN32)

if (UNIX)
    message(STATUS "Setting NVHPC (nvfortran) Fortran compiler flags in Unix")

    set(CMAKE_CXX_FLAGS                 "-fPIC")
    set(CMAKE_CXX_FLAGS_DEBUG           "-g -O0")
    set(CMAKE_CXX_FLAGS_RELWITHDEBINFO  "-g -O2")
    set(CMAKE_CXX_FLAGS_RELEASE         "-O2")
    set(CMAKE_C_FLAGS                   "-fPIC")
    set(CMAKE_C_FLAGS_DEBUG             "-g -O0")
    set(CMAKE_C_FLAGS_RELWITHDEBINFO    "-g -O2")
    set(CMAKE_C_FLAGS_RELEASE           "-O2")

    # -Mextend: allow 132-column fixed-form source (matches Intel default in this tree).
    set(CMAKE_Fortran_FLAGS                "-fPIC -Mextend")
    set(CMAKE_Fortran_FLAGS_RELEASE        "-O2")
    set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-g -O2")
    set(CMAKE_Fortran_FLAGS_DEBUG          "-g -O0")

    set(fortran_standard_flag                    "-std")
    set(cpp_compiler_flags                       "-std=c++17")
    set(automatic_local_variable_storage_flag    "-Mrecursive")
    set(real_size_64_flag                        "-r8")

    set(file_preprocessor_flag                   "-cpp")
    set(check_bounds_flag                        "-Mbounds")
    set(check_nobounds_flag                      "")
    set(check_pointers_flag                      "-Mchkptr")
    set(check_nopointers_flag                    "")
    set(check_uninit_flag                        "")
    set(check_stack_flag                         "")
    # CPU OpenMP for now. Flip to "-mp=gpu" (plus -gpu=ccXX) to enable GPU offload.
    set(openmp_flag                              "-mp")
    set(avx2_flag                                "-tp=native")
    set(generate_reentrancy_threaded_flag        "-Mrecursive")
    set(floating_point_exception_flag            "-Ktrap=fp")
    set(flush_to_zero_flag                       "-Mflushz")
    set(traceback_flag                           "-traceback")
    # nvfortran allocates large automatic arrays on the heap by default; no -heap-arrays equivalent needed.
    set(heap_arrays_one_flag                     "")
    set(heap_arrays_20_flag                      "")
    set(heap_arrays_100_flag                     "")

    set(CMAKE_POSITION_INDEPENDENT_CODE ON)

    # Set debug flags:
    string(APPEND CMAKE_Fortran_FLAGS_DEBUG " ${check_bounds_flag} ${traceback_flag} ${floating_point_exception_flag}")
endif(UNIX)

set(qauto_threaded_flags "SHELL:${automatic_local_variable_storage_flag}" "SHELL:${generate_reentrancy_threaded_flag}")
set(waq_default_flags ${file_preprocessor_flag} ${traceback_flag})

option(ENABLE_CODE_COVERAGE "Enable the code and profiling coverage" OFF)
if(ENABLE_CODE_COVERAGE)
    message(WARNING "Code coverage is not implemented for the NVHPC compiler; ignoring.")
endif(ENABLE_CODE_COVERAGE)
