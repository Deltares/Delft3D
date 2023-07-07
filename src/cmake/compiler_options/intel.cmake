# Set Intel compiler specific flags:
enable_language (Fortran)

if (WIN32)
    message(STATUS "Setting global Intel Fortran compiler flags in Windows")

    # Set global Fortran compiler flags that apply for each Fortran project
    set(nologo_flag /nologo)
    set(compiler_flags "/W1 ${nologo_flag} /libs:dll /threads")

    # Set optional flags:
    message(STATUS "Setting optional Intel Fortran compiler flags in Windows")
    set(file_preprocessor_flag                /fpp)
    set(automatic_local_variable_storage_flag /Qauto)
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
    
    if (CMAKE_GENERATOR MATCHES "Visual Studio") # for visual studio
        # To prevent Visual Studio compilation failures when trying to write the manifest file
        # to a blocked .exe
        set(CMAKE_EXE_LINKER_FLAGS    "${CMAKE_EXE_LINKER_FLAGS} /MANIFEST:NO")
        set(CMAKE_MODULE_LINKER_FLAGS "${CMAKE_MODULE_LINKER_FLAGS} /MANIFEST:NO")
        set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} /MANIFEST:NO")
    endif()

endif(WIN32)

if (UNIX)
    # Set optional flags:
    message(STATUS "Setting optional Fortran compiler flags in Unix")
    # On Linux preprocessing is on by default, but the flag is inserted for
    # at least one C file as well (netCDF). Use a neutral flag to avoid problems
    set(CMAKE_CXX_FLAGS_RELEASE                  "-O2 -fPIC")
    set(CMAKE_C_FLAGS_RELEASE                    "-O2 -fPIC")
    set(CMAKE_CXX_FLAGS_DEBUG                    "-g -O0 -fPIC")
    set(CMAKE_C_FLAGS_DEBUG                      "-g -O0 -fPIC")
    set(CMAKE_Fortran_FLAGS_RELEASE              "-O2 -fPIC")
    set(CMAKE_Fortran_FLAGS_DEBUG                "-g -O0 -fPIC")

    set(cpp_compiler_flags                       "-std=c++11")
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
endif(UNIX)

set(qauto_threaded_flags ${automatic_local_variable_storage_flag} ${generate_reentrancy_threaded_flag})
set(waq_default_flags ${file_preprocessor_flag} ${extend_source132_flag} ${traceback_flag})
