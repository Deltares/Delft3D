message(STATUS "Installing Intel Redistributable Libraries")

# Set the directory of where the source code is located
set(src_path src)

if (intel_version LESS_EQUAL 23)
    set (mkl_path $ENV{ONEAPI_ROOT}/mkl/latest/redist/intel64)
    string(REPLACE "\\" "/" mkl_path "${mkl_path}")
    install (DIRECTORY ${mkl_path}/ DESTINATION lib
    FILES_MATCHING
    PATTERN "mkl_core.*.dll"
    PATTERN "mkl_def.*.dll"
    PATTERN "mkl_avx*.dll"
    PATTERN "mkl_intel_thread.*.dll"
    PATTERN "mkl_sequential.*.dll"
    PATTERN "1033" EXCLUDE
    )

    set (redist_path $ENV{ONEAPI_ROOT}/compiler/latest/windows/redist/intel64_win/compiler)
    string(REPLACE "\\" "/" redist_path "${redist_path}")
    install (DIRECTORY ${redist_path}/ DESTINATION lib
    FILES_MATCHING
    PATTERN "*.dll"
    PATTERN "1033" EXCLUDE
    )

    # Intel MPI
    if ("${OSS_MPI}" STREQUAL "IntelMPI")
        set(mpi_path $ENV{I_MPI_ONEAPI_ROOT})
        string(REPLACE "\\" "/" mpi_path "${mpi_path}")
        install(DIRECTORY ${mpi_path}/env/ DESTINATION bin FILES_MATCHING PATTERN "*.bat")
        install(DIRECTORY ${mpi_path}/bin/ DESTINATION lib
        FILES_MATCHING
        PATTERN "*.dll"
        PATTERN "debug" EXCLUDE
        PATTERN "release" EXCLUDE
        PATTERN "tune" EXCLUDE)
        install(DIRECTORY ${mpi_path}/bin/ DESTINATION bin
        FILES_MATCHING
        PATTERN "*.exe"
        PATTERN "debug" EXCLUDE
        PATTERN "release" EXCLUDE
        PATTERN "tune" EXCLUDE)
        install(DIRECTORY ${mpi_path}/libfabric/bin/ DESTINATION lib FILES_MATCHING PATTERN "*.dll" PATTERN "utils" EXCLUDE)
        install(DIRECTORY ${mpi_path}/bin/release/ DESTINATION lib FILES_MATCHING PATTERN "*.dll")
    endif()

elseif (intel_version GREATER_EQUAL 24)
    # Intel OneAPI folder structures and environment variables have changed with OneAPI 2024
    set (mkl_path $ENV{ONEAPI_ROOT}/mkl/latest/bin)
    string(REPLACE "\\" "/" mkl_path "${mkl_path}")
    install (DIRECTORY ${mkl_path}/ DESTINATION lib
    FILES_MATCHING
    PATTERN "mkl_core.*.dll"
    PATTERN "mkl_def.*.dll"
    PATTERN "mkl_avx*.dll"
    PATTERN "mkl_intel_thread.*.dll"
    PATTERN "mkl_sequential.*.dll"
    PATTERN "intel64" EXCLUDE
    )

    set (redist_path $ENV{ONEAPI_ROOT}/compiler/latest/bin)
    string(REPLACE "\\" "/" redist_path "${redist_path}")
    install (DIRECTORY ${redist_path}/ DESTINATION lib
    FILES_MATCHING
    PATTERN "libi*.dll"
    PATTERN "libm*.dll"
    PATTERN "svml_dispmd.dll"
    PATTERN "1033" EXCLUDE
    PATTERN "compiler" EXCLUDE
    )

    # Intel MPI
    if ("${OSS_MPI}" STREQUAL "IntelMPI")
        set(mpi_path $ENV{ONEAPI_ROOT}/mpi/latest)
        string(REPLACE "\\" "/" mpi_path "${mpi_path}")
        install(DIRECTORY ${mpi_path}/env/ DESTINATION bin FILES_MATCHING PATTERN "*.bat")
        install(DIRECTORY ${mpi_path}/bin/ DESTINATION lib
        FILES_MATCHING
        PATTERN "*.dll"
        PATTERN "debug" EXCLUDE
        PATTERN "release" EXCLUDE
        PATTERN "mpi" EXCLUDE
        PATTERN "tune" EXCLUDE)
        install(DIRECTORY ${mpi_path}/bin/ DESTINATION bin
        FILES_MATCHING
        PATTERN "*.exe"
        PATTERN "debug" EXCLUDE
        PATTERN "release" EXCLUDE
        PATTERN "mpi" EXCLUDE
        PATTERN "tune" EXCLUDE)
        install(DIRECTORY ${mpi_path}/opt/mpi/libfabric/bin/ DESTINATION lib FILES_MATCHING PATTERN "*.dll" PATTERN "utils" EXCLUDE)
    endif()
else()
    message(FATAL_ERROR "intel version ${intel_version} was not recognized. \nCannot install intel redistributable libraries.")
endif()
