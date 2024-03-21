message(STATUS "Installing Intel Redistributable Libraries")

# Set the directory of where the source code is located
set(src_path src) 


# MDK 18-03-2024
# Between Intel oneAPI 2023 and 2024 the file structure changed.
# We cannot check from CMake which Intel oneAPI version we are using.
# Consequently, we perform this step for both versions always
#   - when having only Intel 2023, the install step for 2024 installs nothing.
#   - when having only Intel 2024, the install step for 2023 installs nothing.
#   - when having both, the newest *.dll's overwrite the older ones. Since they
#     are backwards compatible, that works.

#####-----------------------------------------------------------
# Intel 2021/2023
#####-----------------------------------------------------------
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



#####-----------------------------------------------------------
# Intel 2024
#####-----------------------------------------------------------
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
    PATTERN "mpi" EXCLUDE)
    install(DIRECTORY ${mpi_path}/bin/ DESTINATION bin 
    FILES_MATCHING 
    PATTERN "*.exe" 
    PATTERN "debug" EXCLUDE
    PATTERN "release" EXCLUDE
    PATTERN "mpi" EXCLUDE)
    install(DIRECTORY ${mpi_path}/libfabric/bin/ DESTINATION lib FILES_MATCHING PATTERN "*.dll" PATTERN "utils" EXCLUDE)
    install(DIRECTORY ${mpi_path}/bin/release/ DESTINATION lib FILES_MATCHING PATTERN "*.dll")
endif()