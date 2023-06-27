message(STATUS "Intel Redist")

# Set the directory of where the source code is located
set(src_path src) 
if (WIN32)
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
		PATTERN "mpiexec.exe" 
		PATTERN "hydra_service.exe" 
		PATTERN "debug" EXCLUDE
		PATTERN "release" EXCLUDE
		PATTERN "tune" EXCLUDE)
		install(DIRECTORY ${mpi_path}/libfabric/bin/* DESTINATION lib FILES_MATCHING PATTERN "*.dll")
		install(DIRECTORY ${mpi_path}/bin/release/ DESTINATION lib FILES_MATCHING PATTERN "*.dll")
	endif()
	
	set (mkl_path $ENV{ONEAPI_ROOT}/mkl/latest/redist/intel64)
	string(REPLACE "\\" "/" mkl_path "${mkl_path}")
	install (DIRECTORY ${mkl_path}/ DESTINATION lib
	FILES_MATCHING
	PATTERN "mkl_core.*.dll"
	PATTERN "mkl_def.*.dll"
	PATTERN "mkl_avx*.dll"
	PATTERN "mkl_intel_thread.*.dll"
	PATTERN "1033" EXCLUDE
	)
endif(WIN32)