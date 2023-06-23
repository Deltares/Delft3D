message(STATUS "Intel Redist")

# Set the directory of where the source code is located
set(src_path src) 
if (WIN32)
	if ("${OSS_MPI}" STREQUAL "IntelMPI")
        set(mpi_path $ENV{I_MPI_ONEAPI_ROOT})
		string(REPLACE "\\" "/" mpi_path "${mpi_path}")
		install(DIRECTORY ${mpi_path}/env/ DESTINATION scripts FILES_MATCHING PATTERN "*.bat")
		install(DIRECTORY ${mpi_path}/bin/ DESTINATION bin 
		FILES_MATCHING 
		PATTERN "*.exe" 
		PATTERN "*.dll" 
		PATTERN "debug" EXCLUDE
		PATTERN "release" EXCLUDE
		PATTERN "tune" EXCLUDE)
		install(DIRECTORY ${mpi_path}/libfabric/bin/* DESTINATION bin FILES_MATCHING PATTERN "*.dll")
		install(DIRECTORY ${mpi_path}/bin/release/ DESTINATION bin FILES_MATCHING PATTERN "*.dll")
        # call :copyFile "%I_MPI_ONEAPI_ROOT%\env\*.bat"                !destination!
        # call :copyFile "%I_MPI_ONEAPI_ROOT%\bin\*.dll"                !destination!
        # call :copyFile "%I_MPI_ONEAPI_ROOT%\bin\*.exe"                !destination!
        # call :copyFile "%I_MPI_ONEAPI_ROOT%\bin\release\*.dll"        !destination!
        # call :copyFile "%I_MPI_ONEAPI_ROOT%\libfabric\bin\*.dll"      !destination!
	endif()
endif(WIN32)