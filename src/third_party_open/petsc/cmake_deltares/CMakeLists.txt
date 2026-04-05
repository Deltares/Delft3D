if(intel_version LESS 24)
    message(FATAL_ERROR "PETSc was compiled with Intel OneAPI 2024, not with older versions. Please update your compiler.")
endif()

if(NOT TARGET mkl_sequential)
    add_library(mkl_sequential SHARED IMPORTED GLOBAL)
    set_target_properties(mkl_sequential PROPERTIES
        IMPORTED_LOCATION "$ENV{ONEAPI_ROOT}/mkl/latest/bin/mkl_sequential.2.dll"
        IMPORTED_IMPLIB "$ENV{ONEAPI_ROOT}/mkl/latest/lib/mkl_sequential.lib"
    )
endif()

if(NOT TARGET petsc)
    add_library(petsc SHARED IMPORTED GLOBAL)
    set_target_properties(petsc PROPERTIES
        IMPORTED_LOCATION "${petsc_libdir}/libpetsc.dll"
        IMPORTED_IMPLIB "${petsc_libdir}/libpetsc.lib"
        INTERFACE_INCLUDE_DIRECTORIES "${petsc_path}"
        INTERFACE_LINK_LIBRARIES "mkl_sequential"
    )
endif()

install(DIRECTORY ${petsc_libdir} DESTINATION lib
	FILES_MATCHING
	PATTERN "*.exe"
	PATTERN "*.dll"
)
