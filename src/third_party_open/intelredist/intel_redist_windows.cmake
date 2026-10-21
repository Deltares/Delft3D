set(mkl_path $ENV{ONEAPI_ROOT}/mkl/latest/bin)
string(REPLACE "\\" "/" mkl_path "${mkl_path}")
install(DIRECTORY ${mkl_path}/ DESTINATION bin
    FILES_MATCHING
    PATTERN "mkl_core.*.dll"
    PATTERN "mkl_def.*.dll"
    PATTERN "mkl_avx*.dll"
    PATTERN "mkl_intel_thread.*.dll"
    PATTERN "mkl_sequential.*.dll"
    PATTERN "intel64" EXCLUDE
)

set(redist_path $ENV{ONEAPI_ROOT}/compiler/latest/bin)
string(REPLACE "\\" "/" redist_path "${redist_path}")
install(FILES
    ${redist_path}/libifcoremd.dll
    ${redist_path}/libmmd.dll    
    ${redist_path}/svml_dispmd.dll
    ${redist_path}/libiomp5md.dll  
    ${redist_path}/libifportMD.dll 
    DESTINATION bin
)
    
#Debug runtime dlls
install(FILES
    ${redist_path}/libifcoremdd.dll
    ${redist_path}/libmmdd.dll
    ${redist_path}/libiomp5md_db.dll
    DESTINATION bin CONFIGURATIONS debug
)

if(NOT TARGET mkl_sequential)
    add_library(mkl_sequential SHARED IMPORTED GLOBAL)

    file(TO_CMAKE_PATH "$ENV{ONEAPI_ROOT}" oneapi_root_cmake)
    file(GLOB mkl_sequential_dll
        LIST_DIRECTORIES false
        CONFIGURE_DEPENDS
        "${mkl_path}/mkl_sequential.*.dll"
    )

    list(LENGTH mkl_sequential_dll mkl_sequential_dll_count)
    if(NOT mkl_sequential_dll_count EQUAL 1)
        message(FATAL_ERROR
            "Expected exactly one mkl_sequential.*.dll in '${mkl_path}', "
            "but found ${mkl_sequential_dll_count}."
        )
    endif()

    set_target_properties(mkl_sequential PROPERTIES
        IMPORTED_LOCATION "${mkl_sequential_dll}"
        IMPORTED_IMPLIB "${oneapi_root_cmake}/mkl/latest/lib/mkl_sequential.lib"
    )
endif()
    
# Intel MPI
if("${OSS_MPI}" STREQUAL "IntelMPI")
    set(mpi_path $ENV{ONEAPI_ROOT}/mpi/latest)
    string(REPLACE "\\" "/" mpi_path "${mpi_path}")
    install(DIRECTORY ${mpi_path}/env/ DESTINATION bin FILES_MATCHING PATTERN "*.bat")
    install(DIRECTORY ${mpi_path}/bin/ DESTINATION bin
        FILES_MATCHING
        PATTERN "*.dll"
        PATTERN "debug" EXCLUDE
        PATTERN "release" EXCLUDE
        PATTERN "mpi" EXCLUDE
        PATTERN "tune" EXCLUDE
    )
    install(DIRECTORY ${mpi_path}/bin/ DESTINATION bin
        FILES_MATCHING
        PATTERN "*.exe"
        PATTERN "debug" EXCLUDE
        PATTERN "release" EXCLUDE
        PATTERN "mpi" EXCLUDE
        PATTERN "tune" EXCLUDE
    )
    install(DIRECTORY ${mpi_path}/opt/mpi/libfabric/bin/
        DESTINATION bin
        FILES_MATCHING PATTERN "*.dll" PATTERN "utils"
        EXCLUDE
    )

    if(NOT TARGET impi)
        add_library(impi SHARED IMPORTED GLOBAL)
        set_target_properties(impi PROPERTIES
            IMPORTED_LOCATION "${mpi_path}/bin/impi.dll"
            IMPORTED_IMPLIB "${mpi_path}/lib/impi.lib"
        )
    endif()

    if(NOT TARGET libfabric)
        add_library(libfabric SHARED IMPORTED GLOBAL)
        set_target_properties(libfabric PROPERTIES
            IMPORTED_LOCATION "${mpi_path}/opt/mpi/libfabric/bin/libfabric.dll"
            IMPORTED_IMPLIB "${mpi_path}/lib/libfabric.lib"
        )
    endif()
endif()
