# Function to copy all required runtime dependencies for dflowfm_kernel tests
function(copy_dflowfm_test_dependencies TARGET_NAME)
    if(WIN32)
        # Copy PETSc DLL explicitly (not captured by TARGET_RUNTIME_DLLS)
        add_custom_command(TARGET ${TARGET_NAME} PRE_BUILD
            COMMAND ${CMAKE_COMMAND} -E copy_if_different 
                "${petsc_libdir}/libpetsc.dll" 
                $<TARGET_FILE_DIR:${TARGET_NAME}>
            COMMENT "Copying PETSc runtime dependency to $<TARGET_FILE_DIR:${TARGET_NAME}>"
        )

        # Copy MKL runtime dependency
        add_custom_command(TARGET ${TARGET_NAME} PRE_BUILD
            COMMAND ${CMAKE_COMMAND} -E copy_if_different 
                "$ENV{ONEAPI_ROOT}/mkl/latest/bin/mkl_sequential.2.dll" 
                $<TARGET_FILE_DIR:${TARGET_NAME}>
            COMMENT "Copying MKL runtime dependency"
        )

        # Copy HDF5 DLL
        add_custom_command(TARGET ${TARGET_NAME} PRE_BUILD
            COMMAND ${CMAKE_COMMAND} -E copy_if_different 
                "${checkout_src_root}/${hdf5_module}/bin/hdf5.dll" 
                $<TARGET_FILE_DIR:${TARGET_NAME}>
            COMMENT "Copying HDF5 runtime dependency"
        )

        # Copy HDF5_HL DLL
        add_custom_command(TARGET ${TARGET_NAME} PRE_BUILD
            COMMAND ${CMAKE_COMMAND} -E copy_if_different 
                "${checkout_src_root}/${hdf5_module}/bin/hdf5_hl.dll" 
                $<TARGET_FILE_DIR:${TARGET_NAME}>
            COMMENT "Copying hdf5_hl runtime dependency"
        )

        # Copy zlib DLL
        add_custom_command(TARGET ${TARGET_NAME} PRE_BUILD
            COMMAND ${CMAKE_COMMAND} -E copy_if_different 
                "${checkout_src_root}/${zlib_module}/bin/zlib.dll" 
                $<TARGET_FILE_DIR:${TARGET_NAME}>
            COMMENT "Copying zlib runtime dependency"
        )
        
        # Uncomment if needed:
        # add_custom_command(TARGET ${TARGET_NAME} PRE_BUILD
        #     COMMAND ${CMAKE_COMMAND} -E copy_if_different 
        #         "$ENV{ONEAPI_ROOT}/mpi/latest/bin/impi.dll" 
        #         $<TARGET_FILE_DIR:${TARGET_NAME}>
        #     COMMENT "Copying impi runtime dependency"
        # )
    endif()
endfunction()

add_subdirectory(helpers)

f90twtest(test_dflowfm_kernel_example
    CFILES test_dflowfm_kernel_example.cpp
    F90FILES test_dflowfm_kernel_example.f90
    F2HFILES ${CMAKE_CURRENT_SOURCE_DIR}/test_dflowfm_kernel_example.f90
    OUTDIR "${CMAKE_CURRENT_BINARY_DIR}"
    LIBRARIES dflowfm_kernel
    VISUAL_STUDIO_PATH engines_gpl/dflowfm/test
)

f90twtest(test_3d_layer_administration
    CFILES test_3d_layer_administration.cpp
    F90FILES test_3d_layer_administration.f90
    F2HFILES ${CMAKE_CURRENT_SOURCE_DIR}/test_3d_layer_administration.f90
    OUTDIR "${CMAKE_CURRENT_BINARY_DIR}"
    LIBRARIES dflowfm_kernel
    VISUAL_STUDIO_PATH engines_gpl/dflowfm/test
)

f90twtest(test_pol_to_cellmask
    CFILES test_pol_to_cellmask.cpp
    F90FILES test_pol_to_cellmask.f90
    F2HFILES ${CMAKE_CURRENT_SOURCE_DIR}/test_pol_to_cellmask.f90
    OUTDIR "${CMAKE_CURRENT_BINARY_DIR}"
    LIBRARIES dflowfm_kernel
    VISUAL_STUDIO_PATH engines_gpl/dflowfm/test
)

f90twtest(test_getprof_1d
    CFILES test_getprof_1d.cpp
    F90FILES test_getprof_1d.f90
    F2HFILES ${CMAKE_CURRENT_SOURCE_DIR}/test_getprof_1d.f90
    OUTDIR "${CMAKE_CURRENT_BINARY_DIR}"
    LIBRARIES dflowfm_kernel test_helpers
    VISUAL_STUDIO_PATH engines_gpl/dflowfm/test
)

copy_dflowfm_test_dependencies(test_getprof_1d)

f90twtest(test_longculverts
    CFILES test_longculverts.cpp
    F90FILES test_longculverts.f90
    F2HFILES ${CMAKE_CURRENT_SOURCE_DIR}/test_longculverts.f90
    OUTDIR "${CMAKE_CURRENT_BINARY_DIR}"
    LIBRARIES dflowfm_kernel test_helpers
    VISUAL_STUDIO_PATH engines_gpl/dflowfm/test
)

copy_dflowfm_test_dependencies(test_longculverts)

f90twtest(test_compute_horton_infiltration
    CFILES test_compute_horton_infiltration.cpp
    F90FILES test_compute_horton_infiltration.f90
    F2HFILES ${CMAKE_CURRENT_SOURCE_DIR}/test_compute_horton_infiltration.f90
    OUTDIR "${CMAKE_CURRENT_BINARY_DIR}"
    LIBRARIES dflowfm_kernel
    VISUAL_STUDIO_PATH engines_gpl/dflowfm/test
)

f90twtest(test_bubblescreen
    CFILES test_bubblescreen.cpp
    F90FILES test_bubblescreen.f90
    F2HFILES ${CMAKE_CURRENT_SOURCE_DIR}/test_bubblescreen.f90
    OUTDIR "${CMAKE_CURRENT_BINARY_DIR}"
    LIBRARIES dflowfm_kernel
    VISUAL_STUDIO_PATH engines_gpl/dflowfm/test
)

add_subdirectory(io)
