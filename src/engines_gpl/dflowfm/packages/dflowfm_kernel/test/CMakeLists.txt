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

f90twtest(test_compute_horton_infiltration
    CFILES test_compute_horton_infiltration.cpp
    F90FILES test_compute_horton_infiltration.f90
    F2HFILES ${CMAKE_CURRENT_SOURCE_DIR}/test_compute_horton_infiltration.f90
    OUTDIR "${CMAKE_CURRENT_BINARY_DIR}"
    LIBRARIES dflowfm_kernel
    VISUAL_STUDIO_PATH engines_gpl/dflowfm/test
)

f90twtest(test_coordinate_transform
    CFILES test_coordinate_transform.cpp
    F90FILES test_coordinate_transform.f90
    F2HFILES ${CMAKE_CURRENT_SOURCE_DIR}/test_coordinate_transform.f90
    OUTDIR "${CMAKE_CURRENT_BINARY_DIR}"
    LIBRARIES dflowfm_kernel
    VISUAL_STUDIO_PATH engines_gpl/dflowfm/test
)
    add_custom_command(TARGET test_coordinate_transform PRE_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different 
            "$ENV{ONEAPI_ROOT}/mkl/latest/bin/mkl_sequential.2.dll" 
            $<TARGET_FILE_DIR:test_coordinate_transform>
        COMMENT "Copying mkl runtime dependency"
    )

add_subdirectory(io)
