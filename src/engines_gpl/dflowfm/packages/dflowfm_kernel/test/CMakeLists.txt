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

f90twtest(test_longculverts
    CFILES test_longculverts.cpp
    F90FILES test_longculverts.f90
    F2HFILES ${CMAKE_CURRENT_SOURCE_DIR}/test_longculverts.f90
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

f90twtest(test_bubblescreen
    CFILES test_bubblescreen.cpp
    F90FILES test_bubblescreen.f90
    F2HFILES ${CMAKE_CURRENT_SOURCE_DIR}/test_bubblescreen.f90
    OUTDIR "${CMAKE_CURRENT_BINARY_DIR}"
    LIBRARIES dflowfm_kernel
    VISUAL_STUDIO_PATH engines_gpl/dflowfm/test
)

f90twtest(test_sources_sinks
    CFILES test_sources_sinks.cpp
    F90FILES test_sources_sinks.f90
    F2HFILES ${CMAKE_CURRENT_SOURCE_DIR}/test_sources_sinks.f90
    OUTDIR "${CMAKE_CURRENT_BINARY_DIR}"
    LIBRARIES dflowfm_kernel test_helpers
    VISUAL_STUDIO_PATH engines_gpl/dflowfm/test
)

f90twtest(test_ec_module_block
    CFILES test_ec_module_block.cpp
    F90FILES test_ec_module_block.f90
    F2HFILES ${CMAKE_CURRENT_SOURCE_DIR}/test_ec_module_block.f90
    OUTDIR "${CMAKE_CURRENT_BINARY_DIR}"
    LIBRARIES dflowfm_kernel test_helpers
    VISUAL_STUDIO_PATH engines_gpl/dflowfm/test
)


f90twtest(test_unstruc_model
    CFILES test_unstruc_model.cpp
    F90FILES test_unstruc_model.f90
    F2HFILES ${CMAKE_CURRENT_SOURCE_DIR}/test_unstruc_model.f90
    OUTDIR "${CMAKE_CURRENT_BINARY_DIR}"
    LIBRARIES dflowfm_kernel
    VISUAL_STUDIO_PATH engines_gpl/dflowfm/test
)

add_subdirectory(io)
add_subdirectory(helpers)
