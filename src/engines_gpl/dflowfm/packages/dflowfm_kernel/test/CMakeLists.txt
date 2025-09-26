f90twtest(test_dflowfm_kernel_example
    CFILES test_dflowfm_kernel_example.cpp
    F90FILES test_dflowfm_kernel_example.f90
    F2HFILES ${CMAKE_CURRENT_SOURCE_DIR}/test_dflowfm_kernel_example.f90
    OUTDIR "${CMAKE_CURRENT_BINARY_DIR}"
    LIBRARIES dflowfm_kernel
    VISUAL_STUDIO_PATH engines_gpl/dflowfm/test
)

add_custom_command(
    TARGET test_dflowfm_kernel_example PRE_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
        "C:/Program Files (x86)/Intel/oneAPI/compiler/latest/bin/libifcoremdd.dll"
        "$<TARGET_FILE_DIR:test_dflowfm_kernel_example>/libifcoremdd.dll"
)

add_custom_command(
    TARGET test_dflowfm_kernel_example PRE_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
        "C:/Program Files (x86)/Intel/oneAPI/compiler/latest/bin/libmmd.dll"
        "$<TARGET_FILE_DIR:test_dflowfm_kernel_example>/libmmd.dll"
)
f90twtest(test_3d_layer_administration
    CFILES test_3d_layer_administration.cpp
    F90FILES test_3d_layer_administration.f90
    F2HFILES ${CMAKE_CURRENT_SOURCE_DIR}/test_3d_layer_administration.f90
    OUTDIR "${CMAKE_CURRENT_BINARY_DIR}"
    LIBRARIES dflowfm_kernel
    VISUAL_STUDIO_PATH engines_gpl/dflowfm/test
)
