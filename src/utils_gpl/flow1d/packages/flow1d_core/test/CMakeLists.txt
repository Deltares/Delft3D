f90twtest(test_general_structure
    CFILES test_general_structure.cpp
    F90FILES test_general_structure.f90
    F2HFILES ${CMAKE_CURRENT_SOURCE_DIR}/test_general_structure.f90
    OUTDIR "${CMAKE_CURRENT_BINARY_DIR}"
    LIBRARIES flow1d_core
    VISUAL_STUDIO_PATH utils_gpl/flow1d_library/test
)
