f90twtest(test_Readstructure
    CFILES test_Readstructure.cpp
    F90FILES test_Readstructure.f90
    F2HFILES ${CMAKE_CURRENT_SOURCE_DIR}/test_Readstructure.f90
    OUTDIR "${CMAKE_CURRENT_BINARY_DIR}"
    LIBRARIES flow1d_io
    VISUAL_STUDIO_PATH utils_gpl/flow1d/packages/flow1d_io/test
)
