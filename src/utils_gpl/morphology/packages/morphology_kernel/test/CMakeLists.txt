f90twtest(test_morphology
    CFILES ${CMAKE_CURRENT_SOURCE_DIR}/test_morphology.cpp
    F90FILES ${CMAKE_CURRENT_SOURCE_DIR}/test_morphology.f90
    F2HFILES ${CMAKE_CURRENT_SOURCE_DIR}/test_morphology.f90
    OUTDIR "${CMAKE_CURRENT_BINARY_DIR}"
    LIBRARIES morphology_kernel morphology_io
    VISUAL_STUDIO_PATH tests/utils_gpl/morphology/test
)