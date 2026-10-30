add_fortran_gtest(test_Readstructure
    SOURCES test_Readstructure.f90
    LIBRARIES flow1d_io
    VISUAL_STUDIO_PATH utils_gpl/flow1d/packages/flow1d_io/test
)
