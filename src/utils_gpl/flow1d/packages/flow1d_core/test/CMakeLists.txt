add_fortran_gtest(test_general_structure
    SOURCES
        test_general_structure.f90
        test_tabulated_cross_section_clamping.f90
    LIBRARIES flow1d_core
    VISUAL_STUDIO_PATH utils_gpl/flow1d_library/test
)
