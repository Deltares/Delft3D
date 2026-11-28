add_library(
    test_helpers 
    STATIC 
    file_helpers.f90 
    netcdf_helpers.f90 
    network_helpers.f90 
    message_helpers.f90 
    teardown_helpers.f90
)
set_target_properties(test_helpers PROPERTIES FOLDER engines_gpl/dflowfm/test)
target_link_libraries(test_helpers PUBLIC dflowfm_kernel PRIVATE gtest_fortran_wrapper)
