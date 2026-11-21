add_library(dflowfm_io_fortran
    dflowfm_io_c.f90
    dflowfm_io_helpers.f90
    dflowfm_io.f90
)

target_link_libraries(dflowfm_io_fortran
    PUBLIC dflowfm_io_api
)

target_include_directories(dflowfm_io_fortran
    PUBLIC
        ${CMAKE_Fortran_MODULE_DIRECTORY}
)
