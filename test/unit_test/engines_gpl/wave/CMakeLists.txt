set(dependencies wave_data wave_io)

f90twtest(tests_wave_regridding
    CFILES tests_wave_regridding.cpp
    F90FILES tests_wave_regridding.f90
    F2HFILES ${CMAKE_CURRENT_SOURCE_DIR}/tests_wave_regridding.f90
    OUTDIR "${CMAKE_CURRENT_BINARY_DIR}"
    LIBRARIES ${dependencies}
    VISUAL_STUDIO_PATH tests/unit_test/engines_gpl/wave
)

if(WIN32)
    target_link_directories(tests_wave_regridding PRIVATE "${mpi_library_path}")
    target_link_libraries(tests_wave_regridding PRIVATE "${mpi_fortran_library}")
endif()