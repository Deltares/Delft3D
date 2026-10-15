f90twtest(test_morphology
    CFILES ${CMAKE_CURRENT_SOURCE_DIR}/test_morphology.cpp
    F90FILES ${CMAKE_CURRENT_SOURCE_DIR}/test_morphology.f90
    F2HFILES ${CMAKE_CURRENT_SOURCE_DIR}/test_morphology.f90
    OUTDIR "${CMAKE_CURRENT_BINARY_DIR}"
    LIBRARIES  morphology_io
               morphology_kernel
               deltares_common_mpi
    VISUAL_STUDIO_PATH tests/utils_gpl/morphology/test
)

if(WIN32)
    target_link_directories(test_morphology PRIVATE
                            "${mpi_library_path}")
    target_link_libraries(test_morphology PRIVATE
                          "${mpi_fortran_library}")
endif()

install(TARGETS test_morphology RUNTIME  DESTINATION test)