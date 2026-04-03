set(library_name morphology_io)

create_target(${library_name} ${CMAKE_CURRENT_SOURCE_DIR} src_dir src)

# Set dependencies
set(oss_dependencies    deltares_common
                        deltares_common_c
                        deltares_common_mpi
                        ec_module
                        morphology_data
                        morphology_kernel)

target_link_libraries(${library_name} ${oss_dependencies})

include_directories(${checkout_src_root}
                   "${CMAKE_CURRENT_SOURCE_DIR}/../../include")

set_source_files_properties(
                   src/depfil_stm.F90
                   src/
                   PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

target_compile_definitions(${library_name} PRIVATE "MOR_USE_ECMODULE")

set_target_properties (${library_name} PROPERTIES FOLDER utils_gpl/morphology)

f90twtest(test_depfil_stm
  CFILES ${CMAKE_CURRENT_SOURCE_DIR}/src/test_depfil_stm.cpp
  F90FILES ${CMAKE_CURRENT_SOURCE_DIR}/src/test_depfil_stm.f90
  F2HFILES ${CMAKE_CURRENT_SOURCE_DIR}/src/test_depfil_stm.f90
  OUTDIR "${CMAKE_CURRENT_BINARY_DIR}"
  LIBRARIES morphology_io
  VISUAL_STUDIO_PATH tests/morphology_io/test_depfil_stm
)