set(library_name morphology_io)

create_target(${library_name} ${CMAKE_CURRENT_SOURCE_DIR} src_dir src)

# Set dependencies
set(oss_dependencies    deltares_common
                        deltares_common_c
                        deltares_common_mpi
                        ec_module
                        morphology_data
                        morphology_kernel)
oss_include_libraries(${library_name} oss_dependencies)

if(UNIX)
    target_link_libraries(${library_name} ${oss_dependencies})
endif(UNIX)

include_directories(${checkout_src_root}
                   "${CMAKE_CURRENT_SOURCE_DIR}/../../include")

set_source_files_properties(
                   src/depfil_stm.F90
                   PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

target_compile_definitions(${library_name} PRIVATE "MOR_USE_ECMODULE")

# Set additional compilation properties, specific for Debug mode
target_compile_options(${library_name} PRIVATE "$<$<CONFIG:Debug>:${check_pointers_flag}>")
target_compile_options(${library_name} PRIVATE "$<$<CONFIG:Debug>:${floating_point_exception_flag}>")

set_target_properties (${library_name} PROPERTIES FOLDER utils_gpl/morphology)
