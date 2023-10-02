# Set directory which contains all source files
set(src_path src)

# Gather source files
get_fortran_source_files(${src_path}/bloom bloom_source)

# Gather source files
get_fortran_source_files(${src_path}/protist protist_source)

# Gather source files
get_fortran_source_files(${src_path}/proc_preprocess proc_preprocess_source)

# Gather source files
get_fortran_source_files(${src_path}/trwp trwp_source)

# Gather source files
get_fortran_source_files(${src_path}/waq_process waq_process_source)


# Define library
set(library_name waq_process)
add_library(${library_name} ${bloom_source}
                            ${protist_source}
                            ${proc_preprocess_source}
                            ${trwp_source}
                            ${waq_process_source})


# Set dependencies
set(oss_dependencies deltares_common
                     deltares_common_c
                     nefis
                     solvesaphe
                     waq_data
                     waq_utils_c
                     waq_utils_f)

oss_include_libraries(${library_name} oss_dependencies)
target_link_libraries(${library_name} ${oss_dependencies})


target_include_directories(${library_name}  PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/../../include")

# Set additional compilation properties
target_compile_options(${library_name} PRIVATE "${qauto_threaded_flags}")
target_compile_options(${library_name} PRIVATE "${waq_default_flags}")

# Define how the files should be structured in Visual studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${bloom_source}
                                                    ${protist_source}
                                                    ${proc_preprocess_source}
                                                    ${trwp_source}
                                                    ${waq_process_source})

set_target_properties (${library_name} PROPERTIES FOLDER engines_gpl/waq)
