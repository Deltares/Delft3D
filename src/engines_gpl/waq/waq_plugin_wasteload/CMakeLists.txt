# Set version file variables

set(waq_root_path ${CMAKE_CURRENT_SOURCE_DIR}/..) # Trace back to the WAQ path
set(rc_version_file ${waq_root_path}/resources/version/version_number_waq_plugin_wasteload.rc)
set(version_include_dir ${CMAKE_SOURCE_DIR}/../version_includes)

# Set directory which contains all source files
set(src_path .)

# Gather source files
get_fortran_source_files(${src_path} source_files)

# Define library
set(library_name waq_plugin_wasteload)
add_library(${library_name} SHARED ${source_files}
                                   ${rc_version_file})

# Define additional compilation flags
set_source_files_properties(${fortran_version_file}
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

# Set dependencies
set(oss_dependencies deltares_common
                     deltares_common_c
                     waq_data
                     waq_utils_f)

target_link_libraries(${library_name} ${oss_dependencies})

# Set additional compilation properties
target_compile_options(${library_name} PRIVATE "${waq_default_flags}")

target_include_directories(${library_name} PRIVATE ${version_include_dir})

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
set_target_properties (${library_name} PROPERTIES FOLDER engines_gpl/waq)

# Set post-build step
set(install_dir ${CMAKE_BINARY_DIR})
set(build_dir ${CMAKE_BINARY_DIR})

post_build_target (${library_name}
                   ${install_dir}
                   ${build_dir}
                   ${checkout_src_root}
                   ${library_name})

install(TARGETS ${library_name} DESTINATION lib)
