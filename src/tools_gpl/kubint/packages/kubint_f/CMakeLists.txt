set(library_name kubint_f)

# Set version file variables
set(fortran_version_file "${CMAKE_CURRENT_SOURCE_DIR}/src/kubint_common_version.F90")
set(version_include_dir ${CMAKE_SOURCE_DIR}/../version_includes)
set(src_path src)

# Define include directories
get_module_include_path(${checkout_src_root}/${deltares_common_module} 
                        ${library_name}
                        deltares_common_include_path)
message(STATUS "Configuring '${library_name}' with the following deltares_common include path: '${deltares_common_include_path}'")


# Gather source files
get_fortran_source_files(${src_path} source_files)
file(GLOB preprocessor_files ${src_path}/pldep.F90)
set_source_files_properties(${preprocessor_files}
                            ${fortran_version_file}
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

add_library(${library_name} ${source_files}
                            ${preprocessor_files})

# Set additional compilation flags

# Set dependencies
set(oss_dependencies    deltares_common
                        deltares_common_c
                        nefis) 

target_link_libraries(${library_name} ${oss_dependencies})

target_include_directories(${library_name}  PRIVATE ${version_include_dir})

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files}
                                                    ${preprocessor_files})
set_target_properties (${library_name} PROPERTIES FOLDER tools_gpl/kubint)

