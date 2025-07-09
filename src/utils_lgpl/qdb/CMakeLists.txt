# Set directories
# Set the directory of where the source code is located.
set(src_path src)

# Set the directory of where the resources are located.
set(res_path res)

# Set version file variables

set(rc_version_file ${res_path}/qdb_version.rc)
set(version_include_dir ${CMAKE_SOURCE_DIR}/../version_includes)
set(fortran_version_files ${src_path}/qdb_version.F90
                          ${src_path}/qdb_static_version_info.F90
                          )

set_source_files_properties(${fortran_version_files}
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")
# Gather the source files
get_fortran_source_files(${src_path} sources)

# Define the executable
set(executable_name qdb)

add_executable(${executable_name} ${sources} ${rc_version_file})

# Set dependencies
set(oss_dependencies    deltares_common
                        deltares_common_c
                        nefis
                        )

# Add dependencies
if(UNIX)
   message(STATUS "Setting target_compile_definitions in Unix")
   set_property(TARGET ${executable_name} PROPERTY LINKER_LANGUAGE Fortran)
endif(UNIX)

if (WIN32)
   target_compile_definitions(${executable_name} PRIVATE WIN64)
endif(WIN32)

target_link_libraries(${executable_name}
                      ${oss_dependencies}
                      )

# Include directories that are required as references
include_directories(${version_include_dir}
                    ${res_path}
                    )

set_source_files_properties(${fortran_version_file}
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

# Create the folder structure in vfproj
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${sources} ${fortran_version_file} ${rc_version_file})

set_target_properties (${executable_name} PROPERTIES FOLDER utils_lgpl/qdb)
install(TARGETS ${executable_name} RUNTIME  DESTINATION bin)
install(PROGRAMS ${CMAKE_CURRENT_SOURCE_DIR}/scripts/run_qdb.${platform_extension}         DESTINATION bin)

# Set post-build step
set(install_dir ${CMAKE_BINARY_DIR})
set(build_dir ${CMAKE_BINARY_DIR})
