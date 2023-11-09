# Define include directory
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../include)

# Set version file variables
set(waq_root_path ${CMAKE_SOURCE_DIR}/../engines_gpl/waq) # Trace back to the WAQ path
set(rc_version_file ${waq_root_path}/resources/version/version_number_waqpb_import.rc)
set(version_include_dir ${CMAKE_SOURCE_DIR}/../version_includes)

# Set source directory
set(src_path .)

# Gather source files
get_fortran_source_files(${src_path} source)

# Define executable
set(executable_name waqpb_import)
add_executable(${executable_name} ${source}
                                  ${rc_version_file})

# Set dependencies
target_include_directories(${executable_name} PRIVATE ${version_include_dir})
set(oss_dependencies    deltares_common
                        waq_utils_f
                        waqpb_lib)

target_link_libraries(${executable_name} ${oss_dependencies})

# Set additional compilation properties
target_compile_options(${executable_name} PRIVATE "${waq_default_flags}")

if(UNIX)
    set_property(TARGET ${executable_name} PROPERTY LINKER_LANGUAGE Fortran)
endif(UNIX)

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source})
set_target_properties (${executable_name} PROPERTIES FOLDER tools_gpl/waqpb)

# Set post-build step
set(install_dir ${CMAKE_BINARY_DIR})
set(build_dir ${CMAKE_BINARY_DIR})

post_build_target (${executable_name}
                   ${install_dir}
                   ${build_dir}
                   ${checkout_src_root}
                   ${executable_name})

install(TARGETS ${executable_name} RUNTIME  DESTINATION bin)
if (UNIX)
    install(PROGRAMS ${CMAKE_CURRENT_LIST_DIR}/../resources/scripts/run_waqpb_import.sh  DESTINATION bin)
endif(UNIX)
