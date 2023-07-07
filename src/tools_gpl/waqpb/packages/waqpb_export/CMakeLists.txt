# Define include directory
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../include)

# Note that the CMAKE_CURRENT_SOURCE_DIR has a path to THIS CMakeLists location
set(waqpb_root_path ${CMAKE_CURRENT_SOURCE_DIR}/../) # Trace back to the AGRHYD path
set(waq_root_path ${waqpb_root_path}/../../../engines_gpl/waq) # Trace back to the WAQ path

# Set version file variables
set(waq_version_path ${waq_root_path}/version)

set(fortran_version_file_delwaq ${waq_root_path}/packages/waq_utils_f/src/waq_utils/delwaq_version.F90)
set(rc_version_file ${waq_version_path}/version_number_waqpb_export.rc)
set(version_include_dir ${CMAKE_SOURCE_DIR}/../version_includes)


# Set source directory
set(src_path src)

# Gather source files 
get_fortran_source_files(${src_path} source_files)

# Define executable
set(executable_name waqpb_export)
add_executable(${executable_name} ${source_files}
                                  ${rc_version_file}
                                  ${fortran_version_file_delwaq})

# Set dependencies
target_include_directories(${executable_name} PRIVATE ${version_include_dir})
set(oss_dependencies    deltares_common
                        deltares_common_c
                        nefis
                        waq_utils_f
                        waqpb_lib) 
oss_include_libraries(${executable_name} oss_dependencies)
target_link_libraries(${executable_name} ${oss_dependencies})


# Set additional compilation properties
target_compile_options(${executable_name} PRIVATE "${waq_default_flags}")

if(UNIX)
    set_property(TARGET ${executable_name} PROPERTY LINKER_LANGUAGE Fortran)
endif(UNIX)

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
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
    install(PROGRAMS ${CMAKE_SOURCE_DIR}/../tools_gpl/waqpb/scripts/run_waqpb_export.sh  DESTINATION bin)
endif(UNIX)

