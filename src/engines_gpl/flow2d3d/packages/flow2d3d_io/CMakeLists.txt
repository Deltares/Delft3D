set(library_name flow2d3d_io)

# Define include directories
get_module_include_path(${checkout_src_root}/${deltares_common_module}
                        ${library_name}
                        deltares_common_include_path)
get_module_include_path(${checkout_src_root}/${esmfsm_module}
                        ${library_name}
                        esmfsm_module_include_path)
get_module_include_path(${checkout_src_root}/${flow2d3d_data_module}
                        ${library_name}
                        flow2d3d_data_include_path)

message(STATUS "Configuring '${library_name}' with the following deltares_common include path: '${deltares_common_include_path}'")
message(STATUS "Configuring '${library_name}' with the following esmfsm include path: '${esmfsm_module_include_path}'")
message(STATUS "Configuring '${library_name}' with the following data include path: '${flow2d3d_data_include_path}'")

# Note that the CMAKE_CURRENT_SOURCE_DIR has a path to THIS CMakeLists location
set(include_path ${CMAKE_CURRENT_SOURCE_DIR}/include)
include_directories(${include_path}
                    ${deltares_common_include_path}
                    ${esmfsm_module_include_path}
                    ${flow2d3d_data_include_path})

# Set directory which contains all source files
set(src_path src)

# Gather source files
get_fortran_source_files(${src_path}/input input_files)
get_fortran_source_files(${src_path}/output output_files)
get_fortran_source_files(${src_path}/preprocessor preprocessor_files)
set_source_files_properties(
                    ${src_path}/output/wrwaqcco.F90
                    ${src_path}/output/wrwaqfil.F90
                    ${src_path}/output/wrwaqpnt.F90
                    ${src_path}/output/wrwaqsrf.F90
                    ${fortran_version_file}
                    PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

# Define library
add_library(${library_name} ${input_files}
                            ${output_files}
                            ${preprocessor_files})

# Set dependencies
set(oss_dependencies_general    deltares_common
                                deltares_common_c
                                deltares_common_mpi
                                delftio
                                delftio_shm
                                ec_module
                                flow2d3d_data
                                io_hyd
                                io_netcdf
                                io_netcdf_data
                                morphology_data
                                morphology_io
                                morphology_kernel
                                morphology_plugins_c)
# Add dependencies
if (UNIX)
    set(oss_dependencies ${oss_dependencies_general}
                         netcdff)

    target_link_libraries(${library_name}
         ${oss_dependencies}
    )
endif(UNIX)
if (WIN32)
    set(oss_dependencies ${oss_dependencies_general}
                        netcdff)

    add_definitions(-DHAVE_FC_FORM_BINARY)
endif(WIN32)

target_link_libraries(${library_name} ${oss_dependencies})

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${input_files}
                                                    ${output_files}
                                                    ${preprocessor_files})
set_target_properties (${library_name} PROPERTIES FOLDER engines_gpl/flow2d3d)

# Change project flags at the end of this file
target_compile_options(${library_name} PRIVATE ${qauto_threaded_flags})
target_compile_options(${library_name} PRIVATE "SHELL:$<$<CONFIG:Debug>:${check_nobounds_flag}>")
