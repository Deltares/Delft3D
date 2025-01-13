set(library_name flow2d3d_kernel)

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
include_directories(${deltares_common_include_path}
                    ${esmfsm_module_include_path}
                    ${flow2d3d_data_include_path})

# Set directory which contains all source files
set(src_path src)

# Gather source files
get_fortran_source_files(${src_path}/compute compute_files)
get_fortran_source_files(${src_path}/compute_nearfar compute_nearfar_files)
get_fortran_source_files(${src_path}/compute_roller compute_roller_files)
get_fortran_source_files(${src_path}/compute_sediment compute_sediment_files)
get_fortran_source_files(${src_path}/dd dd_files)
get_fortran_source_files(${src_path}/general general_files)
get_fortran_source_files(${src_path}/inichk inichk_files)
get_fortran_source_files(${src_path}/main main_files)
get_fortran_source_files(${src_path}/non_hydro non_hydro_files)
get_fortran_source_files(${src_path}/taylor taylor_files)
get_fortran_source_files(${src_path}/timedep timedep_files)

file(GLOB preprocessor_files ${src_path}/general/pldep.F90)

# Define library
add_library(${library_name} ${compute_files}
                            ${compute_nearfar_files}
                            ${compute_roller_files}
                            ${compute_sediment_files}
                            ${dd_files}
                            ${general_files}
                            ${inichk_files}
                            ${main_files}
                            ${non_hydro_files}
                            ${taylor_files}
                            ${timedep_files}
                            ${preprocessor_files})

# Set additional compilation properties
set_source_files_properties(${preprocessor_files}
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

# Set dependencies
set(oss_dependencies    deltares_common
                        deltares_common_mpi
                        delftio
                        ec_module
                        flow2d3d_data
                        flow2d3d_io_dol_f
                        flow2d3d_kernel_dd_f
                        morphology_data
                        morphology_kernel) 

target_link_libraries(${library_name} ${oss_dependencies})

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${compute_files}
                                                    ${compute_nearfar_files}
                                                    ${compute_roller_files}
                                                    ${compute_sediment_files}
                                                    ${dd_files}
                                                    ${general_files}
                                                    ${inichk_files}
                                                    ${main_files}
                                                    ${non_hydro_files}
                                                    ${taylor_files}
                                                    ${timedep_files})
set_target_properties (${library_name} PROPERTIES FOLDER engines_gpl/flow2d3d)

# Change project flags at the end of this file
target_compile_options(${library_name} PRIVATE "${qauto_threaded_flags}")
target_compile_options(${library_name} PRIVATE "SHELL:$<$<CONFIG:Debug>:${check_nobounds_flag}>")
