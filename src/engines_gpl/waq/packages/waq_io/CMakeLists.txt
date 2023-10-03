# Set path for include files
# Note that the CMAKE_CURRENT_SOURCE_DIR has a path to THIS CMakeLists location
set(waq_io_include_path ${CMAKE_CURRENT_SOURCE_DIR}/../../include)
include_directories(${waq_io_include_path})

# Set directory which contains all source files
set(src_path src)

# Gather source files
get_fortran_source_files(${src_path}/ods ods_files)
get_fortran_source_files(${src_path}/waq_io waq_io_files)

# Define the library
set(library_name waq_io)
add_library(${library_name} ${ods_files}
                            ${waq_io_files})

# Set dependencies
set(oss_dependencies deltares_common
                     deltares_common_c
                     part_data_f
                     part_io_f
                     part_utils_f
                     io_hyd
                     io_netcdf
                     waq_data
                     waq_netcdf
                     waq_validation
                     waq_utils_c
                     waq_utils_f)

target_link_libraries(${library_name} ${oss_dependencies})
oss_include_libraries(${library_name} oss_dependencies)

# Set additional compilation properties
target_compile_options(${library_name} PRIVATE "${waq_default_flags}")

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${ods_files}
                                                    ${waq_io_files})
set_target_properties(${library_name} PROPERTIES FOLDER engines_gpl/waq)
