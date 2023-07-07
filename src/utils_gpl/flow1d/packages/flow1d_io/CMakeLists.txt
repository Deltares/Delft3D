set(library_name flow1d_io)
set(src_path src) 

# Gather source files
get_fortran_source_files(${src_path} source_files)

# Define the library
add_library(${library_name} ${source_files})

# Specify dependencies
if (UNIX)
    set(oss_dependencies    deltares_common
                            ec_module
                            flow1d_core
                            gridgeom
                            io_netcdf)
endif(UNIX)
if (WIN32)
    set(oss_dependencies    deltares_common
                            gridgeom
                            ec_module
                            flow1d_core
                            gridgeom
                            io_netcdf
                            netcdf4
                            netcdff)
endif(WIN32)
oss_include_libraries(${library_name} oss_dependencies)

if(UNIX)
    target_link_libraries(${library_name} ${oss_dependencies})
endif(UNIX)

if (WIN32)
   # Set additional compilation properties
   target_link_libraries(${library_name} ${oss_dependencies})
endif(WIN32)

# Set additional compilation properties, specific for Debug mode
target_compile_options(${library_name} PRIVATE "$<$<CONFIG:Debug>:${check_pointers_flag}>")
target_compile_options(${library_name} PRIVATE "$<$<CONFIG:Debug>:${floating_point_exception_flag}>")

# Create the folder structure in vfproj
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
set_target_properties (${library_name} PROPERTIES FOLDER utils_gpl/flow1d_library)

