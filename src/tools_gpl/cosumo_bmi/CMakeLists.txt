# Set directory which contains all source files
set(src_path src)

# Gather source files
get_fortran_source_files(${src_path} source_files)

# Define library
set(library_name cosumo_bmi)
add_library(${library_name} SHARED  ${source_files})

# Set additional compilation properties
target_compile_options(${library_name} PRIVATE "${extend_source132_flag}")

# Set dependencies on windows
if (WIN32)
    set(library_dependencies    ec_module
                                deltares_common
                                deltares_common_c
                                gridgeom
                                kdtree_wrapper
                                kdtree2
                                io_netcdf
                                io_netcdf_data
                                netcdf4
                                netcdff
                                )

    oss_include_libraries(${library_name} library_dependencies)
    target_link_libraries(${library_name} ${library_dependencies})

    # Set linker properties
    message(STATUS "Setting linker properties in windows")

endif(WIN32)

# Set dependencies on linux
if(UNIX)
    set(library_dependencies    ec_module
                                deltares_common
                                deltares_common_c
                                gridgeom
                                kdtree_wrapper
                                kdtree2
                                io_netcdf
                                io_netcdf_data
                                )

    oss_include_libraries(${library_name} library_dependencies)
    target_link_libraries(${library_name} ${library_dependencies})


    #target_link_options(${library_name} PRIVATE ${openmp_flag})
    set_property(TARGET ${library_name} PROPERTY LINKER_LANGUAGE Fortran)

endif(UNIX)

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${library_files})
set_target_properties (${library_name} PROPERTIES FOLDER tools_gpl/cosumo_bmi)

install(TARGETS ${library_name} RUNTIME DESTINATION lib)
