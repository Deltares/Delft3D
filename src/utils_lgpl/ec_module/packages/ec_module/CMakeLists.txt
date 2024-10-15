set(src_path src)

file(GLOB meteo_sources ${src_path}/meteo/*.f90)
file(GLOB sources ${src_path}/*.f90 ${src_path}/*.F90)

set(version_include_dir ${CMAKE_SOURCE_DIR}/../version_includes)
set(rc_dir ${CMAKE_CURRENT_SOURCE_DIR}/version)
set(rc_version_file ${rc_dir}/ec_version_number.rc)

# Remove files that should not be compiled
list(REMOVE_ITEM sources "${CMAKE_CURRENT_SOURCE_DIR}/${src_path}/ec_stringbuffer.f90"
                         "${CMAKE_CURRENT_SOURCE_DIR}/${src_path}/ec_module_api.f90"
                         "${CMAKE_CURRENT_SOURCE_DIR}/${src_path}/ec_module_version.F90")

add_compile_options("${automatic_local_variable_storage_flag}")
set(library_name ec_module)
add_library(${library_name} ${meteo_sources} ${sources})

# Add dependencies


    set(oss_dependencies deltares_common
                         gridgeom
                         kdtree2
                         kdtree_wrapper
                         io_netcdf
                         io_netcdf_data
                         netcdf4
                         netcdff)


target_link_libraries(${library_name} ${oss_dependencies})

# Set additional compilation properties, specific for Debug mode
target_compile_options(${library_name} PRIVATE "$<$<CONFIG:Debug>:${check_pointers_flag}>")
target_compile_options(${library_name} PRIVATE "$<$<CONFIG:Debug>:${floating_point_exception_flag}>")

set_source_files_properties(${src_path}/ec_parameters.F90
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

# Create the folder structure in vfproj
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${sources} ${meteo_sources})
set_target_properties (${library_name} PROPERTIES FOLDER utils_lgpl/ec_module)

###############
# EC_MODULE_DLL
###############
string(TOUPPER ${CONFIGURATION_TYPE} configuration_type)
if ( ${configuration_type} STREQUAL "DFLOWFM" OR
     ${configuration_type} STREQUAL "DELFT3D4" OR
     ${configuration_type} STREQUAL "ALL" OR
     ${configuration_type} STREQUAL "TOOLS" )

    set(library_name ec_module_dll)
    set(library_renamed libec_module.so)
    # Add dynamic ec_module library
    add_library(${library_name} SHARED ${src_path}/ec_module_api.f90 ${rc_version_file})
    include_directories(${version_include_dir})
    set(oss_dependencies deltares_common kdtree2 kdtree_wrapper gridgeom io_netcdf ec_module)

	target_link_libraries(${library_name} ${oss_dependencies})

    # Set additional compilation properties, specific for Debug mode
    target_compile_options(${library_name} PRIVATE "$<$<CONFIG:Debug>:${check_pointers_flag}>")
    target_compile_options(${library_name} PRIVATE "$<$<CONFIG:Debug>:${floating_point_exception_flag}>")

    # Create the folder structure in vfproj
    source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${src_path}/ec_module_api.f90)
    set_target_properties (${library_name} PROPERTIES FOLDER utils_lgpl/ec_module)


    install(PROGRAMS $<TARGET_FILE:${library_name}> RENAME ec_module.${library_suffix} DESTINATION lib)
endif()
