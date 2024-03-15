set(executable_name agrhyd)

# Set version file variables
set(waq_root_path ${CMAKE_SOURCE_DIR}/../engines_gpl/waq) # Trace back to the WAQ path
set(rc_version_file ${waq_root_path}/resources/version/version_number_agrhyd.rc)
set(version_include_dir ${CMAKE_SOURCE_DIR}/../version_includes)

create_target(
        ${executable_name}
        ${CMAKE_CURRENT_SOURCE_DIR}
        src_dir .
        resource_files ${rc_version_file}
        target_type "executable"
)

# Add dependencies
target_include_directories(${executable_name} PRIVATE ${waq_data_path} ${version_include_dir})
set(oss_dependencies waq_definition
                     deltares_common
                     deltares_common_c
                     io_netcdf
                     io_netcdf_data
                     io_hyd
                     waq_hyd_data
                     waq_utils_f)

oss_include_libraries(${executable_name} oss_dependencies)
target_link_libraries(${executable_name} ${oss_dependencies})

# Set additional compilation properties
target_compile_options(${executable_name} PRIVATE ${waq_default_flags})

if (WIN32)
   target_compile_definitions(${executable_name} PRIVATE WIN64)
   configure_visual_studio_user_file(${executable_name})
endif(WIN32)

# Define how the files should be structured within Visual Studio
set_target_properties (${executable_name} PROPERTIES FOLDER tools_gpl/agrhyd)

install(TARGETS ${executable_name} RUNTIME  DESTINATION bin)
install(PROGRAMS ${CMAKE_CURRENT_LIST_DIR}/resources/default/agrhyd.ini  DESTINATION share/delft3d)
if (UNIX)
    install(PROGRAMS ${CMAKE_CURRENT_LIST_DIR}/resources/scripts/run_agrhyd.sh  DESTINATION bin)
endif(UNIX)
if (WIN32) 
    install(PROGRAMS ${CMAKE_CURRENT_LIST_DIR}/resources/scripts/run_agrhyd.bat  DESTINATION bin)
endif(WIN32)
