set(executable_name waqpb_export)

# Define include directory
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../include)

# Set version file variables
set(waq_root_path ${CMAKE_SOURCE_DIR}/../engines_gpl/waq) # Trace back to the WAQ path
set(rc_version_file ${waq_root_path}/resources/version/version_number_waqpb_export.rc)
set(version_include_dir ${CMAKE_SOURCE_DIR}/../version_includes)

create_target(${executable_name} ${CMAKE_CURRENT_SOURCE_DIR} src_dir . resource_files ${rc_version_file} target_type "executable")

# Set dependencies
target_include_directories(${executable_name} PRIVATE ${version_include_dir})
set(oss_dependencies    deltares_common
                        deltares_common_c
                        nefis
                        waq_utils_f
                        waqpb_lib)
target_link_libraries(${executable_name} ${oss_dependencies})

# Set additional compilation properties
target_compile_options(${executable_name} PRIVATE ${waq_default_flags})
set_target_properties (${executable_name} PROPERTIES FOLDER tools_gpl/waqpb)

if (WIN32)
    configure_visual_studio_user_file(${executable_name})
endif(WIN32)

install(TARGETS ${executable_name} RUNTIME  DESTINATION bin)
install(PROGRAMS ${CMAKE_CURRENT_LIST_DIR}/../resources/scripts/run_waqpb_export.${platform_extension}  DESTINATION bin)
if (WIN32)
    install(PROGRAMS ${CMAKE_CURRENT_LIST_DIR}/../../../../engines_gpl/waq/resources/scripts/export_procdef_csvfiles.bat  DESTINATION bin)
endif(WIN32)