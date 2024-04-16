set(executable_name delpar)

set(waq_root_path ${CMAKE_SOURCE_DIR}/../engines_gpl/waq)
set(waq_version_path ${waq_root_path}/resources/version)
set(rc_version_file ${waq_version_path}/version_number_delpar.rc)
set(version_include_dir ${CMAKE_SOURCE_DIR}/../version_includes)
set(icon_file ${waq_version_path}/part_icon.rc)

create_target(
        ${executable_name} ${CMAKE_CURRENT_SOURCE_DIR}
        src_dir .
        resource_files ${icon_file} ${rc_version_file}
        target_type "executable"
)

# Add dependencies
set(oss_dependencies part_data_f
                     part_kernel_f)

target_link_libraries(${executable_name} ${oss_dependencies})
target_include_directories(${executable_name} PRIVATE ${waq_data_path} ${version_include_dir})

# Set additional compilation properties
target_compile_options(${executable_name} PRIVATE ${waq_default_flags})

if(UNIX)
    target_link_options(${executable_name} PRIVATE ${openmp_flag})
endif(UNIX)

if(WIN32)
    # Set linker properties
    target_link_directories(${executable_name}
        PRIVATE
        "${checkout_src_root}/third_party_open/pthreads/bin/x64")

    target_link_libraries(${executable_name}
        "pthreadVC2.lib")

    # Set linker options
    target_link_options(${executable_name} PRIVATE ${nologo_flag})
endif(WIN32)

# Define how the files should be structured within Visual Studio
target_compile_options(${executable_name} PRIVATE ${openmp_flag})
set_target_properties(${executable_name} PROPERTIES FOLDER engines_gpl/part)

if(WIN32)
    set_target_properties(${executable_name} PROPERTIES LINK_FLAGS "/LARGEADDRESSAWARE /STACK:20000000")
    configure_visual_studio_user_file(${executable_name})
endif(WIN32)

install(TARGETS ${executable_name} RUNTIME DESTINATION bin)
install(PROGRAMS ${CMAKE_CURRENT_LIST_DIR}/../resources/scripts/run_dpart.${platform_extension} DESTINATION bin)