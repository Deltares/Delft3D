set(executable_name delwaq1)

set(waq_root_path ${CMAKE_CURRENT_SOURCE_DIR}/../)
set(waq_version_path ${waq_root_path}/resources/version)
set(rc_version_file ${waq_version_path}/version_number_delwaq1.rc)
set(version_include_dir ${CMAKE_SOURCE_DIR}/../version_includes)

# Set icon file
set(icon_file ${waq_version_path}/waq_icon.rc)

create_target(
        ${executable_name} ${CMAKE_CURRENT_SOURCE_DIR}
        src_dir .
        resource_files ${icon_file} ${rc_version_file}
        target_type "executable"
)

# Add dependencies
set(oss_dependencies delwaq_lib)
target_link_libraries(${executable_name} ${oss_dependencies})

# Set additional compilation properties
target_compile_options(${executable_name} PRIVATE ${waq_default_flags})

if (UNIX)
    target_include_directories(${executable_name} PRIVATE ${version_include_dir})
    message(STATUS "netcdf lib dir is ${NETCDF_LIBRARY_DIRS}")
    target_link_directories(${executable_name} PRIVATE ${NETCDF_LIBRARY_DIRS})
    target_link_options(${executable_name} PRIVATE ${openmp_flag})
endif (UNIX)

if (WIN32)
    target_include_directories(${executable_name} PRIVATE ${version_include_dir} ${mpi_include_path})

    # Set linker properties
    target_link_directories(${executable_name}
            PRIVATE
            "${mpi_library_path}"
            "${checkout_src_root}/third_party_open/netcdf/${netcdf_version}/lib"
            "${checkout_src_root}/third_party_open/pthreads/bin/x64")

    target_link_libraries(${executable_name}
            "comctl32.lib"
            "wsock32.lib"
            "netapi32.lib"
            "kernel32.lib"
            "user32.lib"
            "Shlwapi.lib"
            "Gdi32.lib"
            "Winspool.lib"
            "netcdf.lib"
            "${mpi_fortran_library}"
            "pthreadVC2.lib")

    # Set linker options
    target_link_options(${executable_name} PRIVATE ${nologo_flag})
    set_target_properties(${executable_name} PROPERTIES LINK_FLAGS "/LARGEADDRESSAWARE /STACK:20000000")

    configure_visual_studio_user_file(${executable_name})
endif(WIN32)

target_compile_options(${executable_name} PRIVATE ${openmp_flag})
set_target_properties(${executable_name} PROPERTIES FOLDER engines_gpl/waq)

install(TARGETS ${executable_name} RUNTIME  DESTINATION bin)
install(PROGRAMS ${CMAKE_CURRENT_LIST_DIR}/../resources/scripts/run_delwaq.${platform_extension}  DESTINATION bin)