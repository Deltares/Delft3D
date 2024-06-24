set(library_name delwaq_lib)

# Set version file variables
set(waq_root_path ${CMAKE_CURRENT_SOURCE_DIR}/../)
set(rc_version_file ${waq_root_path}/resources/version/version_number.rc)
set(version_include_dir ${CMAKE_SOURCE_DIR}/../version_includes)

create_target(
        ${library_name} ${CMAKE_CURRENT_SOURCE_DIR}
        src_dir .
        recursive TRUE
        resource_files ${rc_version_file}
        shared
)

# Set dependencies
set(oss_dependencies waq_hyd_data
        waq_io
        waq_memory
        waq_kernel
        waq_data
        waq_process
        waq_utils_f)

if (WIN32)
    target_link_libraries(${library_name} ${oss_dependencies})

    # Set linker properties
    target_link_directories(${library_name}
            PRIVATE
            "${mpi_library_path}"
            "${checkout_src_root}/third_party_open/netcdf/${netcdf_version}/lib"
            "${checkout_src_root}/third_party_open/pthreads/bin/x64")


    target_link_libraries(${library_name}
            "libiomp5md.lib"
            "${mpi_fortran_library}"
            "WbeMuuid.lib"
            "ole32.lib"
            "oleaut32.lib"
            "wsock32.lib"
            "netapi32.lib"
            "kernel32.lib"
            "user32.lib"
            "Shlwapi.lib"
            "Gdi32.lib"
            "Winspool.lib"
            "netcdf.lib"
            "pthreadVC2.lib")

    # Set linker options
    target_link_options(${library_name} PRIVATE ${nologo_flag})
endif (WIN32)

if (UNIX)
    target_link_libraries(${library_name} ${oss_dependencies}
            uuid)
    target_link_options(${library_name} PRIVATE ${openmp_flag})
endif (UNIX)

target_compile_options(${library_name} PRIVATE "${openmp_flag}")
target_compile_options(${library_name} PRIVATE "${waq_default_flags}")

target_include_directories(${library_name} PRIVATE ${version_include_dir})
set_target_properties(${library_name} PROPERTIES FOLDER engines_gpl/waq)

# Change the name of the target library to delwaq.dll
set_target_properties(${library_name} PROPERTIES OUTPUT_NAME delwaq)

install(TARGETS ${library_name} RUNTIME DESTINATION lib)
install(FILES ${CMAKE_SOURCE_DIR}/../engines_gpl/waq/resources/process_lib/bloom.spe DESTINATION share/delft3d)
install(FILES ${CMAKE_SOURCE_DIR}/../engines_gpl/waq/resources/process_lib/proc_def.dat DESTINATION share/delft3d)
install(FILES ${CMAKE_SOURCE_DIR}/../engines_gpl/waq/resources/process_lib/proc_def.def DESTINATION share/delft3d)
install(DIRECTORY ${CMAKE_SOURCE_DIR}/../engines_gpl/waq/resources/process_lib/csvFiles DESTINATION share/delft3d)
install(DIRECTORY ${CMAKE_SOURCE_DIR}/../engines_gpl/waq/resources/process_lib/subFiles DESTINATION share/delft3d)