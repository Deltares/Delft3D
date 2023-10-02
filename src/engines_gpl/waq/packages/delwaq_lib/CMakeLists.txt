# Define include directories
# Note that the CMAKE_CURRENT_SOURCE_DIR has a path to THIS CMakeLists location
set(waq_root_path ${CMAKE_CURRENT_SOURCE_DIR}/../../) # Trace back to the WAQ path
include_directories(${waq_root_path}/include)

# Set version file variables
set(waq_version_path ${waq_root_path}/version)

set(rc_version_file ${waq_version_path}/version_number.rc)
set(version_include_dir ${CMAKE_SOURCE_DIR}/../version_includes)

# Set directory which contains all source files
set(src_path src)

# Gather source files
get_fortran_source_files(${src_path} source_files)
file(GLOB preprocessor_files ${CMAKE_CURRENT_SOURCE_DIR}/${src_path}/waq_bmi.F90)

# Define library
set(library_name delwaq_lib)
add_library(${library_name} SHARED ${source_files}
                                   ${rc_version_file})

# Set dependencies
set(oss_dependencies deltares_common
                     deltares_common_c
                     waq_data
                     waq_io
                     waq_memory
                     waq_kernel
                     waq_process
                     waq_validation
                     waq_utils_c
                     waq_utils_f)

oss_include_libraries(${library_name} oss_dependencies)

if (WIN32)
    target_link_libraries(${library_name} ${oss_dependencies})
    target_include_directories(${library_name} PRIVATE ${mpi_include_path})

    # Set linker properties
    message(STATUS "Setting linker properties in windows")
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
    message(STATUS "Setting target_link_options in windows")
    target_link_options(${library_name} PRIVATE ${nologo_flag})
endif(WIN32)

if (UNIX)
    target_link_libraries(${library_name} ${oss_dependencies}
                                          uuid)
endif(UNIX)

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
source_group(Resources FILES    ${rc_version_file})

target_compile_options(${library_name} PRIVATE "${openmp_flag}")
target_compile_options(${library_name} PRIVATE "${waq_default_flags}")

target_include_directories(${library_name} PRIVATE ${version_include_dir})
set_target_properties(${library_name} PROPERTIES FOLDER engines_gpl/waq)

if(UNIX)
    target_link_options(${library_name} PRIVATE ${openmp_flag})
    set_property(TARGET ${library_name} PROPERTY LINKER_LANGUAGE Fortran)
endif(UNIX)


# Change the name of the target library to delwaq.dll
set_target_properties (${library_name} PROPERTIES OUTPUT_NAME delwaq)

# Set post-build step
set(install_dir ${CMAKE_BINARY_DIR})
set(build_dir ${CMAKE_BINARY_DIR})

post_build_target (${library_name}
                   ${install_dir}
                   ${build_dir}
                   ${checkout_src_root}
                   ${library_name})

install(TARGETS ${library_name} DESTINATION lib)
if (UNIX)
    install(FILES ${CMAKE_SOURCE_DIR}/../engines_gpl/waq/resources/bloom.spe          DESTINATION share/delft3d)
    install(FILES ${CMAKE_SOURCE_DIR}/../engines_gpl/waq/resources/proc_def.dat       DESTINATION share/delft3d)
    install(FILES ${CMAKE_SOURCE_DIR}/../engines_gpl/waq/resources/proc_def.def       DESTINATION share/delft3d)
    install(DIRECTORY  ${CMAKE_SOURCE_DIR}/../engines_gpl/waq/resources/csvFiles      DESTINATION share/delft3d)
    install(DIRECTORY  ${CMAKE_SOURCE_DIR}/../engines_gpl/waq/resources/subFiles      DESTINATION share/delft3d)
endif(UNIX)
