# Set directory which contains all source files
set(src_path src)

# Gather source files
get_fortran_source_files(${src_path}/bloom bloom_source)

# Gather source files
get_fortran_source_files(${src_path}/charon charon_source)

# Gather source files
get_fortran_source_files(${src_path}/protist protist_source)

# Gather source files
get_fortran_source_files(${src_path}/proc_preprocess proc_preprocess_source)

# Gather source files
get_fortran_source_files(${src_path}/trwp trwp_source)

# Gather source files
get_fortran_source_files(${src_path}/waq_process waq_process_source)


# Define library
set(library_name waq_process)
add_library(${library_name} ${bloom_source}
                            ${charon_source}
                            ${protist_source}
                            ${proc_preprocess_source}
                            ${trwp_source}
                            ${waq_process_source})


# Set dependencies
set(oss_dependencies_general deltares_common
                             nefis
                             solvesaphe
                             waq_data
                             waq_utils_c
                             waq_utils_f)


if (UNIX)
    # the `pkg_check_modules` function is created with this call
    find_package(PkgConfig REQUIRED)

    # these calls create special `PkgConfig::<MODULE>` variables
    pkg_check_modules(NETCDF     REQUIRED IMPORTED_TARGET netcdf)
    pkg_check_modules(NETCDF_FTN REQUIRED IMPORTED_TARGET netcdf-fortran)

    set(oss_dependencies ${oss_dependencies_general})

    target_link_libraries(${library_name}
         ${oss_dependencies}
         PkgConfig::NETCDF
         PkgConfig::NETCDF_FTN)
endif(UNIX)
if (WIN32)
    set(oss_dependencies ${oss_dependencies_general}
                         netcdf4
                         netcdff)

    target_link_libraries(${library_name} ${oss_dependencies})
endif(WIN32)
oss_include_libraries(${library_name} oss_dependencies)


target_include_directories(${library_name}  PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/../../include")

# Set additional compilation properties
target_compile_options(${library_name} PRIVATE "${qauto_threaded_flags}")
target_compile_options(${library_name} PRIVATE "${waq_default_flags}")

# Define how the files should be structured in Visual studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${bloom_source}
                                                    ${charon_source}
                                                    ${protist_source}
                                                    ${proc_preprocess_source}
                                                    ${trwp_source}
                                                    ${waq_process_source})

set_target_properties (${library_name} PROPERTIES FOLDER engines_gpl/waq)

