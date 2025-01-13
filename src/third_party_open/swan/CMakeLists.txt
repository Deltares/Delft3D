message(STATUS "swan lib:")
# Include directories that are required as references
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/include)

# Set the directory of where the source code is located
set(src_path src) 
set(version_include_dir ${CMAKE_SOURCE_DIR}/../version_includes)

# Gather the source files
set(headers include/swanout1_deltares.inc)

get_fortran_source_files(${src_path} swan_sourcefiles)
list(FILTER swan_sourcefiles EXCLUDE REGEX ".*HottifySWAN.f90$")
list(FILTER swan_sourcefiles EXCLUDE REGEX ".*swanhcat.for$")


# Define library
set(library_name swan)
set(library_source_list     ${headers}
                            ${swan_sourcefiles})
add_library(${library_name} ${library_source_list})

target_link_libraries(${library_name} PRIVATE no_compiler_warnings)

# Add dependencies
if (UNIX)
 # the `pkg_check_modules` function is created with this call
    find_package(PkgConfig REQUIRED)

    # these calls create special `PkgConfig::<MODULE>` variables
    pkg_check_modules(NETCDF REQUIRED IMPORTED_TARGET netcdf)
    pkg_check_modules(NETCDF_FTN REQUIRED IMPORTED_TARGET netcdf-fortran)
    target_link_libraries(${library_name} PRIVATE
         PkgConfig::NETCDF
         netcdff)

    message (${CMAKE_BINARY_DIR}/netcdff)
    include_directories("${CMAKE_CURRENT_SOURCE_DIR}/include"
                        ${version_include_dir}
                        ${CMAKE_BINARY_DIR}/netcdff)

    add_definitions(-DHAVE_CONFIG_H=1)
endif(UNIX)

if (WIN32)
    set(oss_dependencies    netcdf4
                            netcdff)
    target_link_libraries(${library_name} PRIVATE ${oss_dependencies})
    include_directories("${CMAKE_CURRENT_SOURCE_DIR}/include"
                        ${mpi_module_path}
                        ${version_include_dir})
endif(WIN32)


# Define additional compilation flags
set_source_files_properties(${CMAKE_CURRENT_SOURCE_DIR}/src/ocpids.F
                            ${CMAKE_CURRENT_SOURCE_DIR}/src/swancom1.F
                            ${CMAKE_CURRENT_SOURCE_DIR}/src/swanmain.F
                            ${CMAKE_CURRENT_SOURCE_DIR}/src/swanparll.F
                            ${CMAKE_CURRENT_SOURCE_DIR}/src/swmod2.F
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

# Set additional compilation properties, specific for Debug mode
target_compile_options(${library_name} PRIVATE "SHELL:$<$<CONFIG:Debug>:${check_nopointers_flag}>")

# Define preprocessor definitions 
if (WIN32)
    target_compile_definitions(${library_name} PRIVATE "WIN32;USE_MPI")
endif(WIN32)

if (UNIX)
    target_compile_definitions(${library_name} PRIVATE "USE_MPI")
endif(UNIX)

# Create the folder structure in vfproj
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${library_source_list})
set_target_properties (${library_name} PROPERTIES FOLDER third_party_open/swan_lib)

