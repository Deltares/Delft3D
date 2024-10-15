# Shared definitions

message(STATUS "swan mpi:")

# Don't put the Fortran modules of the SWAN MPI executable build in a central location
unset(CMAKE_Fortran_MODULE_DIRECTORY)

# Set directory which contains all source files
set(src_path ../src)
set(version_include_dir ${CMAKE_SOURCE_DIR}/../version_includes)

# Gather source files
set(headers ../include/swanout1_deltares.inc)

get_fortran_source_files(${src_path} swan_sourcefiles)
list(FILTER swan_sourcefiles EXCLUDE REGEX ".*HottifySWAN.f90$")
list(FILTER swan_sourcefiles EXCLUDE REGEX ".*swanhcat.for$")

set(executable_files ${swan_sourcefiles})

# Set the directory referring to the res folder
set(swan_path "${CMAKE_CURRENT_SOURCE_DIR}/..")
set(res_path ${swan_path}/res)

# Define executable
set(executable_name swan_mpi)
add_executable(${executable_name}   ${headers}
                                    ${rc_version_file}
                                    ${executable_files})

target_link_libraries(${executable_name} PRIVATE no_compiler_warnings)
# Set dependencies
if (WIN32)
    set(exe_dependencies netcdf4
                         netcdff)

    target_link_libraries(${executable_name} PRIVATE ${exe_dependencies})

    include_directories("${CMAKE_CURRENT_SOURCE_DIR}/../include"
                        ${mpi_module_path}
                        ${version_include_dir})
    # Set linker properties
    message(STATUS "Setting linker properties in windows")
    target_link_directories(${executable_name}
                            PRIVATE
                            "${mpi_library_path}"
                            "${checkout_src_root}/third_party_open/netcdf/${netcdf_version}/lib")

    target_link_libraries(${executable_name} PRIVATE
                          "${mpi_fortran_library}"
                          "netcdf.lib")

    # disable OpenMP
    string(REPLACE "/MP" "" TEMP "${CMAKE_Fortran_FLAGS}")
    set(CMAKE_Fortran_FLAGS ${TEMP})
    string(REPLACE "/MP" "" TEMP "${CMAKE_Fortran_FLAGS_DEBUG}")
    set(CMAKE_Fortran_FLAGS_DEBUG ${TEMP})

    # Set linker options
    message(STATUS "Setting target_link_options in windows")
    target_link_options(${executable_name} PRIVATE ${nologo_flag})
endif(WIN32)

if(UNIX)
    # the `pkg_check_modules` function is created with this call
    find_package(PkgConfig REQUIRED)

    # these calls create special `PkgConfig::<MODULE>` variables
    pkg_check_modules(NETCDF REQUIRED IMPORTED_TARGET netcdf)

    target_link_libraries(${executable_name} PRIVATE
         PkgConfig::NETCDF
         netcdff)

    target_include_directories(${executable_name} PUBLIC netcdff)
    include_directories("${CMAKE_CURRENT_SOURCE_DIR}/../include"
                        ${version_include_dir}
                        ${CMAKE_BINARY_DIR}/fortran_module_dir)

    add_definitions(-DHAVE_CONFIG_H=1)
endif(UNIX)

# Define additional compilation flags
set_source_files_properties(${checkout_src_root}/third_party_open/swan/src/ocpids.F
                            ${checkout_src_root}/third_party_open/swan/src/swancom1.F
                            ${checkout_src_root}/third_party_open/swan/src/swanmain.F
                            ${checkout_src_root}/third_party_open/swan/src/swanparll.F
                            ${checkout_src_root}/third_party_open/swan/src/swmod2.F
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

# Set additional compilation properties, specific for Debug mode
target_compile_options(${executable_name} PRIVATE "$<$<CONFIG:Debug>:${check_nopointers_flag}>")

# Define preprocessor definitions
if (WIN32)
    target_compile_definitions(${executable_name} PRIVATE "WIN32;USE_MPI;SWANEXE")
endif(WIN32)

if (UNIX)
    target_compile_definitions(${executable_name} PRIVATE "USE_MPI;SWANEXE")
endif(UNIX)

# Define how the files should be structured within Visual Studio
source_group(TREE ${checkout_src_root}/third_party_open/swan FILES ${executable_files})
set_target_properties (${executable_name} PROPERTIES FOLDER third_party_open/swan_mpi)

if (WIN32)
    set_target_properties(${executable_name} PROPERTIES LINK_FLAGS "/LARGEADDRESSAWARE /STACK:20000000")
endif(WIN32)

# Set post-build step
set(install_dir ${CMAKE_BINARY_DIR})
set(build_dir ${CMAKE_BINARY_DIR})

install(TARGETS ${executable_name} RUNTIME  DESTINATION bin)

    install(PROGRAMS ../scripts/swan.${platform_extension}  DESTINATION bin)
