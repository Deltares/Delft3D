# Shared definitions

message(STATUS "swan omp:")

# Don't put the Fortran modules of the SWAN OpenMP executable build in a central location
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
set(executable_name swan_omp)
add_executable(${executable_name}   ${headers}
                                    ${rc_version_file}
                                    ${executable_files})

# Set dependencies
if (WIN32)
    set(exe_dependencies netcdf4
                         netcdff)

    oss_include_libraries(${executable_name} exe_dependencies)
    target_link_libraries(${executable_name} ${exe_dependencies})

    include_directories("${CMAKE_CURRENT_SOURCE_DIR}/../include"
                        ${version_include_dir})
    # Set linker properties
    message(STATUS "Setting linker properties in windows")
    target_link_directories(${executable_name}
                            PRIVATE
                            "${checkout_src_root}/third_party_open/netcdf/${netcdf_version}/lib")

    target_link_libraries(${executable_name}
                            "netcdf.lib")

    # Set linker options
    message(STATUS "Setting target_link_options in windows")
    target_link_options(${executable_name} PRIVATE ${nologo_flag})
    set_target_properties(${executable_name} PROPERTIES COMPILE_FLAGS ${openmp_flag})
endif(WIN32)

if(UNIX)
    # the `pkg_check_modules` function is created with this call
    find_package(PkgConfig REQUIRED)

    # these calls create special `PkgConfig::<MODULE>` variables
    pkg_check_modules(NETCDF REQUIRED IMPORTED_TARGET netcdf)

    target_link_libraries(${executable_name}
         PkgConfig::NETCDF
         netcdff)

    include_directories("${CMAKE_CURRENT_SOURCE_DIR}/../include"
                        ${version_include_dir}
                        ${CMAKE_BINARY_DIR}/fortran_module_dir)

    add_definitions(-DHAVE_CONFIG_H=1)
    set_target_properties(${executable_name} PROPERTIES COMPILE_FLAGS "${openmp_flag}")
    target_link_options(${executable_name} PRIVATE ${openmp_flag})
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
    target_compile_definitions(${executable_name} PRIVATE "WIN32;SWANEXE")
endif(WIN32)

if (UNIX)
    target_compile_definitions(${executable_name} PRIVATE "SWANEXE")
endif(UNIX)

# Define how the files should be structured within Visual Studio
source_group(TREE ${checkout_src_root}/third_party_open/swan FILES ${executable_files})
set_target_properties (${executable_name} PROPERTIES FOLDER third_party_open/swan_omp)

if (WIN32)
    set_target_properties(${executable_name} PROPERTIES LINK_FLAGS "/LARGEADDRESSAWARE /STACK:20000000")
    configure_visual_studio_user_file(${executable_name})
endif(WIN32)

# Set post-build step
set(install_dir ${CMAKE_BINARY_DIR})
set(build_dir ${CMAKE_BINARY_DIR})


install(TARGETS ${executable_name} RUNTIME  DESTINATION bin)

if (UNIX)
    install(PROGRAMS ../scripts/swan.sh  DESTINATION bin)
endif(UNIX)

if (WIN32)
    install(PROGRAMS ../scripts/swan.bat DESTINATION bin)
endif(WIN32)
