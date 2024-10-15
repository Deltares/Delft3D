set(executable_name nesthd2)

# Set version file variables
set(fortran_version_file "${CMAKE_CURRENT_SOURCE_DIR}/src/nesthd2_common_version.F90")
set(version_include_dir ${CMAKE_SOURCE_DIR}/../version_includes)
set(src_path src)

# Include directories that are required as references
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/include)

# Set version file variables

# Gather source files
get_fortran_source_files(${src_path} sources)

                            
# Define the executable
add_executable(${executable_name} ${sources} ${fortran_version_file})


# Set dependencies
set(oss_dependencies    deltares_common
                        deltares_common_c
                        nefis)

# Add dependencies
if (UNIX)
    # the `pkg_check_modules` function is created with this call
    find_package(PkgConfig REQUIRED)

    target_link_libraries(${executable_name} ${oss_dependencies})
endif(UNIX)

if (WIN32)
    target_link_libraries(${executable_name} ${oss_dependencies})
endif(WIN32)

if(UNIX)
    message(STATUS "Setting target_compile_definitions in Unix")    
   set_property(TARGET ${executable_name} PROPERTY LINKER_LANGUAGE Fortran)
endif(UNIX)

if (WIN32)
   target_compile_definitions(${executable_name} PRIVATE WIN64)
endif(WIN32)

# Set additional compilation properties, specific for Debug mode
target_compile_options(${executable_name} PRIVATE "$<$<CONFIG:Debug>:${check_pointers_flag}>")
target_compile_options(${executable_name} PRIVATE "$<$<CONFIG:Debug>:${floating_point_exception_flag}>")

set_source_files_properties(${fortran_version_file}
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

if (WIN32)
    # Set linker properties
    message(STATUS "Setting linker properties in windows")

    message(STATUS "Setting target_compile_definitions in windows")
    target_compile_definitions(${executable_name} PRIVATE "WIN64")

    # Set linker options
    message(STATUS "Setting target_link_options in windows")
    target_link_options(${executable_name} PRIVATE ${nologo_flag})
endif(WIN32)

target_include_directories(${executable_name}  PRIVATE ${version_include_dir})

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${sources} ${fortran_version_file})
set_target_properties (${executable_name} PROPERTIES FOLDER tools_gpl/nesthd2)
install(TARGETS ${executable_name} RUNTIME  DESTINATION bin)


# Set post-build step
set(install_dir ${CMAKE_BINARY_DIR})
set(build_dir ${CMAKE_BINARY_DIR})


install(TARGETS ${executable_name} RUNTIME  DESTINATION bin)
