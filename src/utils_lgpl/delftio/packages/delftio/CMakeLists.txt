# Set path for include files 
# Note that the CMAKE_CURRENT_SOURCE_DIR has a path to THIS CMakeLists location
set(delftio_include_path ${CMAKE_CURRENT_SOURCE_DIR}/../../include)
set(version_include_dir ${CMAKE_SOURCE_DIR}/../version_includes)

include_directories(${delftio_include_path}
                    ${version_include_dir})

# Set include path to have it accessible for other components
set(public_include_path ${delftio_include_path})

# Set directory which contains all source files
set(src_path src)

# Gather source files
get_fortran_source_files(${src_path}/delftio_sync delftio_sync)
get_fortran_source_files(${src_path}/diof90 diof90)
get_fortran_source_files(${src_path}/diof90nefis diof90nefis)

# Define library
set(library_name delftio)
add_library(${library_name} ${delftio_sync}
                            ${diof90}
                            ${diof90nefis})

# Set additional compilation properties
set_target_properties(${library_name} PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

# Set dependencies
set(oss_dependencies    deltares_common
                        deltares_common_c
                        delftio_shm) 

target_link_libraries(${library_name} ${oss_dependencies})

# Set preprocessor definitions
if(WIN32)
    target_compile_definitions(${library_name} PRIVATE "WIN32")
endif(WIN32)
if(UNIX)
    target_compile_definitions(${library_name} PRIVATE "HAVE_CONFIG_H")
endif(UNIX)

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${delftio_sync}
                                                    ${diof90}
                                                    ${diof90nefis})
set_target_properties (${library_name} PROPERTIES FOLDER utils_lgpl/delft_io)
