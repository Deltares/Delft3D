# Set include directores
set(esmfsm_include_path ${CMAKE_CURRENT_SOURCE_DIR}/../../include) # Trace back to the esmfsm path
set(include_path include)
include_directories(${include_path}
                    ${esmfsm_include_path})

# Set include path to have it accessible for other components
set(public_include_path ${esmfsm_include_path})

# Set directory which contains all source files
set(src_path src)

# Gather source files
get_fortran_source_files(${src_path}/fsm/ fsm_files)

# Define library
set(library_name esmfsm)
add_library(${library_name} ${fsm_files})

# Set dependencies
set(oss_dependencies    deltares_common
                        esmfsm_c
                        esmfsm_version_number) 

target_link_libraries(${library_name} ${oss_dependencies})

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${fsm_files})
set_target_properties (${library_name} PROPERTIES FOLDER utils_lgpl/esmfsm)
