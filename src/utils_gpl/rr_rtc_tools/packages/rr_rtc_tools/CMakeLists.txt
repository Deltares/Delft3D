set(src_path src) 

# Gather source files
get_fortran_source_files(${src_path} source_files)

# Define the library
set(library_name rr_rtc_tools)
add_library(${library_name} ${source_files})

# Specify dependencies
set(oss_dependencies)
oss_include_libraries(${library_name} oss_dependencies)

if(UNIX)
    target_link_libraries(${library_name} ${oss_dependencies})
endif()

set_source_files_properties(
                   src/GPSLEEP.F90
                   PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

target_compile_definitions(${library_name} PRIVATE "X64")
# Set preprocessor definitions
if(WIN32)
    target_compile_definitions(${library_name} PRIVATE "WIN32")
endif(WIN32)
if(UNIX)
    target_compile_definitions(${library_name} PRIVATE "HAVE_CONFIG_H")
endif(UNIX)

# Set additional compilation properties, specific for Debug mode
target_compile_options(${library_name} PRIVATE "$<$<CONFIG:Debug>:${check_bounds_flag}>")

 # Create the folder structure in vfproj
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
set_target_properties (${library_name} PROPERTIES FOLDER utils_gpl/${library_name})
