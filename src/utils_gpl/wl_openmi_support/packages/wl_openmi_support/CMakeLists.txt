set(src_path src) 

# Set version file variables
set(ini_version_file ${CMAKE_CURRENT_SOURCE_DIR}/src/version_number.ini)
set(fortran_version_file ${CMAKE_CURRENT_SOURCE_DIR}/src/wlopenmi_version.F90)

# Generate the F90 version file
# generate_version_files(${fortran_version_file} ${ini_version_file})

# Gather source files
get_fortran_source_files(${src_path} source_files)
# Remove files that should not be compiled
list(REMOVE_ITEM source_files "${CMAKE_CURRENT_SOURCE_DIR}/${src_path}/OESOpenMI_omi.F90")


# Define the library
set(library_name wl_openmi_support)
add_library(${library_name} ${source_files})

# Specify dependencies
set(oss_dependencies delftio)

target_link_libraries(${library_name} ${oss_dependencies})


set_source_files_properties(
                   src/wlopenmi_version.F90
                   src/WLOpenMISupport.F90
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
target_compile_options(${library_name} PRIVATE "SHELL:$<$<CONFIG:Debug>:${check_bounds_flag}>")

# Create the folder structure in vfproj
if (WIN32)
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
set_target_properties (${library_name} PROPERTIES FOLDER utils_gpl/${library_name})
include_directories("${CMAKE_CURRENT_SOURCE_DIR}/include")
endif(WIN32)
