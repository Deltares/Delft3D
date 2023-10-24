# Define the md5 digest library
set(md5digest_source_path "${CMAKE_CURRENT_SOURCE_DIR}/../md5digest")
set(md5digest_library_name md5digest)
set(md5digest_library_src ${md5digest_source_path}/md5.c ${md5digest_source_path}/md5.h)
add_library(${md5digest_library_name} ${md5digest_library_src})

# Set preprocessor definitions
target_compile_definitions(${md5digest_library_name} PRIVATE _MBCS)

if (UNIX)
   include_directories( ${PROJECT_BINARY_DIR}/md5 )
endif(UNIX)

# Create the folder structure in vfproj
source_group(TREE "${CMAKE_CURRENT_SOURCE_DIR}/.." FILES ${md5digest_library_src})
set_target_properties (${md5digest_library_name} PROPERTIES FOLDER third_party_open/md5)




# Define the md5 file library 
set(md5file_source_path "${CMAKE_CURRENT_SOURCE_DIR}/../md5file")
set(md5file_library_name md5)
set(md5file_library_src ${md5file_source_path}/md5file.f90)
add_library(${md5file_library_name} ${md5file_library_src})

# Add dependencies
add_dependencies(${md5file_library_name} ${md5digest_library_name})

if (UNIX)
   include_directories( ${PROJECT_BINARY_DIR}/md5 )
   target_link_libraries(${md5file_library_name} md5digest)
   target_compile_options(${md5file_library_name} PRIVATE ${c_compiler_flags})
endif(UNIX)

# Set additional compilation properties, specific for Debug mode
target_compile_options(${md5file_library_name} PRIVATE "$<$<CONFIG:Debug>:${check_pointers_flag}>")
target_compile_options(${md5file_library_name} PRIVATE "$<$<CONFIG:Debug>:${floating_point_exception_flag}>")

# Create the folder structure in vfproj
source_group(TREE "${CMAKE_CURRENT_SOURCE_DIR}/.." FILES ${md5file_library_src})
set_target_properties (${md5file_library_name} PROPERTIES FOLDER third_party_open/md5)
