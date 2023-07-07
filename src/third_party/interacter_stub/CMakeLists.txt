# Define the library 
set(library_name interacter_stub)

get_fortran_source_files(src source_files)
add_library(${library_name} ${source_files})

 # Create the folder structure in vfproj
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
set_target_properties (${library_name} PROPERTIES FOLDER third_party_open)

