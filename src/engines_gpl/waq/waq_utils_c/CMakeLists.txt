set(library_name waq_utils_c)

create_target(${library_name} ${CMAKE_CURRENT_SOURCE_DIR} src_files fortran_to_c_interfacing.cpp)

set_target_properties(waq_utils_c PROPERTIES FOLDER engines_gpl/waq)