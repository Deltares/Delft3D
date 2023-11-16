# Include all directories
set(src_path .)

# Gather the source files
get_fortran_source_files(${src_path} source)

# Executable target
set(executable_name test_waqlib_multi)
add_executable(${executable_name} ${source})

# Extra dependencies
set(oss_dependencies delwaq_lib)

# Link libraries
target_link_libraries(${executable_name} ${oss_dependencies})

# Other link libraries
if (WIN32)
target_link_directories(${executable_name} PRIVATE
                        "${mpi_library_path}"
                        "${checkout_src_root}/third_party_open/pthreads/bin/x64"
                        )
endif(WIN32)
if (UNIX)
    set_property(TARGET ${executable_name} PROPERTY LINKER_LANGUAGE Fortran)
endif(UNIX)

set_target_properties(${executable_name} PROPERTIES FOLDER engines_gpl/waq/delwaq_lib_tests)

# Define how the files should be visualized in visual studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source})
