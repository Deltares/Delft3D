set(executable_name ec_module_test)

file(GLOB source    src/*.f90
                    src/*.F90)

add_executable(${executable_name} ${source})

set(oss_dependencies deltares_common
                    deltares_common_c
                    deltares_common_mpi
                    ec_module
                    ftnunit
                    gridgeom
                    io_netcdf
                    io_netcdf_data
                    kdtree_wrapper
                    kdtree2
                    netcdf-fortran::netcdf-fortran
                    )

target_link_libraries(${executable_name} ${oss_dependencies})
if(UNIX)
    set_property(TARGET ${executable_name} PROPERTY LINKER_LANGUAGE Fortran)
endif(UNIX)

set_source_files_properties(${CMAKE_CURRENT_SOURCE_DIR}/src/ec_module_test.F90
                            ${CMAKE_CURRENT_SOURCE_DIR}/src/m_ec_module_test.F90
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

# Create the folder structure in vfproj
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source})
set_target_properties (${executable_name} PROPERTIES FOLDER tests/utils_lgpl/ec_module)

configure_visual_studio_user_file(${executable_name})

if(WIN32)
install (DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/scripts/ DESTINATION test)
endif()
install (DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/tests DESTINATION test)
install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/. DESTINATION test FILES_MATCHING PATTERN "*.py" PATTERN "src" EXCLUDE PATTERN "scripts" EXCLUDE PATTERN "tests" EXCLUDE)
install(TARGETS ${executable_name} RUNTIME  DESTINATION test)

