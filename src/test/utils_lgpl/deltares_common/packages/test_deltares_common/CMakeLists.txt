file(GLOB source    src/*.f90)
list(REMOVE_ITEM source ${CMAKE_CURRENT_SOURCE_DIR}/src/test_precision_body.f90)

set(executable_name test_deltares_common)

add_executable(${executable_name} ${source})

set(oss_dependencies deltares_common  ftnunit)

target_link_libraries(${executable_name}  ${oss_dependencies})

if(UNIX)
    set_property(TARGET ${executable_name} PROPERTY LINKER_LANGUAGE Fortran)
endif(UNIX)

# Create the folder structure in vfproj
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source})
set_target_properties (${executable_name} PROPERTIES FOLDER tests/utils_lgpl/deltares_common)

# post-build
set(install_dir ${CMAKE_BINARY_DIR})
set(build_dir ${CMAKE_BINARY_DIR})
set(build_project ${executable_name})

configure_visual_studio_user_file(${executable_name})

install(TARGETS ${executable_name} RUNTIME  DESTINATION test)
if(WIN32)
install (DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/scripts/ DESTINATION test)
endif()
install (DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/test_data DESTINATION test)



