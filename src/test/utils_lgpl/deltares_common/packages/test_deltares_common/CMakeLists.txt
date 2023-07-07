file(GLOB source    src/*.f90)
list(REMOVE_ITEM source ${CMAKE_CURRENT_SOURCE_DIR}/src/test_precision_body.f90)

set(executable_name test_deltares_common)

add_executable(${executable_name} ${source})

set(oss_dependencies deltares_common  ftnunit)
oss_include_libraries(${executable_name} oss_dependencies)

target_link_libraries(${executable_name}  deltares_common 
                                          ftnunit)

if(UNIX)
    set_property(TARGET ${executable_name} PROPERTY LINKER_LANGUAGE Fortran)
endif(UNIX)

# post-build
set(install_dir ${CMAKE_BINARY_DIR})
set(build_dir ${CMAKE_BINARY_DIR})
set(build_project ${executable_name})

post_build_target( ${executable_name}
                   ${install_dir} 
                   ${build_dir} 
                   ${checkout_src_root} 
                   ${build_project} )

install(TARGETS ${executable_name} RUNTIME  DESTINATION bin)

