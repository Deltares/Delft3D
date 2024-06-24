# tests
set(dwaq_dir ${dir}/src/cmake/configurations/include/dwaq)

if(NOT TARGET ftnunit)
    add_subdirectory(${checkout_src_root}/${ftnunit_module} ftnunit)
endif()

include(${dwaq_dir}/dwaq_kernel.cmake)
# include(${dir}/src/cmake/configurations/dwaq_configuration.cmake)

#
# D-Waq tools
#=============
include(${dwaq_dir}/dwaq_tools.cmake)



#
# D-Part kernel
#=============
include(${dwaq_dir}/dpart.cmake)


#
# Third party libraries for D-Waq
#=============
include(${dwaq_dir}/dwaq_third_party.cmake)


#
# Utils for D-Waq
#=============
include(${dwaq_dir}/dwaq_utils.cmake)


# add_subdirectory(dimr test_dimr)
# add_subdirectory(delwaq_lib test_delwaq_lib)
# add_subdirectory(delwaq2 test_delwaq2)
add_subdirectory(waq_kernel test_waq_kernel)
add_subdirectory(waq_io test_waq_io)
add_subdirectory(waq_process test_waq_process)
add_subdirectory(waq_utils test_waq_waq_utils)
