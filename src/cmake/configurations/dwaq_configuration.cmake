#
# D-WAQ kernel
#=============
include(${CMAKE_CURRENT_LIST_DIR}/include/dwaq/dwaq_base.cmake)
include(${CMAKE_CURRENT_LIST_DIR}/include/dwaq/dwaq_kernel.cmake)
include(${CMAKE_CURRENT_LIST_DIR}/include/dwaq/dwaq_dflowfm_online_coupling.cmake)

#
# D-Waq tools
#=============
include(${CMAKE_CURRENT_LIST_DIR}/include/dwaq/dwaq_tools.cmake)

#
# D-Part kernel
#=============
include(${CMAKE_CURRENT_LIST_DIR}/include/dwaq/dpart.cmake)

#
# Third party libraries for D-Waq
#=============
include(${CMAKE_CURRENT_LIST_DIR}/include/dwaq/dwaq_third_party.cmake)


#
# Utils for D-Waq
#=============
include(${CMAKE_CURRENT_LIST_DIR}/include/dwaq/dwaq_utils.cmake)


#
# Linux installation
#=============
if(UNIX)
    add_subdirectory(${checkout_src_root}/${install_waq_module} install_waq)
endif()


#
# Unit tests for D-Waq
#=============
include(${CMAKE_CURRENT_LIST_DIR}/include/dwaq/dwaq_tests.cmake)


# Project name must be at the end of the configuration: it might get a name when including other configurations and needs to overwrite that
project(dwaq)
