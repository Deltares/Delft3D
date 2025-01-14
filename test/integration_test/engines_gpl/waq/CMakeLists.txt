# tests
set(dwaq_dir ${dir}/src/cmake/configurations/components/dwaq)

if(NOT TARGET ftnunit)
    add_subdirectory(${checkout_src_root}/${ftnunit_module} ftnunit)
endif()

include(${dwaq_dir}/dwaq_kernel.cmake)


#
# D-Waq tools
# =============
include(${dwaq_dir}/dwaq_tools.cmake)

#
# D-Part kernel
# =============
include(${dwaq_dir}/dpart.cmake)

#
# Third party libraries for D-Waq
# =============
include(${dwaq_dir}/dwaq_third_party.cmake)

#
# Utils for D-Waq
# =============
include(${dwaq_dir}/dwaq_utils.cmake)

add_subdirectory(waq_bmi test_waq_bmi)