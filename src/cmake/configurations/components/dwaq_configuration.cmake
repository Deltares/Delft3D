set(waq_include_dir ${CMAKE_CURRENT_LIST_DIR}/dwaq)
include(${waq_include_dir}/dwaq_functions.cmake)

list(APPEND CMAKE_MESSAGE_INDENT "  ")

set(include_paths_kernel ${waq_include_dir}/dwaq_base.cmake
                         ${waq_include_dir}/dwaq_kernel.cmake
                         ${waq_include_dir}/dwaq_dflowfm_online_coupling.cmake)

include_component("D-WAQ kernel" "${include_paths_kernel}")
include_component("D-WAQ tools" ${waq_include_dir}/dwaq_tools.cmake)
include_component("D-PART kernel" ${waq_include_dir}/dpart.cmake)
include_component("D-WAQ third party libraries" ${waq_include_dir}/dwaq_third_party.cmake)
include_component("D-WAQ utils" ${waq_include_dir}/dwaq_utils.cmake)
include_component("D-WAQ unit and integration tests" ${waq_include_dir}/dwaq_tests.cmake)

# Installation
add_target_with_subdirectory(install_waq ${install_waq_module})

#intel MPI & MKL
if(WIN32)
   if(${configuration_type} STREQUAL ${dwaq_configuration})
        message(STATUS "Intel MPI & MKL")
        list(APPEND CMAKE_MESSAGE_INDENT "   ")

        add_target_with_subdirectory(intelredist ${intelredist_module})
        add_target_with_subdirectory(pthreads ${pthreads_module})

        list(POP_BACK CMAKE_MESSAGE_INDENT)
   endif()
endif(WIN32)

# Project name must be at the end of the configuration: it might get a name when including other configurations and needs to overwrite that
project(dwaq)

list(POP_BACK CMAKE_MESSAGE_INDENT)