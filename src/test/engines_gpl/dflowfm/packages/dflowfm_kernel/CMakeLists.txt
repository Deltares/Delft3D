set(executable_name dflowfm_kernel_test)

file(GLOB source src/*.f90)

add_executable(${executable_name} ${source})

target_link_libraries(${executable_name} PRIVATE
    deltares_common
    deltares_common_c
    deltares_common_mpi
    dhydrology_kernel
    ec_module
    flow1d
    flow1d_core
    flow1d_io
    ftnunit
    gridgeom
    io_netcdf
    io_netcdf_data
    kdtree_wrapper
    kdtree2
    fortrangis
    libsigwatch
    md5digest
    md5
    morphology_data
    morphology_io
    morphology_kernel
    morphology_plugins_c
    polypack
    trachytopes_io
    trachytopes_kernel
    triangle::triangle
    wq_processes
    waq_process
    waq_utils_c
    waq_utils_f
    interacter_stub
    dflowfm_kernel
)

if(UNIX)
    set(THREADS_PREFER_PTHREAD_FLAG ON)
    find_package(Threads REQUIRED)

    target_link_libraries(${executable_name} PRIVATE Threads::Threads)

    set_property(TARGET ${executable_name} PROPERTY LINKER_LANGUAGE Fortran)
endif(UNIX)

include_directories(${mpi_module_path})

# Create the folder structure in vfproj
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source})
set_target_properties (${executable_name} PROPERTIES FOLDER tests/engines_gpl/dflowfm)

configure_visual_studio_user_file(${executable_name})

install (DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/../test_dflowfm/test_data DESTINATION test)
if (WIN32)
	install (DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/scripts/ DESTINATION test)
endif()
install(TARGETS ${executable_name} RUNTIME  DESTINATION test)

# Regression test for observation values after same-time BMI mutations.
create_test(
    dflowfm_bmi_cache_test
    dependencies dflowfm_dll
    visual_studio_folder tests/engines_gpl/dflowfm
    labels "dflowfm_bmi_cache_test:integration_test"
    test_files bmi/test_dflowfm_bmi_cache.f90
    include_dir ${CMAKE_CURRENT_SOURCE_DIR}/../test_dflowfm/test_data/observations_snapped
)

set_tests_properties(
    dflowfm_bmi_cache_test
    PROPERTIES WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/test_data/observations_snapped"
)

