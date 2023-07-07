# Specify the modules to be included
if(NOT TARGET deltares_common)
    add_subdirectory(${checkout_src_root}/${deltares_common_module} deltares_common)
endif()

if(NOT TARGET deltares_common_c)
    add_subdirectory(${checkout_src_root}/${deltares_common_c_module} deltares_common_c)
endif()

if(NOT TARGET deltares_common_mpi)
    add_subdirectory(${checkout_src_root}/${deltares_common_mpi_module} deltares_common_mpi)
endif()

if(NOT TARGET ftnunit)
    add_subdirectory(${checkout_src_root}/${ftnunit_module} ftnunit)
endif()

# Trachytopes
if(NOT TARGET trachytopes_kernel)
    add_subdirectory(${checkout_src_root}/${trachytopes_kernel_module} trachytopes_kernel)
endif()

if(NOT TARGET trachytopes_io)
    add_subdirectory(${checkout_src_root}/${trachytopes_io_module} trachytopes_io)
endif()

# Flow1d
if(NOT TARGET flow1d_core)
    add_subdirectory(${checkout_src_root}/${flow1d_core_module} flow1d_core)
endif()

if(NOT TARGET flow1d_io)
    add_subdirectory(${checkout_src_root}/${flow1d_io_module} flow1d_io)
endif()

if(NOT TARGET flow1d)
    add_subdirectory(${checkout_src_root}/${flow1d_module} flow1d)
endif()

# Waq
if(NOT TARGET waq_data)
    add_subdirectory(${checkout_src_root}/${waq_data_module} waq_data)
endif()

if(NOT TARGET waq_utils_c)
    add_subdirectory(${checkout_src_root}/${waq_utils_c_module} waq_utils_c)
endif()

if(NOT TARGET waq_utils_f)
    add_subdirectory(${checkout_src_root}/${waq_utils_f_module} waq_utils_f)
endif()

if(NOT TARGET waq_process)
    add_subdirectory(${checkout_src_root}/${waq_process_module} waq_process)
endif()

if(NOT TARGET wq_processes)
    add_subdirectory(${checkout_src_root}/${wq_processes_module} wq_processes)
endif()

# Morphology
if(NOT TARGET morphology_plugins_c)
    add_subdirectory(${checkout_src_root}/${morphology_plugins_c_module} morphology_plugins_c)
endif()

if(NOT TARGET morphology_data)
    add_subdirectory(${checkout_src_root}/${morphology_data_module} morphology_data)
endif()

if(NOT TARGET morphology_kernel)
    add_subdirectory(${checkout_src_root}/${morphology_kernel_module} morphology_kernel)
endif()

if(NOT TARGET morphology_io)
    add_subdirectory(${checkout_src_root}/${morphology_io_module} morphology_io)
endif()

# Hydrology
if(NOT TARGET dhydrology_kernel)
    add_subdirectory(${checkout_src_root}/${hydrology_kernel_module} dhydrology_kernel)
endif()

# Dflowfm modules 
if(NOT TARGET dflowfm_kernel)
    add_subdirectory(${checkout_src_root}/${dflowfm_kernel_module} dflowfm_kernel)
endif()

# Third party libraries
# kdtree2
if(NOT TARGET kdtree2)
    add_subdirectory(${checkout_src_root}/${kdtree_module} kdtree2)
endif()

if(NOT TARGET kdtree_wrapper)
    add_subdirectory(${checkout_src_root}/${kdtree_wrapper_module} kdtree_wrapper)
endif()

# md5
if(NOT TARGET md5)
    add_subdirectory(${checkout_src_root}/${md5_module} md5)
endif()

# metis
if(WIN32)
    if(NOT TARGET metis)
        add_subdirectory(${checkout_src_root}/${metis_module} metis)
    endif()
endif(WIN32)

if(NOT TARGET metisoptions)
    add_subdirectory(${checkout_src_root}/${metisoptions_module} metisoptions) # Note that the metisoptions should be loaded AFTER metis is loaded, as it depends on settings set by the CMakeLists.txt of the metis library
endif()

# petsc
if(WIN32)
    if(NOT TARGET petsc)
        add_subdirectory(${checkout_src_root}/${petsc_module} petsc)
    endif()
endif(WIN32)

# triangle
if(NOT TARGET triangle_c)
    add_subdirectory(${checkout_src_root}/${triangle_c_module} triangle_c)
endif()

# libsigwatch
if(NOT TARGET libsigwatch)
    add_subdirectory(${checkout_src_root}/${libsigwatch_module} libsigwatch)
endif()

# FLAP
if(NOT TARGET FLAP)
    add_subdirectory(${checkout_src_root}/${FLAP_module} FLAP)
endif()

# fortrangis
if(NOT TARGET fortrangis)
    add_subdirectory(${checkout_src_root}/${fortrangis_module} fortrangis)
endif()

if(NOT TARGET shp)
    add_subdirectory(${checkout_src_root}/${shp_module} shp)
endif()

if(WIN32)
    if(NOT TARGET proj)
        add_subdirectory(${checkout_src_root}/${proj_module} proj)
    endif()
endif(WIN32)

# netcdf
if(WIN32)
    if(NOT TARGET netcdff)
        add_subdirectory(${checkout_src_root}/${netcdf_module} netcdff)
    endif()
endif(WIN32)

# io_netcdf
if(NOT TARGET io_netcdf)
    add_subdirectory(${checkout_src_root}/${io_netcdf_module} io_netcdf)
endif()

# ec_module
if(NOT TARGET ec_module)
    add_subdirectory(${checkout_src_root}/${ec_module} ec_module)
endif()

# gridgeom
if(NOT TARGET gridgeom)
    add_subdirectory(${checkout_src_root}/${gridgeom_module} gridgeom)
endif()

# interacter_stub
if(NOT TARGET interacter_stub)
    add_subdirectory(${checkout_src_root}/${interacter_stub_module} interacter_stub)
endif()

# polypack
if(NOT TARGET polypack)
    add_subdirectory(${checkout_src_root}/${polypack_module} polypack)
endif()

# Nefis
if(NOT TARGET nefis)
    add_subdirectory(${checkout_src_root}/${nefis_module} nefis)
endif()

# Solvesaphe
if(NOT TARGET solvesaphe)
    add_subdirectory(${checkout_src_root}/${solvesaphe_module} solvesaphe)
endif()

# Test binaries
add_subdirectory(${checkout_src_root}/${test_deltares_common_module} test_deltares_common)
add_subdirectory(${checkout_src_root}/${test_ec_module}              test_ec_module)
add_subdirectory(${checkout_src_root}/${test_waq_utils_f}            test_waq_utils_f)
add_subdirectory(${checkout_src_root}/${test_dflowfm_kernel}         test_dflowfm_kernel)

if(UNIX)
    # install
    add_subdirectory(${checkout_src_root}/${install_tests_module} install_tests)
endif(UNIX)

# Project name must be at the end of the configuration: it might get a name when including other configurations and needs to overwrite that
project(tests)
