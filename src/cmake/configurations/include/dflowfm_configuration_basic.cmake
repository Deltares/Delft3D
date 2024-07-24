# Specify the modules to be included
if(NOT TARGET deltares_common)
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/${deltares_common_module} deltares_common)
endif()

if(NOT TARGET deltares_common_c)
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/${deltares_common_c_module} deltares_common_c)
endif()

if(NOT TARGET deltares_common_mpi)
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/${deltares_common_mpi_module} deltares_common_mpi)
endif()

# Ice
if(NOT TARGET ice_data)
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/${ice_data_module} ice_data)
endif()

if(NOT TARGET ice_io)
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/${ice_io_module} ice_io)
endif()

# Trachytopes
if(NOT TARGET trachytopes_kernel)
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/${trachytopes_kernel_module} trachytopes_kernel)
endif()

if(NOT TARGET trachytopes_io)
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/${trachytopes_io_module} trachytopes_io)
endif()

# Flow1d
if(NOT TARGET flow1d_core)
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/${flow1d_core_module} flow1d_core)
endif()

if(NOT TARGET flow1d_io)
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/${flow1d_io_module} flow1d_io)
endif()

if(NOT TARGET flow1d)
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/${flow1d_module} flow1d)
endif()

if(NOT TARGET flow1d_implicit)
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/${flow1d_implicit} flow1d_implicit)
endif()

# Waq
include(${CMAKE_SOURCE_DIR}/src/cmake/configurations/include/dwaq/dwaq_base.cmake)
include(${CMAKE_SOURCE_DIR}/src/cmake/configurations/include/dwaq/dwaq_dflowfm_online_coupling.cmake)




# Morphology
if(NOT TARGET morphology_plugins_c)
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/${morphology_plugins_c_module} morphology_plugins_c)
endif()

if(NOT TARGET morphology_data)
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/${morphology_data_module} morphology_data)
endif()

if(NOT TARGET morphology_kernel)
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/${morphology_kernel_module} morphology_kernel)
endif()

if(NOT TARGET morphology_io)
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/${morphology_io_module} morphology_io)
endif()

# Hydrology
if(NOT TARGET dhydrology_kernel)
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/${hydrology_kernel_module} dhydrology_kernel)
endif()

# Dflowfm modules
add_subdirectory(${CMAKE_SOURCE_DIR}/src/${dflowfm_kernel_module} dflowfm_kernel)
add_subdirectory(${CMAKE_SOURCE_DIR}/src/${dflowfm_cli_exe_module} dflowfm_cli_exe)
# dflowfm_lib: only when without interacter
if(NOT WITH_INTERACTER)
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/${dflowfm_lib_module} dflowfm_lib)
endif()

# Tools_gpl
# DFMoutput
if(NOT TARGET dfmoutput)
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/${dfmoutput_module} dfmoutput)
endif()

# DFM_volume_tool
if(NOT TARGET dfm_volume_tool)
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/${dfm_volume_tool_module} dfm_volume_tool)
endif()

# DFM_api_access
if(NOT TARGET dfm_api_access)
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/${dfm_api_access_module} dfm_api_access)
endif()



# Third party libraries
# kdtree2
if(NOT TARGET kdtree2)
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/${kdtree_module} kdtree2)
endif()

if(NOT TARGET kdtree_wrapper)
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/${kdtree_wrapper_module} kdtree_wrapper)
endif()

# md5
if(NOT TARGET md5)
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/${md5_module} md5)
endif()

# metis

if(NOT TARGET metis)
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/${metis_module} metis)
endif()

if(NOT TARGET metisoptions)
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/${metisoptions_module} metisoptions) # Note that the metisoptions should be loaded AFTER metis is loaded, as it depends on settings set by the CMakeLists.txt of the metis library
endif()

# petsc
if(WIN32)
    if(NOT TARGET petsc)
        add_subdirectory(${CMAKE_SOURCE_DIR}/src/${petsc_module} petsc)
    endif()
endif(WIN32)

# triangle
if(NOT TARGET triangle_c)
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/${triangle_c_module} triangle_c)
endif()

# libsigwatch
if(NOT TARGET libsigwatch)
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/${libsigwatch_module} libsigwatch)
endif()

# FLAP
if(NOT TARGET FLAP)
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/${FLAP_module} FLAP)
endif()

# fortrangis
if(NOT TARGET fortrangis)
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/${fortrangis_module} fortrangis)
endif()

if(NOT TARGET shp)
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/${shp_module} shp)
endif()

# proj
if(WIN32)
    if(NOT TARGET proj)
        include(${CMAKE_SOURCE_DIR}/src/cmake/configurations/include/proj_configuration.cmake)
    endif()
endif(WIN32)

# netcdf
if(NOT TARGET netcdff)
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/${netcdf_module} netcdff)
endif()

# io_netcdf
if(NOT TARGET io_netcdf)
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/${io_netcdf_module} io_netcdf)
endif()

if(NOT TARGET io_netcdf_data)
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/${io_netcdf_data_module} io_netcdf_data)
endif()

# ec_module
if(NOT TARGET ec_module)
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/${ec_module} ec_module)
endif()

# gridgeom
if(NOT TARGET gridgeom)
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/${gridgeom_module} gridgeom)
endif()

# icepack
if(NOT TARGET icepack)
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/${icepack_module} icepack)
endif()

if(NOT WITH_INTERACTER)
    # Use interacter_stub instead
    if(NOT TARGET interacter_stub)
        add_subdirectory(${CMAKE_SOURCE_DIR}/src/${interacter_stub_module} interacter_stub)
    endif()
endif()

# polypack
if(NOT TARGET polypack)
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/${polypack_module} polypack)
endif()

# Nefis
if(NOT TARGET nefis)
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/${nefis_module} nefis)
endif()


# Unit tests for dflowfm and io_netcdf
# Only for the version without interacter
if(NOT WITH_INTERACTER)
    if(NOT TARGET ftnunit)
        add_subdirectory(${CMAKE_SOURCE_DIR}/src/${ftnunit_module} ftnunit)
    endif()

    if(NOT TARGET test_dflowfm_kernel)
        add_subdirectory(${CMAKE_SOURCE_DIR}/src/${test_dflowfm_kernel} test_dflowfm_kernel)
    endif()

    if(NOT TARGET test_deltares_common)
        add_subdirectory(${CMAKE_SOURCE_DIR}/src/${test_deltares_common_module} test_deltares_common)
    endif()

    if(NOT TARGET test_ec_module)
        add_subdirectory(${CMAKE_SOURCE_DIR}/src/${test_ec_module} test_ec_module)
    endif()

    if (NOT TARGET test_io_netcdf)
        add_subdirectory(${CMAKE_SOURCE_DIR}/src/${test_io_netcdf} test_io_netcdf)
    endif()
endif(NOT WITH_INTERACTER)


if(UNIX)
    # install
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/${install_dflowfm_module} install_dflowfm)
endif()

# Plugins
if(NOT TARGET plugin_culvert)
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/plugins_lgpl/plugin_culvert plugin_culvert)
endif()
if(NOT TARGET plugin_delftflow_traform)
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/plugins_lgpl/plugin_delftflow_traform plugin_delftflow_traform)
endif()
