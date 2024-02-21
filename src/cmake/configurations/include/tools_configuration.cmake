project(tools)

# Specify the modules to be included

# Deltares_common
if(NOT TARGET deltares_common)
    add_subdirectory(${checkout_src_root}/${deltares_common_module} deltares_common)
endif()

if(NOT TARGET deltares_common_c)
    add_subdirectory(${checkout_src_root}/${deltares_common_c_module} deltares_common_c)
endif()

if(NOT TARGET deltares_common_mpi)
    add_subdirectory(${checkout_src_root}/${deltares_common_mpi_module} deltares_common_mpi)
endif()

# triangle
if(NOT TARGET triangle_c)
    add_subdirectory(${checkout_src_root}/${triangle_c_module} triangle_c)
endif()

# gridgeom
if(NOT TARGET gridgeom)
    add_subdirectory(${checkout_src_root}/${gridgeom_module} gridgeom)
endif()

if(NOT TARGET gridgeom_dll)
    add_subdirectory(${checkout_src_root}/${gridgeom_dll_module} gridgeom_dll)
endif()

# Third party libraries
# kdtree2
if(NOT TARGET kdtree2)
    add_subdirectory(${checkout_src_root}/${kdtree_module} kdtree2)
endif()

if(NOT TARGET kdtree_wrapper)
    add_subdirectory(${checkout_src_root}/${kdtree_wrapper_module} kdtree_wrapper)
endif()

# Tools_gpl
# Mormerge
if(NOT TARGET mormerge)
    add_subdirectory(${checkout_src_root}/${mormerge_module} mormerge)
endif()

# dfmoutput
if(NOT (TARGET dfmoutput OR NO_FM_TOOLS))
    add_subdirectory(${checkout_src_root}/${dfmoutput_module} dfmoutput)
endif()

# dfm_volume_tool
if(NOT (TARGET dfm_volume_tool OR NO_FM_TOOLS))
    add_subdirectory(${checkout_src_root}/${dfm_volume_tool_module} dfm_volume_tool)
endif()

# dfm_api_access
if(NOT (TARGET dfm_api_access OR NO_FM_TOOLS))
    add_subdirectory(${checkout_src_root}/${dfm_api_access_module} dfm_api_access)
endif()

# cosumo_bmi
if(NOT TARGET cosumo_bmi)
    add_subdirectory(${checkout_src_root}/${cosumo_bmi_module} cosumo_bmi)
endif()

# Third party
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

# proj
if(WIN32)
    if(NOT TARGET proj)
        include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/include/proj_configuration.cmake)
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

if(NOT TARGET io_netcdf_data)
    add_subdirectory(${checkout_src_root}/${io_netcdf_data_module} io_netcdf_data)
endif()

# Nefis
if(NOT TARGET nefis)
    add_subdirectory(${checkout_src_root}/${nefis_module} nefis)
endif()

# delftio
if(NOT TARGET delftio_shm)
    add_subdirectory(${checkout_src_root}/${delftio_shm_module} delftio_shm)
endif()

if(NOT TARGET delftio)
    add_subdirectory(${checkout_src_root}/${delftio_module} delftio)
endif()

# emfsm
if(NOT TARGET esmfsm_version_number)
    add_subdirectory(${checkout_src_root}/${esmfsm_version_number_module} esmfsm_version_number)
endif()

if(NOT TARGET esmfsm_c)
    add_subdirectory(${checkout_src_root}/${esmfsm_c_module} esmfsm_c)
endif()

if(NOT TARGET esmfsm)
    add_subdirectory(${checkout_src_root}/${esmfsm_module} esmfsm)
endif()

# ec_module
if(NOT TARGET ec_module)
    add_subdirectory(${checkout_src_root}/${ec_module} ec_module)
endif()