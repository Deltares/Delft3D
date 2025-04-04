# Wave modules
# ============
add_subdirectory(${checkout_src_root}/${wave_data_module} wave_data)
add_subdirectory(${checkout_src_root}/${wave_io_module} wave_io)
add_subdirectory(${checkout_src_root}/${wave_kernel_module} wave_kernel)
add_subdirectory(${checkout_src_root}/${wave_manager_module} wave_manager)
add_subdirectory(${checkout_src_root}/${wave_module} wave)



# Utils
# =====

# Deltares common
if(NOT TARGET deltares_common)
    add_subdirectory(${checkout_src_root}/${deltares_common_module} deltares_common)
endif()
if(NOT TARGET deltares_common_c)
    add_subdirectory(${checkout_src_root}/${deltares_common_c_module} deltares_common_c)
endif()
if(NOT TARGET deltares_common_mpi)
    add_subdirectory(${checkout_src_root}/${deltares_common_mpi_module} deltares_common_mpi)
endif()

# delftio
if(NOT TARGET delftio_shm)
    add_subdirectory(${checkout_src_root}/${delftio_shm_module} delftio_shm)
endif()
if(NOT TARGET delftio)
    add_subdirectory(${checkout_src_root}/${delftio_module} delftio)
endif()

# io_netcdf
if(NOT TARGET io_netcdf)
    add_subdirectory(${checkout_src_root}/${io_netcdf_module} io_netcdf)
endif()

if(NOT TARGET io_netcdf_data)
    add_subdirectory(${checkout_src_root}/${io_netcdf_data_module} io_netcdf_data)
endif()

# ec_module
if(NOT TARGET ec_module)
    add_subdirectory(${checkout_src_root}/${ec_module} ec_module)
endif()

# gridgeom
if(NOT TARGET gridgeom)
    add_subdirectory(${checkout_src_root}/${gridgeom_module} gridgeom)
endif()

# Nefis
if(NOT TARGET nefis)
    add_subdirectory(${checkout_src_root}/${nefis_module} nefis)
endif()


# Third party
# ===========

# fortrangis
if(NOT TARGET fortrangis)
    add_subdirectory(${checkout_src_root}/${fortrangis_module} fortrangis)
endif()

# triangle
if(NOT TARGET triangle_c)
    add_subdirectory(${checkout_src_root}/${triangle_c_module} triangle_c)
endif()

# netcdf
if(WIN32)
    if(NOT TARGET netcdff)
        add_subdirectory(${checkout_src_root}/${netcdf_module} netcdff)
    endif()
endif()

# kdtree2
if(NOT TARGET kdtree2)
    add_subdirectory(${checkout_src_root}/${kdtree_module} kdtree2)
endif()

# kdtree_wrapper
if(NOT TARGET kdtree_wrapper)
    add_subdirectory(${checkout_src_root}/${kdtree_wrapper_module} kdtree_wrapper)
endif()

# shp
if(NOT TARGET shp)
    add_subdirectory(${checkout_src_root}/${shp_module} shp)
endif()

# proj
if(WIN32)
    if(NOT TARGET proj)
        include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/miscellaneous/proj_configuration.cmake)
    endif()
endif(WIN32)

# Swan
if(NOT TARGET swan)
    add_subdirectory(${checkout_src_root}/${swan_mpi_lib_module} swan_mpi_lib)
    add_subdirectory(${checkout_src_root}/${swan_mpi_module} swan_mpi)
    add_subdirectory(${checkout_src_root}/${swan_omp_module} swan_omp)
endif()

if(UNIX)
    # install
    add_subdirectory(${checkout_src_root}/${install_wave_module} install_wave)
endif()

# Project name must be at the end of the configuration: it might get a name when including other configurations and needs to overwrite that
project(dwaves)
