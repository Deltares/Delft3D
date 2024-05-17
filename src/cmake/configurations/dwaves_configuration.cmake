# Wave modules
# ============
add_subdirectory(${CMAKE_SOURCE_DIR}/src/${wave_data_module} wave_data)
add_subdirectory(${CMAKE_SOURCE_DIR}/src/${wave_io_module} wave_io)
add_subdirectory(${CMAKE_SOURCE_DIR}/src/${wave_kernel_module} wave_kernel)
add_subdirectory(${CMAKE_SOURCE_DIR}/src/${wave_manager_module} wave_manager)
add_subdirectory(${CMAKE_SOURCE_DIR}/src/${wave_module} wave)



# Utils
# =====

# Deltares common
if(NOT TARGET deltares_common)
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/${deltares_common_module} deltares_common)
endif()
if(NOT TARGET deltares_common_c)
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/${deltares_common_c_module} deltares_common_c)
endif()
if(NOT TARGET deltares_common_mpi)
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/${deltares_common_mpi_module} deltares_common_mpi)
endif()

# delftio
if(NOT TARGET delftio_shm)
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/${delftio_shm_module} delftio_shm)
endif()
if(NOT TARGET delftio)
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/${delftio_module} delftio)
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

# Nefis
if(NOT TARGET nefis)
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/${nefis_module} nefis)
endif()

# esmfsm
if(NOT TARGET esmfsm_version_number)
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/${esmfsm_version_number_module} esmfsm_version_number)
endif()
if(NOT TARGET esmfsm_c)
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/${esmfsm_c_module} esmfsm_c)
endif()
if(NOT TARGET esmfsm)
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/${esmfsm_module} esmfsm)
endif()


# Third party
# ===========

# fortrangis
if(NOT TARGET fortrangis)
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/${fortrangis_module} fortrangis)
endif()

# triangle
if(NOT TARGET triangle_c)
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/${triangle_c_module} triangle_c)
endif()

# netcdf
if(WIN32)
    if(NOT TARGET netcdff)
        add_subdirectory(${CMAKE_SOURCE_DIR}/src/${netcdf_module} netcdff)
    endif()
endif()

# proj
if(WIN32)
    if(NOT TARGET proj)
        include(${CMAKE_SOURCE_DIR}/src/cmake/configurations/include/proj_configuration.cmake)
    endif()
endif(WIN32)

# kdtree2
if(NOT TARGET kdtree2)
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/${kdtree_module} kdtree2)
endif()

if(NOT TARGET kdtree_wrapper)
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/${kdtree_wrapper_module} kdtree_wrapper)
endif()

if(NOT TARGET shp)
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/${shp_module} shp)
endif()

# Swan
if(NOT TARGET swan)
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/${swan_mpi_lib_module} swan_mpi_lib)
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/${swan_mpi_module} swan_mpi)
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/${swan_omp_module} swan_omp)
endif()



if(UNIX)
    # install
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/${install_wave_module} install_wave)
endif()

# Project name must be at the end of the configuration: it might get a name when including other configurations and needs to overwrite that
project(dwaves)
