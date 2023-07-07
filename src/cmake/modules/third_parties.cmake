# File to define the third party components and their corresponding tests
set(third_party_path third_party)
set(third_party_open_path third_party_open)
set(cmake_directory cmake_deltares)

# kdtree2
set(kdtree_module ${third_party_open_path}/kdtree2/${cmake_directory})

# md5
set(md5_module ${third_party_open_path}/md5/${cmake_directory})

# metis
if(WIN32)
    set(metis_module ${third_party_open_path}/metis/${cmake_directory})
endif(WIN32)

# petsc
if(WIN32)
    set(petsc_module ${third_party_open_path}/petsc/${cmake_directory})
endif(WIN32)

# triangle_c
set(triangle_c_module ${third_party_open_path}/triangle/${cmake_directory})

# libsigwatch
set(libsigwatch_module ${third_party_open_path}/libsigwatch/${cmake_directory})

# FLAP
set(FLAP_module ${third_party_open_path}/FLAP/${cmake_directory})

# fortrangis
set(fortrangis_module ${third_party_open_path}/fortrangis/${cmake_directory})
set(shp_module ${third_party_open_path}/shapelib/${cmake_directory})
if(WIN32)
    set(proj_module ${third_party_open_path}/proj4/${cmake_directory})
endif(WIN32)

# netcdf
if(WIN32)
    set(netcdf_module ${third_party_open_path}/netcdf/${cmake_directory})
endif(WIN32)
set(netcdf_version "netCDF 4.6.1")


# polypack
set(polypack_module ${third_party_open_path}/polypack/${cmake_directory})

# interacter_stub
set(interacter_stub_module ${third_party_path}/interacter_stub)

# expat
set(expat_module ${third_party_open_path}/expat/${cmake_directory})

# swan
set(swan_mpi_lib_module ${third_party_open_path}/swan)
set(swan_mpi_module ${third_party_open_path}/swan/swan_mpi)
set(swan_omp_module ${third_party_open_path}/swan/swan_omp)

# solvesaphe
set(solvesaphe_module ${third_party_open_path}/solveSAPHE/${cmake_directory})
