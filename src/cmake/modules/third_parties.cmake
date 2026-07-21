# File to define the third party components and their corresponding tests
set(third_party_path third_party)
set(third_party_open_path third_party_open)
set(cmake_directory cmake_deltares)

# kdtree2
set(kdtree_module ${third_party_open_path}/kdtree2/${cmake_directory})

# md5
set(md5_module ${third_party_open_path}/md5/${cmake_directory})

# metis
set(metis_module ${third_party_open_path}/metis/${cmake_directory})

# petsc
if(WIN32)
    set(petsc_module ${third_party_open_path}/petsc/${cmake_directory})
endif(WIN32)

# precice
set(precice_module ${third_party_open_path}/precice)

# triangle_c
set(triangle_c_module ${third_party_open_path}/triangle/${cmake_directory})

# libsigwatch
set(libsigwatch_module ${third_party_open_path}/libsigwatch/${cmake_directory})

# FLAP
set(FLAP_module ${third_party_open_path}/FLAP/${cmake_directory})

# fortrangis
set(fortrangis_module ${third_party_open_path}/fortrangis/${cmake_directory})
set(shp_module ${third_party_open_path}/shapelib/${cmake_directory})

# polypack
set(polypack_module ${third_party_open_path}/polypack)

# interacter_stub
set(interacter_stub_module ${third_party_path}/interacter_stub)

# swan
set(swan_mpi_lib_module ${third_party_open_path}/swan)
set(swan_mpi_module ${third_party_open_path}/swan/swan_mpi)
set(swan_omp_module ${third_party_open_path}/swan/swan_omp)

# solvesaphe
set(solvesaphe_module ${third_party_open_path}/solveSAPHE/${cmake_directory})

# spherepack
set(spherepack_module ${third_party_open_path}/spherepack/${cmake_directory})

#intel redist
set(intelredist_module ${third_party_open_path}/intelredist)

set(microsoftredist_module ${third_party_open_path}/microsoftredist)

# getopt
set(getopt_module ${third_party_open_path}/getopt/${cmake_directory})

if(WIN32)
    set(pthreads_module ${third_party_open_path}/pthreads)
endif(WIN32)

# precicef (preCICE fortran bindings)
set(precicef_module "${third_party_open_path}/precicef")

# pugixml
set(pugixml_module ${third_party_open_path}/pugixml)
