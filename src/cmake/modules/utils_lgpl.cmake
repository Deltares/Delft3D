# File to define the utils_lgpl components and their corresponding tests
set(utils_lgpl_path utils_lgpl)

# deltares_common modules
set(utils_lgpl_deltares_common_path ${utils_lgpl_path}/deltares_common/packages)
set(deltares_common_module ${utils_lgpl_deltares_common_path}/deltares_common)
set(deltares_common_c_module ${utils_lgpl_deltares_common_path}/deltares_common_c)
set(deltares_common_mpi_module ${utils_lgpl_deltares_common_path}/deltares_common_mpi)

#delftio
set(utils_lgpl_delftio_path ${utils_lgpl_path}/delftio/packages)
set(delftio_module ${utils_lgpl_delftio_path}/delftio)
set(delftio_shm_module ${utils_lgpl_delftio_path}/delftio_shm)

# ftnunit
set(ftnunit_module ${utils_lgpl_path}/ftnunit/packages/ftnunit)

# metisoptions
set(metisoptions_module ${utils_lgpl_path}/metistools/packages/metisoptions)

# kdtree2
set(kdtree_wrapper_module ${utils_lgpl_path}/kdtree_wrapper/packages/kdtree_wrapper)

# io_netcdf
set(io_netcdf_module ${utils_lgpl_path}/io_netcdf/packages/io_netcdf)
set(io_netcdf_data_module ${utils_lgpl_path}/io_netcdf/packages/io_netcdf_data)

# ec_module
set(ec_module ${utils_lgpl_path}/ec_module/packages/ec_module)

# esmfsm
set(utils_lgpl_esmfsm_path ${utils_lgpl_path}/esmfsm/packages)
set(esmfsm_version_number_module ${utils_lgpl_esmfsm_path}/esmfsm_version_number)
set(esmfsm_c_module ${utils_lgpl_esmfsm_path}/esmfsm_c)
set(esmfsm_module ${utils_lgpl_esmfsm_path}/esmfsm)

# gridgeom
set(gridgeom_module ${utils_lgpl_path}/gridgeom/packages/gridgeom)
set(gridgeom_dll_module ${utils_lgpl_path}/gridgeom/packages/gridgeom_dll)

# Nefis
set(nefis_module ${utils_lgpl_path}/nefis/packages/nefis)

# io_hyd
set(io_hyd_module ${utils_lgpl_path}/io_hyd/packages/io_hyd)

# io_hyd
set(waq_hyd_data_module ${utils_lgpl_path}/waq_hyd_data)

# D_Hydro_lib
set(d_hydro_lib_module ${utils_lgpl_path}/d_hydro_lib/packages/d_hydro_lib)

# Tests
set(test_deltares_common_module test/utils_lgpl/deltares_common/packages/test_deltares_common)
set(test_ec_module              test/utils_lgpl/ec_module/packages/ec_module_test)
set(test_io_netcdf              test/utils_lgpl/io_netcdf/packages/test_io_netcdf)
set(test_waq_utils_f            test/engines_gpl/waq/waq_utils_f)
set(test_dflowfm_kernel         test/engines_gpl/dflowfm/packages/dflowfm_kernel)

# Unit tests
# utils_lgpl
set(utils_lgpl_tests_module ${unit_tests_dir}/utils_lgpl)
