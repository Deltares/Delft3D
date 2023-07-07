#
# WAQ
#=============
if(NOT TARGET waq_plugin_wasteload)
    add_subdirectory(${checkout_src_root}/${waq_plugin_wasteload_module} waq_plugin_wasteload)
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

if(NOT TARGET waq_kernel)
    add_subdirectory(${checkout_src_root}/${waq_kernel_module} waq_kernel)
endif()

if(NOT TARGET waq_io)
    add_subdirectory(${checkout_src_root}/${waq_io_module} waq_io)
endif()

if(NOT TARGET delwaq_lib)
    add_subdirectory(${checkout_src_root}/${delwaq_lib_module} delwaq_lib)
endif()

if(NOT TARGET waq_data)
    add_subdirectory(${checkout_src_root}/${waq_data_module} waq_data)
endif()

if(NOT TARGET delwaq1)
    add_subdirectory(${checkout_src_root}/${delwaq1_module} delwaq1)
endif()

if(NOT TARGET delwaq2)
    add_subdirectory(${checkout_src_root}/${delwaq2_module} delwaq2)
endif()

if(NOT TARGET delwaq_lib_examples)
    add_subdirectory(${checkout_src_root}/${delwaq_lib_examples_module} delwaq_lib_examples)
endif()

if(NOT TARGET waq_delftio)
    add_subdirectory(${checkout_src_root}/${waq_delftio_module} waq_delftio)
endif()

if(NOT TARGET wq_processes)
    add_subdirectory(${checkout_src_root}/${wq_processes_module} wq_processes)
endif()

#
# WAQ Tools
#=============
# Waqpb
if(NOT TARGET waqpb_export)
    add_subdirectory(${checkout_src_root}/${waqpb_export_module} waqpb_export)
endif()
if(NOT TARGET waqpb_import)
    add_subdirectory(${checkout_src_root}/${waqpb_import_module} waqpb_import)
endif()
if(NOT TARGET waqpb_lib)
    add_subdirectory(${checkout_src_root}/${waqpb_lib_module} waqpb_lib)
endif()
if(NOT TARGET waqmerge)
    add_subdirectory(${checkout_src_root}/${waqmerge_module} waqmerge)
endif()
if(NOT TARGET ddcouple)
    add_subdirectory(${checkout_src_root}/${ddcouple_module} ddcouple)
endif()
if(NOT TARGET agrhyd)
    add_subdirectory(${checkout_src_root}/${agrhyd_module} agrhyd)
endif()
if(NOT TARGET maptonetcdf)
    add_subdirectory(${checkout_src_root}/${maptonetcdf_module} maptonetcdf)
endif()



#
# PART
#=============
if(NOT TARGET part_data_f)
    add_subdirectory(${checkout_src_root}/${part_data_f_module} part_data_f)
endif()

if(NOT TARGET part_utils_f)
    add_subdirectory(${checkout_src_root}/${part_utils_f_module} part_utils_f)
endif()

if(NOT TARGET part_io_f)
    add_subdirectory(${checkout_src_root}/${part_io_f_module} part_io_f)
endif()

if(NOT TARGET part_kernel_f)
    add_subdirectory(${checkout_src_root}/${part_kernel_f_module} part_kernel_f)
endif()

if(NOT TARGET delpar)
    add_subdirectory(${checkout_src_root}/${delpar_module} delpar)
endif()

#
# Third party libraries
#=============
# kdtree2
if(NOT TARGET kdtree2)
    add_subdirectory(${checkout_src_root}/${kdtree_module} kdtree2)
endif()

if(NOT TARGET kdtree_wrapper)
    add_subdirectory(${checkout_src_root}/${kdtree_wrapper_module} kdtree_wrapper)
endif()

# triangle
if(NOT TARGET triangle_c)
    add_subdirectory(${checkout_src_root}/${triangle_c_module} triangle_c)
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

#
# Utils
#=============
# Deltares_common
if(NOT TARGET deltares_common)
    add_subdirectory(${checkout_src_root}/${deltares_common_module} deltares_common)
endif()
if(NOT TARGET deltares_common_c)
    add_subdirectory(${checkout_src_root}/${deltares_common_c_module} deltares_common_c)
endif()

# netcdf
if(WIN32)
    if(NOT TARGET netcdff)
        add_subdirectory(${checkout_src_root}/${netcdf_module} netcdff)
    endif()
endif()

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

# Nefis
if(NOT TARGET nefis)
    add_subdirectory(${checkout_src_root}/${nefis_module} nefis)
endif()

# Solvesaphe
if(NOT TARGET solvesaphe)
    add_subdirectory(${checkout_src_root}/${solvesaphe_module} solvesaphe)
endif()

# io_hyd
if(NOT TARGET io_hyd)
    add_subdirectory(${checkout_src_root}/${io_hyd_module} io_hyd)
endif()

#
# Linux installation
#=============
if(UNIX)
    add_subdirectory(${checkout_src_root}/${install_waq_module} install_waq)
endif()

# Project name must be at the end of the configuration: it might get a name when including other configurations and needs to overwrite that
project(dwaq)
