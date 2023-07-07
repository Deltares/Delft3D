# Tools for Delft3D-FM

# Tools_gpl

# Mormerge
if(NOT TARGET mormerge)
    add_subdirectory(${checkout_src_root}/${mormerge_module} mormerge)
endif()

# dfmoutput
if(NOT TARGET dfmoutput)
    add_subdirectory(${checkout_src_root}/${dfmoutput_module} dfmoutput)
endif()

# dfm_volume_tool
if(NOT TARGET dfm_volume_tool)
    add_subdirectory(${checkout_src_root}/${dfm_volume_tool_module} dfm_volume_tool)
endif()

# dfm_api_access
if(NOT TARGET dfm_api_access)
    add_subdirectory(${checkout_src_root}/${dfm_api_access_module} dfm_api_access)
endif()

# Waqpb
if(NOT TARGET waqpb_lib)
    add_subdirectory(${checkout_src_root}/${waqpb_lib_module} waqpb_lib)
endif()

if(NOT TARGET waqpb_import)
    add_subdirectory(${checkout_src_root}/${waqpb_import_module} waqpb_import)
endif()

if(NOT TARGET waqpb_export)
    add_subdirectory(${checkout_src_root}/${waqpb_export_module} waqpb_export)
endif()

# Waqmerge
if(NOT TARGET waqmerge)
    add_subdirectory(${checkout_src_root}/${waqmerge_module} waqmerge)
endif()

# Ddcouple
if(NOT TARGET ddcouple)
    add_subdirectory(${checkout_src_root}/${ddcouple_module} ddcouple)
endif()

# Agrhyd
if(NOT TARGET agrhyd)
    add_subdirectory(${checkout_src_root}/${agrhyd_module} agrhyd)
endif()

# Maptonetcdf
if(NOT TARGET maptonetcdf)
    add_subdirectory(${checkout_src_root}/${maptonetcdf_module} maptonetcdf)
endif()

# cosumo_bmi
if(NOT TARGET cosumo_bmi)
    add_subdirectory(${checkout_src_root}/${cosumo_bmi_module} cosumo_bmi)
endif()
