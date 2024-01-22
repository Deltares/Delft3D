# Tools for D-Waq
if(NOT TARGET waqpb_lib)
    add_subdirectory(${checkout_src_root}/${waqpb_lib_module} waqpb_lib)
endif()

if(NOT TARGET waqpb_import)
    add_subdirectory(${checkout_src_root}/${waqpb_import_module} waqpb_import)
endif()

if(NOT TARGET waqpb_export)
    add_subdirectory(${checkout_src_root}/${waqpb_export_module} waqpb_export)
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

if(NOT TARGET agrhyd)
    add_subdirectory(${checkout_src_root}/${agrhyd_module} agrhyd)
endif()

if(NOT TARGET calcage)
    add_subdirectory(${checkout_src_root}/${calcage_module} calcage)
endif()
