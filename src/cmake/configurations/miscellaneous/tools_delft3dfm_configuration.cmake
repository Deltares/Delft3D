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

# cosumo_bmi
if(NOT TARGET cosumo_bmi)
    add_subdirectory(${checkout_src_root}/${cosumo_bmi_module} cosumo_bmi)
endif()

# csumo_nfio
if(NOT TARGET csumo_nfio)
    add_subdirectory(${checkout_src_root}/${csumo_nfio_module} csumo_nfio)
endif()

# csumo_precice
if(NOT TARGET csumo_precice)
    add_subdirectory(${checkout_src_root}/${csumo_precice_module} csumo_precice)
endif()

# D-Waq tools
include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/components/dwaq/dwaq_tools.cmake)