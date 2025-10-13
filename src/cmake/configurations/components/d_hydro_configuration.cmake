# D_HYDRO specific components
add_subdirectory(${checkout_src_root}/${d_hydro_module} d_hydro)

# Specify the modules to be included
if(NOT TARGET getopt)
  add_subdirectory(${checkout_src_root}/${getopt_module} getopt)
endif()

if(NOT TARGET deltares_common)
  add_subdirectory(${checkout_src_root}/${deltares_common_module} deltares_common)
endif()

if(NOT TARGET deltares_common_c)
  add_subdirectory(${checkout_src_root}/${deltares_common_c_module} deltares_common_c)
endif()

# D-Hydro lib
if(NOT TARGET d_hydro_lib)
    add_subdirectory(${checkout_src_root}/${d_hydro_lib_module} d_hydro_lib)
endif()

# Project name must be at the end of the configuration: it might get a name when including other configurations and needs to overwrite that
project(d_hydro)
