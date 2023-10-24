# Specify the modules to be included
if(NOT TARGET deltares_common)
  add_subdirectory(${checkout_src_root}/${deltares_common_module} deltares_common)
endif()

if(NOT TARGET deltares_common_c)
  add_subdirectory(${checkout_src_root}/${deltares_common_c_module} deltares_common_c)
endif()

# for unix, we need to build expat xml parser
if(UNIX)
  if(NOT TARGET expat)
    add_subdirectory(${checkout_src_root}/${expat_module} expat)
  endif()
endif(UNIX)

# DIMR specific components
add_subdirectory(${checkout_src_root}/${dimr_lib_module} dimr_lib)
add_subdirectory(${checkout_src_root}/${dimr_module} dimr)

if(UNIX)
  # install
  add_subdirectory(${checkout_src_root}/${install_dimr_module} install_dimr)
endif()

# Project name must be at the end of the configuration: it might get a name when including other configurations and needs to overwrite that
project(dimr)
