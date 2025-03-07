if(NOT TARGET fbc-tools)
    add_subdirectory(${checkout_src_root}/${fbc_module_path} fbc-tools)
endif()

# Project name must be at the end of the configuration: it might get a name when including other configurations and needs to overwrite that
project(fbc-tools)
