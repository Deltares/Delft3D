# Tools for Delft3D4

# UTILS_LGPL

if(NOT TARGET deltares_common)
    add_subdirectory(${checkout_src_root}/${deltares_common_module} deltares_common)
endif()
if(NOT TARGET deltares_common_c)
    add_subdirectory(${checkout_src_root}/${deltares_common_c_module} deltares_common_c)
endif()
if(NOT TARGET deltares_common_mpi)
    add_subdirectory(${checkout_src_root}/${deltares_common_mpi_module} deltares_common_mpi)
endif()

# emfsm
if(NOT TARGET esmfsm_version_number)
    add_subdirectory(${checkout_src_root}/${esmfsm_version_number_module} esmfsm_version_number)
endif()
if(NOT TARGET esmfsm_c)
    add_subdirectory(${checkout_src_root}/${esmfsm_c_module} esmfsm_c)
endif()
if(NOT TARGET esmfsm)
    add_subdirectory(${checkout_src_root}/${esmfsm_module} esmfsm)
endif()
if (UNIX)
    if(NOT TARGET esm_create)
        add_subdirectory(${checkout_src_root}/${utils_lgpl_path}/esmfsm/tests/esm_create esm_create)
    endif()
    if(NOT TARGET esm_delete)
        add_subdirectory(${checkout_src_root}/${utils_lgpl_path}/esmfsm/tests/esm_delete esm_delete)
    endif()
    if(NOT TARGET esm_info)
        add_subdirectory(${checkout_src_root}/${utils_lgpl_path}/esmfsm/tests/esm_info esm_info)
    endif()
endif(UNIX)

# Nefis
if(NOT TARGET nefis)
    add_subdirectory(${checkout_src_root}/${nefis_module} nefis)
endif()


# TOOLS_GPL

if(NOT TARGET datsel)
    add_subdirectory(${checkout_src_root}/${datsel_module} datsel)
    add_subdirectory(${checkout_src_root}/${datsel_f_module} datsel_f)
endif()

if(NOT TARGET kubint)
    add_subdirectory(${checkout_src_root}/${kubint_module} kubint)
    add_subdirectory(${checkout_src_root}/${kubint_f_module} kubint_f)
endif()

if(NOT TARGET lint)
    add_subdirectory(${checkout_src_root}/${lint_module} lint)
    add_subdirectory(${checkout_src_root}/${lint_f_module} lint_f)
endif()

if(NOT TARGET nesthd1)
    add_subdirectory(${checkout_src_root}/${nesthd1_module} nesthd1)
endif()

if(NOT TARGET nesthd2)
    add_subdirectory(${checkout_src_root}/${nesthd2_module} nesthd2)
endif()
