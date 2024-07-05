# File to define the engines_gpl components and their corresponding tests
set(engines_gpl_path engines_gpl)

# D-Flow Flexible Mesh
set(dflowfm_kernel_module ${engines_gpl_path}/dflowfm/packages/dflowfm_kernel)
set(dflowfm_cli_exe_module ${engines_gpl_path}/dflowfm/packages/dflowfm-cli_exe)
set(dflowfm_lib_module ${engines_gpl_path}/dflowfm/packages/dflowfm_lib)

# Waq
set(waq_module_path ${engines_gpl_path}/waq)
set(waq_definition_module ${waq_module_path}/waq_definition)
set(waq_utils_c_module ${waq_module_path}/waq_utils_c)
set(waq_utils_f_module ${waq_module_path}/waq_utils_f)
set(waq_netcdf_module ${waq_module_path}/waq_netcdf)
set(waq_validation_module ${waq_module_path}/waq_validation)
set(waq_external_access_layer_module ${waq_module_path}/waq_external_access_layer)
set(waq_preprocessor_module ${waq_module_path}/waq_preprocessor)
set(waq_proc_preprocess_module ${waq_module_path}/waq_proc_preprocess)
set(waq_computation_module ${waq_module_path}/waq_computation)
set(waq_process_module ${waq_module_path}/waq_process)
set(wq_processes_module ${waq_module_path}/wq_processes)
set(waq_plugin_wasteload_module ${waq_module_path}/waq_plugin_wasteload)
set(waq_kernel_module ${waq_module_path}/waq_kernel)
set(waq_memory_module ${waq_module_path}/waq_memory)
set(waq_io_module ${waq_module_path}/waq_io)
set(waq_data_module ${waq_module_path}/waq_data)
set(delwaq_lib_module ${waq_module_path}/delwaq_lib)
set(waq_logging_module ${waq_module_path}/waq_logging)
set(delwaq_exe_module ${waq_module_path}/delwaq_exe)

# Waves
set(wave_module_path "${engines_gpl_path}/wave/packages")
set(wave_data_module "${wave_module_path}/data")
set(wave_io_module "${wave_module_path}/io")
set(wave_kernel_module "${wave_module_path}/kernel")
set(wave_manager_module "${wave_module_path}/manager")
set(wave_module "${wave_module_path}/wave")

# Flow2D3D
set(flow2d3d_module_path "${engines_gpl_path}/flow2d3d/packages")
set(flow2d3d_data_module "${flow2d3d_module_path}/flow2d3d_data")
set(flow2d3d_io_dol_f_module "${flow2d3d_module_path}/flow2d3d_io_dol_f")
set(flow2d3d_io_module "${flow2d3d_module_path}/flow2d3d_io")
set(flow2d3d_kernel_dd_f_module "${flow2d3d_module_path}/flow2d3d_kernel_dd_f")
set(flow2d3d_kernel_module "${flow2d3d_module_path}/flow2d3d_kernel")
set(flow2d3d_manager_module "${flow2d3d_module_path}/flow2d3d_manager")
set(flow2d3d_module "${flow2d3d_module_path}/flow2d3d")
set(flow2d3d_plugin_culvert_c_module "${flow2d3d_module_path}/flow2d3d_plugin_culvert_c")
set(flow2d3d_plugin_user_module "${flow2d3d_module_path}/flow2d3d_plugin_user")

# Part
set(part_module_path ${engines_gpl_path}/part)
set(part_data_f_module ${part_module_path}/part_data)
set(part_utils_f_module ${part_module_path}/part_utils)
set(part_io_f_module ${part_module_path}/part_io)
set(part_kernel_f_module ${part_module_path}/part_kernel)
set(delpar_module ${part_module_path}/delpar)

# Dimr
set(dimr_module_path "${engines_gpl_path}/dimr/packages")
set(dimr_lib_module "${dimr_module_path}/dimr_lib")
set(dimr_module "${dimr_module_path}/dimr")

# d_hydro.exe
set(d_hydro_module_path "${engines_gpl_path}/d_hydro/packages")
set(d_hydro_module "${d_hydro_module_path}/d_hydro")

# Tests
set(tests_directory src/test/engines_gpl)
set(dflowfm_kernel_test_module ${tests_directory}/dflowfm/packages/dflowfm_kernel)

# Unit tests
# WAQ
set(delwaq_tests_module ${unit_tests_dir}/engines_gpl/waq)

# Install
set(install_dflowfm_module cmake/install_fm)
set(install_waq_module cmake/install_waq)
set(install_wave_module cmake/install_wave)
set(install_dimr_module cmake/install_dimr)
set(install_tests_module cmake/install_tests)
set(install_d_hydro_module cmake/install_d_hydro)
set(install_flow2d3d_module cmake/install_flow2d3d)
