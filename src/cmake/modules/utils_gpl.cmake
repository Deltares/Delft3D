# File to define the utils_gpl components and their corresponding tests
set(utils_gpl_path utils_gpl)

# Ice
set(ice_module_path "${utils_gpl_path}/ice/packages")
set(ice_data_module ${ice_module_path}/ice_data)
set(ice_io_module ${ice_module_path}/ice_io)

# Trachytopes
set(trachytopes_path "${utils_gpl_path}/trachytopes/packages")
set(trachytopes_kernel_module ${trachytopes_path}/trachytopes_kernel)
set(trachytopes_io_module ${trachytopes_path}/trachytopes_io)

# Flow 1D
set(flow1d_module_path "${utils_gpl_path}/flow1d/packages")
set(flow1d_core_module ${flow1d_module_path}/flow1d_core)
set(flow1d_io_module ${flow1d_module_path}/flow1d_io)
set(flow1d_module ${flow1d_module_path}/flow1d)

# Morphology
set(morphology_module_path "${utils_gpl_path}/morphology/packages")
set(morphology_plugins_c_module "${morphology_module_path}/morphology_plugins_c")
set(morphology_data_module "${morphology_module_path}/morphology_data")
set(morphology_kernel_module "${morphology_module_path}/morphology_kernel")
set(morphology_io_module "${morphology_module_path}/morphology_io")
set(morphology_waq_module "${morphology_module_path}/morphology_waq")

# Hydrology
set(hydrology_module_path "${utils_gpl_path}/dhydrology/packages")
set(hydrology_kernel_module "${hydrology_module_path}/dhydrology_kernel")
set(hydrology_kernel_io_module "${hydrology_module_path}/dhydrology_io")

# Control Lib
set(control_lib_path "${utils_gpl_path}/control_lib/packages")
set(control_lib_module ${control_lib_path}/control_lib)

# rr_rtc_tools
set(rr_rtc_tools_module_path "${utils_gpl_path}/rr_rtc_tools/packages")
set(rr_rtc_tools_module ${rr_rtc_tools_module_path}/rr_rtc_tools)

# wl_openmi_support
set(wl_openmi_support_module_path "${utils_gpl_path}/wl_openmi_support/packages")
set(wl_openmi_support_module "${wl_openmi_support_module_path}/wl_openmi_support")
