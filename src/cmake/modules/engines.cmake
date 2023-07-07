# File to define the engines components and their corresponding tests
set(engines_path engines)

# D-Flow Flexible Mesh
set(dflowfm_interacter_module ${engines_path}/dflowfm/packages/dflowfm_interacter)

# RR Rainfall Runoff
set(rr_module_path "${engines_path}/rr/packages")
set(rr_dll_module "${rr_module_path}/rr_dll")
set(rr_kernel_c_module "${rr_module_path}/rr_kernel_c")
set(rr_kernel_f_module "${rr_module_path}/rr_kernel_f")
set(rr_walrus_c_module "${rr_module_path}/rr_walrus_c")
set(rr_module "${rr_module_path}/rr")

# RTC Real Time Control
set(rtc_module_path "${engines_path}/rtc/packages")
set(rtc_module "${rtc_module_path}/rtc")
set(rtc_plugin_c_module "${rtc_module_path}/plugin_rtc_c")
set(rtc_kernel_module "${rtc_module_path}/rtc_kernel")

# flow1d
set(flow1d_module_path "${engines_path}/delftflow/packages")
set(flow1d_cf_module "${flow1d_module_path}/delftflow")
set(flow1d_kernel_cf_module "${flow1d_module_path}/kernel_cf")

# flow1d2d
set(flow1d2d_module_path "${engines_path}/flow1d2d/packages")
set(flow1d2d_module "${flow1d2d_module_path}/flow1d2d")
set(flow1d2d_api_access_module "${flow1d2d_module_path}/flow1d_flowfm_api_access")
