# test dependencies
set(dependencies deltares_common
                 waq_hyd_data
                 io_netcdf_data
                 io_netcdf
                 netcdff
				 io_hyd)

create_test(
        tests_aggregate_waqgeom
        dependencies ${dependencies}
        visual_studio_folder tests/unit_test/utils_lgpl/io_hyd/packages/io_hyd
        test_files tests_aggregate_waqgeom.f90
        test_list
		tests_aggregate_ugrid_layers_interfaces
		labels
		"tests_aggregate_ugrid_layers_interfaces:fast"
)
