# test dependencies
set(dependencies waq_external_access_layer)

create_test(
        test_connection_manager
        dependencies ${dependencies}
        visual_studio_folder tests/unit_test/engines_gpl/waq/waq_external_access_layer
        test_files test_connection_manager.f90
        test_list
        test_add_connection
        test_get_connection_by_exchange_name
        test_add_multiple_connections
        test_get_incoming_connections_by_category
)

create_test(
        test_connection_parser
        dependencies ${dependencies}
        visual_studio_folder tests/unit_test/engines_gpl/waq/waq_external_access_layer
        test_files test_connection_parser.f90
        test_list
        test_parse_connection_string_wasteload_flow_incoming
        test_parse_connection_string_incoming_const_VWind
        test_parse_connection_string_outgoing_cell_oxy_with_index
        test_parse_connection_string_outgoing_cell_oxy_with_id
)

create_test(
        test_connection_data_mapping
        dependencies ${dependencies}
        visual_studio_folder tests/unit_test/engines_gpl/waq/waq_external_access_layer
        test_files test_connection_data_mapping.f90
        test_list
        test_set_connection_data_wasteload_with_index
        test_set_connection_data_wasteload_with_location_id
        test_set_connection_data_incoming_wasteload
        test_set_connection_data_segment_with_index
        test_set_connection_data_observation_with_index
        test_set_connection_data_for_constant
)