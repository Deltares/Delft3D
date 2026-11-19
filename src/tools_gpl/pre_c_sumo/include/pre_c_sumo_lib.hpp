#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_PRE_C_SUMO_LIB_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_PRE_C_SUMO_LIB_HPP
#include <string_view>

namespace pre_c_sumo
{
    /**
     * @anchor pre_c_sumo_run_main
     * @brief Entry point into the C-SUMO preCICE library.
     *
     * Main workflow overview:
     * @dotfile preC_SUMO_Swimlanes.dot "preC-SUMO coupling workflow" width=900px
     *
     * Before preCICE initialization, the initial NF2FF files are read and converted to determine and register
     * the sink/source mesh.
     *
     * The settings are read once. For each coupling window, the core sequence is: @ref pre_c_sumo_receive_ff_data ->
     * @ref pre_c_sumo_write_ff2nf -> @ref pre_c_sumo_wait_nf2ff -> @ref pre_c_sumo_read_nf2ff ->
     * @ref pre_c_sumo_convert_nf_to_sink_source_connections ->
     * @ref pre_c_sumo_sink_source_connections_write_to_precice, followed by advancing the preCICE participant.
     *
     * @param csumo_settings_file_name Path and filename of the C-SUMO configuration XML file.
     * @param precice_config_file_name Path and filename of the preCICE adapter configuration file.
     * @return Returns 0 on success and a non-zero value on failure.
     */
    int run(std::string_view csumo_settings_file_name, std::string_view precice_config_file_name);

    /**
     * @anchor pre_c_sumo_run_default
     * @brief Test helper that prints a greeting without initializing a preCICE participant.
     *
     * @return Returns 0 on success.
     */
    int run();

} // namespace pre_c_sumo

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_PRE_C_SUMO_LIB_HPP
