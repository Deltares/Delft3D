#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_PRE_C_SUMO_LIB_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_PRE_C_SUMO_LIB_HPP
#include <expected>

#include <string_view>
#include "csumo_settings_reader.hpp"

namespace pre_c_sumo
{
    /**
     * @brief Entry point into the C-SUMO preCICE library.
     *
     * @param csumoConfigFileName Path and filename of C-SUMO configuration xml file.
     * @param adapterConfigFileName Path and filename of preCICE adapter configuration file.
     * @return int Return 0 on success, non-zero on failure
     */
    int run(std::string_view csumoConfigFileName, std::string_view adapterConfigFileName);

    /**
     * @brief Legacy entry point for testing without preCICE. Not intended for production use.
     *
     * @return int Return 0 on success
     */
    int run();

    bool do_timeloop();
    std::expected<pre_c_sumo::CSumoSettingsReader, pre_c_sumo::ParseError> read_csumo_config_file(
        const std::string_view);
    void receive_ff_data();
    void write_ff2nf_files(std::expected<pre_c_sumo::CSumoSettingsReader, pre_c_sumo::ParseError>);
    void wait_for_nf2ff_files(std::expected<pre_c_sumo::CSumoSettingsReader, pre_c_sumo::ParseError>);
    void read_nf2ff_files(std::expected<pre_c_sumo::CSumoSettingsReader, pre_c_sumo::ParseError>);
    void convert_nf_to_sources_sinks(std::expected<pre_c_sumo::CSumoSettingsReader, pre_c_sumo::ParseError>);
    void send_sources_sinks_to_ff(std::expected<pre_c_sumo::CSumoSettingsReader, pre_c_sumo::ParseError>);
    void process_sinks();
    void process_intakes();
    void process_sources();
    bool is_diffuser_modelled();
    void process_source_locations();
    void create_diffuser_model();
} // namespace pre_c_sumo

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_PRE_C_SUMO_LIB_HPP
