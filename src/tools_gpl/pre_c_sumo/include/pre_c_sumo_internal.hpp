#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_INTERNAL_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_INTERNAL_HPP
#include <expected>

#include <string_view>
#include "csumo_settings_reader.hpp"

namespace pre_c_sumo
{
    bool do_timeloop();
    std::expected<pre_c_sumo::CSumoSettingsReader, pre_c_sumo::ParseError> read_csumo_config_file(
        const std::string_view);
    void receive_ff_data();
    void write_ff2nf_files(std::expected<pre_c_sumo::CSumoSettingsReader, pre_c_sumo::ParseError>);
    void wait_for_nf2ff_files(std::expected<pre_c_sumo::CSumoSettingsReader, pre_c_sumo::ParseError>);
    void read_nf2ff_files(std::expected<pre_c_sumo::CSumoSettingsReader, pre_c_sumo::ParseError>);
    void convert_nf_to_sources_sinks(std::expected<pre_c_sumo::CSumoSettingsReader, pre_c_sumo::ParseError>);
    void send_sources_sinks_to_ff(std::expected<pre_c_sumo::CSumoSettingsReader, pre_c_sumo::ParseError>);
    void convert_nf_sinks_to_ff();
    void convert_nf_intakes_to_ff();
    void convert_nf_sources_to_ff();
    bool is_diffuser_modelled();
    void process_source_locations();
    void create_diffuser_model();

} // namespace pre_c_sumo

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_INTERNAL_HPP
