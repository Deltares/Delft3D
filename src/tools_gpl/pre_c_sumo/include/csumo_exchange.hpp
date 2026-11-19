#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_EXCHANGE_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_EXCHANGE_HPP

#include <expected>
#include <string_view>
#include <vector>

#include "csumo_data.hpp"
#include "exchange_file_types.hpp"
#include "parsing_types.hpp"
#include "precice_data.hpp"

namespace pre_c_sumo
{
    /// @anchor pre_c_sumo_read_csumo_settings_file
    [[nodiscard]] std::expected<CSumoSettings, parsing_utils::ParseError> readCsumoSettingsFile(
        std::string_view csumo_settings_file_name);

    /// @anchor pre_c_sumo_write_ff2nf
    [[nodiscard]] std::expected<void, WriteError> writeFF2NFFiles(
        const CSumoSettings& csumo_settings, const PreciceMesh& mesh_2d, const PreciceMesh& mesh_3d,
        double current_time_seconds, const ExchangeFileOperations& file_operations = {});

    /// @anchor pre_c_sumo_wait_nf2ff
    bool waitForNF2FFFiles(const CSumoSettings& csumo_settings, double current_time_seconds);

    /// @anchor pre_c_sumo_read_nf2ff
    [[nodiscard]] std::expected<std::vector<NF2FFData>, parsing_utils::ParseError> readNF2FFFiles(
        const CSumoSettings& csumo_settings, double current_time_seconds);
} // namespace pre_c_sumo

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_EXCHANGE_HPP
