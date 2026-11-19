#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_EXCHANGE_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_EXCHANGE_HPP

#include <expected>
#include <string_view>
#include <vector>

#include "csumo_data.hpp"
#include "file_io.hpp"
#include "io_errors.hpp"
#include "precice_data.hpp"

namespace pre_c_sumo::c_sumo
{
    /// @anchor pre_c_sumo_read_csumo_settings_file
    [[nodiscard]] std::expected<Settings, io_errors::ReadError> readSettingsFile(
        std::string_view csumo_settings_file_name);

    /// @anchor pre_c_sumo_write_ff2nf
    [[nodiscard]] std::expected<void, io_errors::WriteError> writeFF2NFFiles(const Settings& csumo_settings,
                                                                             const precice::Mesh& mesh_2d,
                                                                             const precice::Mesh& mesh_3d,
                                                                             double current_time_seconds);

    /// @anchor pre_c_sumo_wait_nf2ff
    bool waitForNF2FFFiles(const Settings& csumo_settings, double current_time_seconds);

    /// @anchor pre_c_sumo_read_nf2ff
    [[nodiscard]] std::expected<std::vector<NF2FFData>, io_errors::ReadError> readNF2FFFiles(
        const Settings& csumo_settings, double current_time_seconds);
} // namespace pre_c_sumo::c_sumo

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_EXCHANGE_HPP
