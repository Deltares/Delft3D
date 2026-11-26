#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_EXCHANGE_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_EXCHANGE_HPP

#include <expected>
#include <string_view>
#include <vector>

#include "csumo/data.hpp"
#include "utils/io_errors.hpp"

namespace pre_c_sumo::c_sumo
{
    /**
     * @anchor pre_c_sumo_read_csumo_settings_file
     * @brief Reads and parses a C-SUMO settings file.
     * @param csumo_settings_file_name Path to the settings XML file.
     * @return Parsed settings, or a ReadError if the file cannot be read or parsed.
     */
    [[nodiscard]] std::expected<Settings, utils::io_errors::ReadError> readSettingsFile(
        std::string_view csumo_settings_file_name);

    /**
     * @anchor pre_c_sumo_wait_nf2ff
     * @brief Waits for the NF2FF files for a coupling time to be present and complete.
     * @param csumo_settings Settings identifying the expected NF2FF files.
     * @param current_time_seconds Simulation time for which files are expected [s].
     * @return True if all expected files become available before the timeout; otherwise false.
     */
    bool waitForNF2FFFiles(const Settings& csumo_settings, double current_time_seconds);

    /**
     * @anchor pre_c_sumo_read_nf2ff
     * @brief Reads and parses the NF2FF result file for each configured diffuser.
     * @param csumo_settings Settings identifying the diffusers and exchange paths.
     * @param current_time_seconds Simulation time represented by the results [s].
     * @return Parsed results in diffuser order, or a ReadError on the first read or parse failure.
     */
    [[nodiscard]] std::expected<std::vector<NF2FFData>, utils::io_errors::ReadError> readNF2FFFiles(
        const Settings& csumo_settings, double current_time_seconds);
} // namespace pre_c_sumo::c_sumo

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_EXCHANGE_HPP
