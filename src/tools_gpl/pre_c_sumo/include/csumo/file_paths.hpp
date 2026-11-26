#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_FILE_PATHS_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_FILE_PATHS_HPP

#include <filesystem>
#include <vector>

#include "csumo/data.hpp"

namespace pre_c_sumo::c_sumo
{
    /**
     * @brief Builds the FF2NF exchange filename for one diffuser and simulation time.
     * @param diffuser Diffuser settings containing the exchange directory.
     * @param subgrid_model_nr One-based sub-grid model number.
     * @param current_time_seconds Simulation time represented by the file [s].
     * @return Path to the corresponding FF2NF XML file.
     */
    [[nodiscard]] std::filesystem::path ff2nfFilepath(const DiffuserSettings& diffuser, int subgrid_model_nr,
                                                      double current_time_seconds);

    /**
     * @brief Builds the NF2FF result filename for one diffuser and simulation time.
     * @param diffuser Diffuser settings containing the exchange directory.
     * @param subgrid_model_nr One-based sub-grid model number.
     * @param current_time_seconds Simulation time represented by the file [s].
     * @return Path to the corresponding NF2FF XML file.
     */
    [[nodiscard]] std::filesystem::path nf2ffFilepath(const DiffuserSettings& diffuser, int subgrid_model_nr,
                                                      double current_time_seconds);

    /**
     * @brief Builds FF2NF file paths for all configured diffusers in settings order.
     * @param settings Settings containing the diffusers.
     * @param current_time_seconds Simulation time represented by the files [s].
     * @return One FF2NF path per diffuser.
     */
    [[nodiscard]] std::vector<std::filesystem::path> ff2nfFilepaths(const Settings& settings,
                                                                    double current_time_seconds);

    /**
     * @brief Builds NF2FF file paths for all configured diffusers in settings order.
     * @param settings Settings containing the diffusers.
     * @param current_time_seconds Simulation time represented by the files [s].
     * @return One NF2FF path per diffuser.
     */
    [[nodiscard]] std::vector<std::filesystem::path> nf2ffFilepaths(const Settings& settings,
                                                                    double current_time_seconds);
} // namespace pre_c_sumo::c_sumo

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_FILE_PATHS_HPP
