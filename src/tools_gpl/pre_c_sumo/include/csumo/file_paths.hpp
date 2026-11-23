#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_FILE_PATHS_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_FILE_PATHS_HPP

#include "csumo/data.hpp"

namespace pre_c_sumo::c_sumo
{
    [[nodiscard]] std::filesystem::path ff2nfFilepath(const DiffuserSettings& diffuser, int subgrid_model_nr,
                                                      double current_time_seconds);
    [[nodiscard]] std::filesystem::path nf2ffFilepath(const DiffuserSettings& diffuser, int subgrid_model_nr,
                                                      double current_time_seconds);
    [[nodiscard]] std::vector<std::filesystem::path> ff2nfFilepaths(
        const Settings& settings, double current_time_seconds);
    [[nodiscard]] std::vector<std::filesystem::path> nf2ffFilepaths(
        const Settings& settings, double current_time_seconds);
} // namespace pre_c_sumo::c_sumo

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_FILE_PATHS_HPP
