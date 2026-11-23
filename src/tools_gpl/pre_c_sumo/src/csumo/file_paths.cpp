#include "csumo/file_paths.hpp"

#include <filesystem>
#include <format>
#include <ranges>
#include <string>
#include <vector>

#include "csumo/data.hpp"

namespace pre_c_sumo::c_sumo
{
    std::filesystem::path ff2nfFilepath(const DiffuserSettings& diffuser, int subgrid_model_nr,
                                        double current_time_seconds)
    {
        const std::string run_id = "preC-SUMO";
        return diffuser.ff2nf_dir /
               std::format("FF2NF__{}_SubMod{:03d}_{:.3f}.xml", run_id, subgrid_model_nr, current_time_seconds / 60.0);
    }

    std::filesystem::path nf2ffFilepath(const DiffuserSettings& diffuser, int subgrid_model_nr,
                                        double current_time_seconds)
    {
        const std::string run_id = "preC-SUMO";
        const std::filesystem::path nf2ff_dir =
            diffuser.ff2nf_dir.has_parent_path() ? diffuser.ff2nf_dir.parent_path() / "NF2FF" : "NF2FF";
        return nf2ff_dir /
               std::format("NF2FF__{}_SubMod{:03d}_{:.3f}.xml", run_id, subgrid_model_nr, current_time_seconds / 60.0);
    }

    std::vector<std::filesystem::path> ff2nfFilepaths(const Settings& settings, double current_time_seconds)
    {
        std::vector<std::filesystem::path> paths;
        for (const auto& [index, diffuser] : settings.diffusers | std::views::enumerate)
        {
            paths.emplace_back(ff2nfFilepath(diffuser, static_cast<int>(index + 1), current_time_seconds));
        }
        return paths;
    }

    std::vector<std::filesystem::path> nf2ffFilepaths(const Settings& settings, double current_time_seconds)
    {
        std::vector<std::filesystem::path> paths;
        for (const auto& [index, diffuser] : settings.diffusers | std::views::enumerate)
        {
            paths.emplace_back(nf2ffFilepath(diffuser, static_cast<int>(index + 1), current_time_seconds));
        }
        return paths;
    }
} // namespace pre_c_sumo::c_sumo
