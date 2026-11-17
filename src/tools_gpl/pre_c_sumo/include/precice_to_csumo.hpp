#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_PRECICE_TO_CSUMO_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_PRECICE_TO_CSUMO_HPP

#include "csumo_data.hpp"
#include "precice_data.hpp"

namespace pre_c_sumo
{
    /// Convert one horizontal sample and its consecutive layers without changing either mesh.
    [[nodiscard]] FarFieldPoint2D convertPreciceToCSumoPoint(
        std::size_t index_2d, std::size_t index_3d, const PreciceMesh& mesh_2d, const PreciceMesh& mesh_3d);

    /// Assemble the complete C-SUMO far-field input for one diffuser.
    [[nodiscard]] FF2NFConfig convertPreciceToCSumo(
        const DiffuserSettings& diffuser, std::size_t mapping_index, const PreciceMesh& mesh_2d,
        const PreciceMesh& mesh_3d, double current_time_seconds);

    /// Return depth-averaged preCICE constituent values at a diffuser's intake.
    [[nodiscard]] std::vector<double> getIntakeConstituents(const PreciceMesh& mesh_3d, std::size_t diffuser_index);
} // namespace pre_c_sumo

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_PRECICE_TO_CSUMO_HPP
