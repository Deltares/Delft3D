#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_PRECICE_TO_CSUMO_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_PRECICE_TO_CSUMO_HPP

#include <expected>
#include <string>

#include "csumo_data.hpp"
#include "precice_data.hpp"

namespace pre_c_sumo
{
    struct MeshConversionError
    {
        std::string message;
    };

    /// Convert one horizontal sample and its consecutive layers, rejecting invalid buffers or indices.
    [[nodiscard]] std::expected<c_sumo::FarFieldPoint2D, MeshConversionError> convertPreciceToCSumoPoint(
        std::size_t index_2d, std::size_t index_3d, const precice::Mesh& mesh_2d, const precice::Mesh& mesh_3d);

    /// Assemble C-SUMO far-field input, rejecting inconsistent mappings or buffers.
    [[nodiscard]] std::expected<c_sumo::FF2NFConfig, MeshConversionError> convertPreciceToCSumo(
        const c_sumo::DiffuserSettings& diffuser, std::size_t mapping_index, const precice::Mesh& mesh_2d,
        const precice::Mesh& mesh_3d, double current_time_seconds);
} // namespace pre_c_sumo

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_PRECICE_TO_CSUMO_HPP
