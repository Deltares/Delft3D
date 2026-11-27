#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_CONVERSIONS_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_CONVERSIONS_HPP

#include <cstddef>
#include <expected>
#include <string>
#include <string_view>
#include <vector>

#include "csumo/data.hpp"
#include "precice/data.hpp"
#include "precice/sink_source_connections.hpp"

namespace pre_c_sumo
{
    /**
     * @brief Describes why a preCICE/C-SUMO data conversion could not be completed.
     */
    struct MeshConversionError
    {
        std::string message; ///< Error message.
    };

    /**
     * @brief Converts one horizontal sample and its consecutive vertical layers.
     *
     * Rejects invalid indices or quantity buffers that do not contain the requested samples.
     * @param index_2d Index of the horizontal sample in @p mesh_2d.
     * @param index_3d Index of the first vertical sample in @p mesh_3d.
     * @param mesh_2d Mesh containing horizontal coordinates and quantities.
     * @param mesh_3d Mesh containing vertically layered coordinates and quantities.
     * @return The converted far-field point, or a MeshConversionError if the buffers or indices are invalid.
     */
    [[nodiscard]] std::expected<c_sumo::FarFieldPoint2D, MeshConversionError> convertPreciceToCSumoPoint(
        std::size_t index_2d, std::size_t index_3d, const precice::Mesh& mesh_2d, const precice::Mesh& mesh_3d);

    /**
     * @brief Assembles FF2NF input for one diffuser from its preCICE mesh samples.
     *
     * Rejects inconsistent diffuser mappings or invalid quantity buffers.
     * @param diffuser Settings and locations for the diffuser.
     * @param mapping_index Index of this diffuser's mapping in both mesh forward maps.
     * @param mesh_2d Horizontal preCICE mesh and its received quantities.
     * @param mesh_3d Three-dimensional preCICE mesh and its received quantities.
     * @param current_time_seconds Simulation time represented by the samples [s].
     * @return FF2NF input on success, or a MeshConversionError if the mappings or buffers are invalid.
     */
    [[nodiscard]] std::expected<c_sumo::FF2NFConfig, MeshConversionError> convertPreciceToCSumo(
        const c_sumo::DiffuserSettings& diffuser, std::size_t mapping_index, const precice::Mesh& mesh_2d,
        const precice::Mesh& mesh_3d, double current_time_seconds);

    /**
     * @brief Constructs preCICE coordinates and diffuser mappings from C-SUMO sampling locations.
     * @param csumo_mesh_name Name to assign to the generated preCICE mesh.
     * @param diffusers Diffuser settings whose discharge, intake, and ambient locations define the mesh.
     * @param z_spec Specification of the mesh dimensions and vertical coordinates.
     * @return A mesh with sampling coordinates and a forward mapping for each diffuser.
     */
    [[nodiscard]] precice::Mesh createPreciceMesh(std::string_view csumo_mesh_name,
                                                  const std::vector<c_sumo::DiffuserSettings>& diffusers,
                                                  const precice::ZSpecification& z_spec);

    /**
     * @anchor pre_c_sumo_convert_nf_to_sink_source_connections
     * @brief Convert C-SUMO results into preCICE sink/source buffers without communicating.
     *
     * Uses cumulative entrainment differences, normalized source/intake weights, and
     * squared source weights for effective velocity. Excess constituents are offset
     * by depth-averaged intake values. C-SUMO positive-down z values become positive-up.
     * Single-source diffusers with sinks are expanded into DESA tracks.
     * Returns an error if an NF2FF result has more constituents than the preCICE data supports.
     * Diffuser settings and NF2FF results must have matching counts. Intake constituent buffers
     * must contain all layers referenced by the corresponding mesh mapping.
     *
     * @param diffuser_settings Settings corresponding by index to @p near_field_data.
     * @param near_field_data NF2FF results to convert.
     * @param mesh_3d Mesh providing the vertical mapping and intake constituent values.
     * @return Aligned sink/source buffers, or an error if the inputs are inconsistent or unsupported.
     */
    [[nodiscard]] std::expected<precice::SinkSourceConnections, precice::SinkSourceConnectionsError>
    convertCSumoToPrecice(const std::vector<c_sumo::DiffuserSettings>& diffuser_settings,
                          const std::vector<c_sumo::NF2FFData>& near_field_data, const precice::Mesh& mesh_3d);
} // namespace pre_c_sumo

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_CONVERSIONS_HPP
