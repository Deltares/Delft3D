#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_TO_PRECICE_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_TO_PRECICE_HPP

#include <expected>

#include "precice_sink_source_connections.hpp"
#include "csumo_data.hpp"
#include "precice_data.hpp"

namespace pre_c_sumo
{
    /// Construct preCICE coordinates and diffuser mappings from C-SUMO sampling locations.
    [[nodiscard]] precice::Mesh createPreciceMesh(std::string_view mesh_name,
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
     */
    [[nodiscard]] std::expected<precice::SinkSourceConnections, precice::SinkSourceConnectionsError>
    convertCSumoToPrecice(const std::vector<c_sumo::DiffuserSettings>& diffuser_settings,
                          const std::vector<c_sumo::NF2FFData>& near_field_data, const precice::Mesh& mesh_3d);
} // namespace pre_c_sumo

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_TO_PRECICE_HPP
