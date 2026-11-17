#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_TO_PRECICE_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_TO_PRECICE_HPP

#include <expected>

#include "connected_sinks_sources.hpp"
#include "csumo_data.hpp"
#include "precice_data.hpp"

namespace pre_c_sumo
{
    /// Construct preCICE coordinates and diffuser mappings from C-SUMO sampling locations.
    [[nodiscard]] PreciceMesh createPreciceMesh(
        std::string_view mesh_name, const std::vector<DiffuserSettings>& diffusers, const ZSpecification& z_spec);

    /**
     * @anchor pre_c_sumo_convert_nf_to_connected_sink_sources
     * @brief Convert C-SUMO results into preCICE source/sink buffers without communicating.
     *
     * Uses cumulative entrainment differences, normalized source/intake weights, and
     * squared source weights for effective velocity. Excess constituents are offset
     * by depth-averaged intake values. C-SUMO positive-down z values become positive-up.
     * Single-source diffusers with sinks are expanded into DESA tracks.
     */
    [[nodiscard]] std::expected<ConnectedSinkSources, ConnectedSinkSourcesError> convertCSumoToPrecice(
        const std::vector<DiffuserSettings>& diffuser_settings, const std::vector<NF2FFData>& near_field_data,
        const PreciceMesh& mesh_3d);
} // namespace pre_c_sumo

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_TO_PRECICE_HPP
