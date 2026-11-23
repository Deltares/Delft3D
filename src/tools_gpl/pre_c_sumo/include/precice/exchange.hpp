#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_PRECICE_EXCHANGE_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_PRECICE_EXCHANGE_HPP

#include <expected>

#include "precice/sink_source_connections.hpp"
#include "precice/data.hpp"

namespace precice
{
    class Participant;
}

namespace pre_c_sumo::precice
{
    /// @anchor pre_c_sumo_receive_ff_data
    void receiveFFData(::precice::Participant& participant, Mesh& mesh_2d, Mesh& mesh_3d, double coupling_time_step);

    /**
     * @anchor pre_c_sumo_sink_source_connections_write_to_precice
     * @brief Send preCICE sink/source buffers, clearing them only after a successful write.
     *
     * Empty buffers send zeros for every registered vertex. Non-empty buffers must
     * match the registered vertex count.
     */
    [[nodiscard]] std::expected<void, SinkSourceConnectionsError> writeSinkSourceConnections(
        ::precice::Participant& participant, std::string_view mesh_name, const std::vector<int>& vertex_ids,
        SinkSourceConnections& connections);
} // namespace pre_c_sumo::precice

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_PRECICE_EXCHANGE_HPP
