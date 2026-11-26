#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_PRECICE_EXCHANGE_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_PRECICE_EXCHANGE_HPP

#include <expected>

#include "precice/data.hpp"
#include "precice/sink_source_connections.hpp"

namespace precice
{
    class Participant;
}

namespace pre_c_sumo::precice
{
    /**
     * @anchor pre_c_sumo_receive_ff_data
     * @brief Reads all registered far-field quantities for the current coupling step.
     * @param participant preCICE participant from which to read.
     * @param mesh_2d Horizontal mesh and its quantity buffers to fill.
     * @param mesh_3d Three-dimensional mesh and its quantity buffers to fill.
     * @param coupling_time_step preCICE time-step size to use for the read [s].
     */
    void receiveFFData(::precice::Participant& participant, Mesh& mesh_2d, Mesh& mesh_3d, double coupling_time_step);

    /**
     * @anchor pre_c_sumo_sink_source_connections_write_to_precice
     * @brief Send preCICE sink/source buffers, clearing them only after a successful write.
     *
     * Empty buffers send zeros for every registered vertex. Non-empty buffers must
     * match the registered vertex count.
     * @param participant preCICE participant to write through.
     * @param mesh Registered sink/source mesh and its vertex IDs.
     * @param connections Aligned sink/source buffers; cleared after a successful write.
     * @return Success, or an error if the mesh and buffer sizes are inconsistent.
     */
    [[nodiscard]] std::expected<void, SinkSourceConnectionsError> writeSinkSourceConnections(
        ::precice::Participant& participant, const SinkSourceConnectionsMesh& mesh, SinkSourceConnections& connections);
} // namespace pre_c_sumo::precice

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_PRECICE_EXCHANGE_HPP
