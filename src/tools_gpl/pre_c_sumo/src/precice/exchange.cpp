#include "precice/exchange.hpp"

#include <cstddef>
#include <expected>
#include <format>
#include <precice/precice.hpp>
#include <string_view>
#include <vector>

#include "precice/data.hpp"
#include "precice/sink_source_connections.hpp"

namespace pre_c_sumo::precice
{
    void receiveFFData(::precice::Participant& participant, Mesh& csumo_2d_mesh, Mesh& csumo_3d_mesh,
                       const double coupling_time_step)
    {
        for (auto& quantity : csumo_2d_mesh.quantities)
        {
            participant.readData(csumo_2d_mesh.name, quantity.first, csumo_2d_mesh.vertex_ids, coupling_time_step,
                                 quantity.second);
        }
        for (auto& quantity : csumo_3d_mesh.quantities)
        {
            participant.readData(csumo_3d_mesh.name, quantity.first, csumo_3d_mesh.vertex_ids, coupling_time_step,
                                 quantity.second);
        }
    }

    std::expected<void, SinkSourceConnectionsError> writeSinkSourceConnections(::precice::Participant& participant,
                                                                               const SinkSourceConnectionsMesh& mesh,
                                                                               SinkSourceConnections& connections)
    {
        const auto& data = connections.data();
        const std::size_t registered_vertex_count = mesh.precice_ids.size();
        const std::size_t entry_count = connections.get_number_of_entries();

        if (registered_vertex_count == 0)
        {
            return std::unexpected(SinkSourceConnectionsError{"Cannot write sinks/sources to an empty preCICE mesh."});
        }

        if (entry_count != 0 && entry_count != registered_vertex_count)
        {
            return std::unexpected(SinkSourceConnectionsError{
                std::format("Connected sink/source count changed from the registered preCICE mesh size {} to {}. "
                            "Remeshing is not implemented.",
                            registered_vertex_count, entry_count)});
        }

        const std::vector<double> zero_values(registered_vertex_count, 0.0);

        // preCICE expects values for all registered vertices each step.
        // When no entries are present, write explicit zeros with fixed mesh length.
        auto write_or_zero = [&](std::string_view data_name, const std::vector<double>& values) {
            if (values.empty())
            {
                participant.writeData(mesh.name, data_name, mesh.precice_ids, zero_values);
                return;
            }

            participant.writeData(mesh.name, data_name, mesh.precice_ids, values);
        };

        write_or_zero("sinks_x", data.sink_x_vector);
        write_or_zero("sinks_y", data.sink_y_vector);
        write_or_zero("sinks_z_min", data.sink_z_bottom_vector);
        write_or_zero("sinks_z_max", data.sink_z_top_vector);
        write_or_zero("sources_x", data.source_x_vector);
        write_or_zero("sources_y", data.source_y_vector);
        write_or_zero("sources_z_min", data.source_z_bottom_vector);
        write_or_zero("sources_z_max", data.source_z_top_vector);
        write_or_zero("sources_sinks_discharge", data.discharge_vector);
        write_or_zero("sources_momentum_magnitude_weighted", data.momentum_magnitude_weighted_vector);
        write_or_zero("sources_momentum_direction", data.momentum_direction_vector);
        for (std::size_t index = 0; index < constituent_ids.size(); ++index)
        {
            write_or_zero(constituent_ids[index], data.constituents_vectors[index]);
        }

        // After the write, we can clear the list.
        connections.clear();
        return {};
    }

} // namespace pre_c_sumo::precice
