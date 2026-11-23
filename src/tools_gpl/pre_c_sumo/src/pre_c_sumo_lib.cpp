#include "pre_c_sumo_lib.hpp"

#include <cstddef>
#include <cstdio>
#include <precice/precice.hpp>
#include <print>
#include <string_view>
#include <utility>
#include <vector>

#include "conversions.hpp"
#include "csumo/exchange.hpp"
#include "csumo/ff2nf_writer.hpp"
#include "precice/data.hpp"
#include "precice/exchange.hpp"
#include "precice/sink_source_connections.hpp"

namespace pre_c_sumo
{
    /**
     * @details Entry point into the preC-SUMO preCICE library.
     */
    int run(const std::string_view csumo_settings_file_name, const std::string_view precice_config_file_name)
    {
        constexpr int mpi_rank = 0;
        constexpr int mpi_size = 1;
        ::precice::Participant participant{"preC-SUMO", precice_config_file_name, mpi_rank, mpi_size};

        const auto csumo_settings = c_sumo::readSettingsFile(csumo_settings_file_name);
        if (!csumo_settings.has_value())
        {
            std::println(stderr, "Error: Unable to load {}: {}\n", csumo_settings_file_name,
                         csumo_settings.error().message);
            return -1;
        }

        // Construct 2d mesh
        precice::Mesh csumo_2d_mesh =
            createPreciceMesh("csumo_2d_nodes", csumo_settings->diffusers, precice::ZSpecification{true, 0, 0, 0});
        participant.setMeshVertices(csumo_2d_mesh.name, csumo_2d_mesh.coordinates, csumo_2d_mesh.vertex_ids);
        // Add preCICE quantity data buffers.
        csumo_2d_mesh.quantities[precice::water_levels_id] = std::vector<double>(csumo_2d_mesh.number_of_nodes);
        csumo_2d_mesh.quantities[precice::bed_levels_id] = std::vector<double>(csumo_2d_mesh.number_of_nodes);

        // Construct 3d mesh
        precice::Mesh csumo_3d_mesh = createPreciceMesh("csumo_3d_nodes", csumo_settings->diffusers,
                                                        precice::ZSpecification{false, -11.0, 3.0, 1.0});
        participant.setMeshVertices(csumo_3d_mesh.name, csumo_3d_mesh.coordinates, csumo_3d_mesh.vertex_ids);
        // Add preCICE quantity data buffers.
        csumo_3d_mesh.quantities[precice::densities_id] = std::vector<double>(csumo_3d_mesh.number_of_nodes);
        csumo_3d_mesh.quantities[precice::flow_velocities_id] = std::vector<double>(csumo_3d_mesh.number_of_nodes * 3);
        for (const auto id : precice::constituent_ids)
        {
            csumo_3d_mesh.quantities[id] = std::vector<double>(csumo_3d_mesh.number_of_nodes);
        }

        double current_time_seconds = 0.0;
        if (!c_sumo::waitForNF2FFFiles(csumo_settings.value(), current_time_seconds))
        {
            std::println(stderr, "Error: Timeout on waiting for NF2FF data for time = {} seconds",
                         current_time_seconds);
            return -1;
        }
        const auto initial_nf2ff_readers = c_sumo::readNF2FFFiles(csumo_settings.value(), current_time_seconds);
        if (!initial_nf2ff_readers.has_value())
        {
            std::println(stderr, "Error: Unable to read initial NF2FF data: {}", initial_nf2ff_readers.error().message);
            return -1;
        }
        auto initial_result = convertCSumoToPrecice(csumo_settings->diffusers, *initial_nf2ff_readers, csumo_3d_mesh);

        if (!initial_result.has_value())
        {
            std::println(stderr, "Error: Unable to convert initial NF2FF data: {}", initial_result.error().message);
            return -1;
        }

        precice::SinkSourceConnections initial_sink_source_connections = initial_result.value();

        // Set sinks/sources mesh
        constexpr std::string_view sink_source_connections_mesh_name = "sources_sinks_nodes";
        precice::SinkSourceConnectionsMesh sink_source_connections_mesh;
        sink_source_connections_mesh.setVertexCount(initial_sink_source_connections.get_number_of_entries());
        participant.setMeshVertices(sink_source_connections_mesh_name, sink_source_connections_mesh.coordinates,
                                    sink_source_connections_mesh.precice_ids);
        if (participant.requiresInitialData())
        {
            std::println(
                stderr,
                "Error: preC-SUMO cannot provide initial coupling data."
                "Remove initialize=\"true\" from all exchanges sent by preC-SUMO in the preCICE configuration.");
            return -1;
        }

        participant.initialize();
        while (participant.isCouplingOngoing())
        {
            double coupling_time_step = participant.getMaxTimeStepSize();

            precice::receiveFFData(participant, csumo_2d_mesh, csumo_3d_mesh, coupling_time_step);
            for (std::size_t index = 0; index < csumo_settings->diffusers.size(); ++index)
            {
                auto input = convertPreciceToCSumo(csumo_settings->diffusers[index], index, csumo_2d_mesh,
                                                   csumo_3d_mesh, current_time_seconds);
                if (!input)
                {
                    std::println(stderr,
                                 "Error: Unable to write FF2NF data at time {} s: "
                                 "Failed to convert preCICE data for diffuser {}: {}",
                                 current_time_seconds, index + 1, input.error().message);
                    return -1;
                }
                const auto ff2nf_result = c_sumo::FF2NFWriter(std::move(*input)).write();
                if (!ff2nf_result)
                {
                    std::println(stderr, "Error: Unable to write FF2NF data at time {} s: {}", current_time_seconds,
                                 ff2nf_result.error().message);
                    return -1;
                }
            }
            if (!c_sumo::waitForNF2FFFiles(csumo_settings.value(), current_time_seconds))
            {
                std::println(stderr, "Error: Timeout on waiting for NF2FF data for time = {} seconds",
                             current_time_seconds);
                return -1;
            }
            const auto nf2ff_readers = c_sumo::readNF2FFFiles(csumo_settings.value(), current_time_seconds);
            if (!nf2ff_readers.has_value())
            {
                std::println(stderr, "Error: Unable to read NF2FF data: {}", nf2ff_readers.error().message);
                return -1;
            }
            auto conversion_result = convertCSumoToPrecice(csumo_settings->diffusers, *nf2ff_readers, csumo_3d_mesh);
            if (!conversion_result.has_value())
            {
                std::println(stderr, "Unable to convert NF to preCICE sink/source entries: {}",
                             conversion_result.error().message);
                return -1;
            }
            precice::SinkSourceConnections sink_source_connections = conversion_result.value();

            if (participant.isTimeWindowComplete())
            {
                participant.resetMesh(sink_source_connections_mesh_name);
                sink_source_connections_mesh.setVertexCount(sink_source_connections.get_number_of_entries());
                participant.setMeshVertices(sink_source_connections_mesh_name, sink_source_connections_mesh.coordinates,
                                            sink_source_connections_mesh.precice_ids);
                std::println("Reset sinks/sources mesh to {} vertices.",
                             sink_source_connections.get_number_of_entries());
            }
            else
            {
                std::println("Skipped mesh reset. Want to write {} entries, mesh has {} vertices.",
                             sink_source_connections.get_number_of_entries(),
                             sink_source_connections_mesh.precice_ids.size());
            }

            auto write_result =
                precice::writeSinkSourceConnections(participant, sink_source_connections_mesh_name,
                                                    sink_source_connections_mesh.precice_ids, sink_source_connections);

            if (!write_result.has_value())
            {
                std::println(stderr, "Error: Unable to write sinks/sources data at time {} s: {}", current_time_seconds,
                             write_result.error().message);
                return -1;
            }

            participant.advance(coupling_time_step);
            current_time_seconds += coupling_time_step;
        }
        std::println("preC-SUMO finished.");
        return 0;
    }

    /**
     * @details This function prints a greeting message to the console using C++23's std::println.
     */
    int run()
    {
        std::println("Hello, world from preC-SUMO application!");
        return 0;
    }

} // namespace pre_c_sumo
