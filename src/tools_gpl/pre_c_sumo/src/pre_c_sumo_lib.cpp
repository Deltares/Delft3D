#include "pre_c_sumo_lib.hpp"

#include <precice/precice.hpp>
#include <print>
#include <string_view>
#include <stdexcept>
#include <vector>
#include <map>

#include "csumo_settings_reader.hpp"
#include "connected_sinks_sources.hpp"
#include "coupling_steps.hpp"
#include "csumo_to_precice.hpp"
#include "precice_exchange.hpp"

namespace pre_c_sumo
{
    /**
     * @details Entry point into the preC-SUMO preCICE library.
     */
    int run(const std::string_view csumo_settings_file_name, const std::string_view precice_config_file_name)
    {
        constexpr int mpi_rank = 0;
        constexpr int mpi_size = 1;
        precice::Participant participant{"preC-SUMO", precice_config_file_name, mpi_rank, mpi_size};

        const auto csumo_settings = readCsumoSettingsFile(csumo_settings_file_name);
        if (!csumo_settings.has_value())
        {
            std::println(stderr, "Error: Unable to load {}: {}\n", csumo_settings_file_name,
                         csumo_settings.error().message);
            return -1;
        }

        // Construct 2d mesh
        PreciceMesh csumo_2d_mesh =
            createPreciceMesh("csumo_2d_nodes", csumo_settings->diffusers, ZSpecification{true, 0, 0, 0});
        participant.setMeshVertices(csumo_2d_mesh.name, csumo_2d_mesh.coordinates, csumo_2d_mesh.vertex_ids);
        // Add preCICE quantity data buffers.
        csumo_2d_mesh.quantities[water_levels_id] = std::vector<double>(csumo_2d_mesh.number_of_nodes);
        csumo_2d_mesh.quantities[bed_levels_id] = std::vector<double>(csumo_2d_mesh.number_of_nodes);

        // Construct 3d mesh
        PreciceMesh csumo_3d_mesh =
            createPreciceMesh("csumo_3d_nodes", csumo_settings->diffusers, ZSpecification{false, -11.0, 3.0, 1.0});
        participant.setMeshVertices(csumo_3d_mesh.name, csumo_3d_mesh.coordinates, csumo_3d_mesh.vertex_ids);
        // Add preCICE quantity data buffers.
        csumo_3d_mesh.quantities[densities_id] = std::vector<double>(csumo_3d_mesh.number_of_nodes);
        csumo_3d_mesh.quantities[flow_velocities_id] = std::vector<double>(csumo_3d_mesh.number_of_nodes * 3);
        for (const auto id : constituent_ids)
        {
            csumo_3d_mesh.quantities[id] = std::vector<double>(csumo_3d_mesh.number_of_nodes);
        }

        double current_time_seconds = 0.0;
        if (!waitForNF2FFFiles(csumo_settings.value(), current_time_seconds))
        {
            std::println(stderr, "Error: Timeout on waiting for NF2FF data for time = {} seconds",
                         current_time_seconds);
            return -1;
        }
        const auto initial_nf2ff_readers = readNF2FFFiles(csumo_settings.value(), current_time_seconds);
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

        ConnectedSinkSources initial_connected_sink_sources = initial_result.value();

        // Set sources_sinks mesh
        constexpr std::string_view sources_sinks_mesh = "sources_sinks_nodes";
        SourcesSinks sources_sinks;
        sources_sinks.setVertexCount(initial_connected_sink_sources.get_number_of_entries());
        participant.setMeshVertices(sources_sinks_mesh, sources_sinks.coordinates, sources_sinks.precice_ids);
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

            receiveFFData(participant, csumo_2d_mesh, csumo_3d_mesh, coupling_time_step);
            const auto ff2nf_result =
                writeFF2NFFiles(csumo_settings.value(), csumo_2d_mesh, csumo_3d_mesh, current_time_seconds);
            if (!ff2nf_result.has_value())
            {
                std::println(stderr, "Error: Unable to write FF2NF data at time {} s: {}", current_time_seconds,
                             ff2nf_result.error().message);
                return -1;
            }
            if (!waitForNF2FFFiles(csumo_settings.value(), current_time_seconds))
            {
                std::println(stderr, "Error: Timeout on waiting for NF2FF data for time = {} seconds",
                             current_time_seconds);
                return -1;
            }
            const auto nf2ff_readers = readNF2FFFiles(csumo_settings.value(), current_time_seconds);
            if (!nf2ff_readers.has_value())
            {
                std::println(stderr, "Error: Unable to read NF2FF data: {}", nf2ff_readers.error().message);
                return -1;
            }
            auto conversion_result = convertCSumoToPrecice(csumo_settings->diffusers, *nf2ff_readers, csumo_3d_mesh);
            if (!conversion_result.has_value())
            {
                std::println(stderr, "Unable to convert NF to ConnectedSinkSources: {}",
                             conversion_result.error().message);
                return -1;
            }
            ConnectedSinkSources connected_sink_sources = conversion_result.value();

            if (participant.isTimeWindowComplete())
            {
                participant.resetMesh(sources_sinks_mesh);
                sources_sinks.setVertexCount(connected_sink_sources.get_number_of_entries());
                participant.setMeshVertices(sources_sinks_mesh, sources_sinks.coordinates, sources_sinks.precice_ids);
                std::println("Reset Sources_Sinks. PreciceMesh set to {} vertices.",
                             connected_sink_sources.get_number_of_entries());
            }
            else
            {
                std::println("Skipped mesh reset. Want to write {} entries, mesh has {} vertices.",
                             connected_sink_sources.get_number_of_entries(), sources_sinks.precice_ids.size());
            }

            auto write_result = writeConnectedSinkSources(participant, "sources_sinks_nodes", sources_sinks.precice_ids,
                                                          connected_sink_sources);

            if (!write_result.has_value())
            {
                std::println(stderr, "Error: Unable to write sources/sinks data at time {} s: {}", current_time_seconds,
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
