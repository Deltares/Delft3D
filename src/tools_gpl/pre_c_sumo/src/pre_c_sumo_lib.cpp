#include "pre_c_sumo_lib.hpp"

#include <precice/precice.hpp>
#include <print>
#include <string_view>
#include <vector>
#include <map>

#include "csumo_settings_reader.hpp"
#include "coupling_steps.hpp"

namespace pre_c_sumo
{
    /**
     * @details Construct forward mapping from settings to data node indices.
     */
    DiffuserMapping makeDiffuserMapping(const DiffuserSettings& diffuser_setting, const std::size_t diffuser_index)
    {
        const bool has_intake = diffuser_setting.intake.has_value();
        const std::size_t intake_index = has_intake ? diffuser_index + 1 : 0;
        const std::size_t number_of_ambient_points = diffuser_setting.ambient_positions.size();
        const std::size_t first_ambient_point_index = has_intake ? diffuser_index + 2 : diffuser_index + 1;

        const DiffuserMapping diffuser_mapping = {.diffuser_index = diffuser_index,
                                                  .has_intake = has_intake,
                                                  .intake_index = intake_index,
                                                  .number_of_ambient_points = number_of_ambient_points,
                                                  .first_ambient_point_index = first_ambient_point_index};
        return diffuser_mapping;
    }

    /**
     * @details Construct both preCICE 2d mesh coordinates and forward mappings from settings.
     * The latter allows us to find the index of values belonging to diffusers, intakes and ambient points
     * in preCICE communication buffers in O(1) time.
     */
    Mesh getMesh2d(const std::string_view csumo_mesh_name, const CSumoSettingsReader& csumo_settings)
    {
        const int dimensions = 2;
        Mesh mesh = {};
        mesh.name = csumo_mesh_name;
        for (const DiffuserSettings& d : csumo_settings.diffusers())
        {
            const std::size_t diffuser_index = mesh.coordinates.size() / dimensions;
            mesh.coordinates.emplace_back(d.position.x_coordinate); // diffuser position x
            mesh.coordinates.emplace_back(d.position.y_coordinate); // diffuser position y

            if (d.intake.has_value()) // (optional intake)
            {
                mesh.coordinates.emplace_back(d.intake.value().x_coordinate); // intake point x
                mesh.coordinates.emplace_back(d.intake.value().y_coordinate); // intake point y
            }

            for (const parsing_utils::Point2D& p : d.ambient_positions)
            {
                mesh.coordinates.emplace_back(p.x_coordinate);
                mesh.coordinates.emplace_back(p.y_coordinate);
            }

            mesh.forward_map.emplace_back(makeDiffuserMapping(d, diffuser_index));
        }

        mesh.number_of_nodes = mesh.coordinates.size() / dimensions;
        mesh.vertex_ids.resize(mesh.number_of_nodes);

        return mesh;
    }

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

        Mesh csumo_2d_mesh = getMesh2d("csumo_2d_nodes", csumo_settings.value());
        participant.setMeshVertices(csumo_2d_mesh.name, csumo_2d_mesh.coordinates, csumo_2d_mesh.vertex_ids);

        // Add preCICE quantity data buffers.
        csumo_2d_mesh.quantities[water_levels_id] = std::vector<double>(csumo_2d_mesh.number_of_nodes);
        csumo_2d_mesh.quantities[bed_levels_id] = std::vector<double>(csumo_2d_mesh.number_of_nodes);
        csumo_2d_mesh.quantities[water_depth_id] = std::vector<double>(csumo_2d_mesh.number_of_nodes);

        // TESTDATA: set sources_sinks mesh
        constexpr int sources_sinks_size = 4;
        // constexpr int dim = 2;
        std::vector<double> sources_sinks_nodes = {250.000,  350.087, 252.500,  350.048,
                                                   1050.000, 350.365, 1050.500, 350.365};
        std::vector<int> sources_sinks_nodes_ids(sources_sinks_size);
        participant.setMeshVertices("sources_sinks_nodes", sources_sinks_nodes, sources_sinks_nodes_ids);

        // TESTDATA: set sources_sinks data
        // constexpr int sources_sinks_data_size = 1; // discharge
        std::vector<double> sources_sinks = {1.23, 4.56, -1.23, -4.56};
        participant.writeData("sources_sinks_nodes", "sources_sinks", sources_sinks_nodes_ids, sources_sinks);

        participant.initialize();
        double coupling_time_step;
        double current_time_seconds = 0.0;
        while (participant.isCouplingOngoing())
        {
            coupling_time_step = participant.getMaxTimeStepSize();

            receiveFFData(participant, csumo_2d_mesh, coupling_time_step);
            writeFF2NFFiles(csumo_settings.value(), csumo_2d_mesh, current_time_seconds);
            waitForNF2FFFiles(csumo_settings.value());
            readNF2FFFiles(csumo_settings.value());
            convertNFToSourcesSinks(csumo_settings.value());

            sendSourcesSinksToFF(csumo_settings.value());

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
