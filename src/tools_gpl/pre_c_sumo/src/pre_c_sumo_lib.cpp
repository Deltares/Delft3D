#include "pre_c_sumo_lib.hpp"

#include <precice/precice.hpp>
#include <print>
#include <string_view>
#include <vector>
#include <map>

#include "csumo_settings_reader.hpp"
#include "coupling_steps.hpp"

namespace
{
    using namespace pre_c_sumo;
    /**
     * @details Construct forward mapping from settings to data node indices.
     * 3D data of all subgrid models (each subgrid model is related to one diffuser) is stored in a 1D vector:
     *     diffuser 1 (z=0), diffuser 1 (z=1), ..., diffuser 1 (z=n),
     *     intake 1 (z=0), intake 1 (z=1), ..., intake 1 (z=n),               // optional
     *     ambient 1a (z=0), ambient 1a (z=1), ..., ambient 1a (z=n),
     *     ambient 1b (z=0), ambient 1b (z=1), ..., ambient 1b (z=n),
     *     ...
     *     diffuser 2 (z=0), diffuser 2 (z=1), ..., diffuser 2 (z=n),
     *     intake 2 (z=0), intake 2 (z=1), ..., intake 2 (z=n),               // optional
     *     ambient 2a (z=0), ambient 2a (z=1), ..., ambient 2a (z=n),
     *     ambient 2b (z=0), ambient 2b (z=1), ..., ambient 2b (z=n),
     *     ...
     *
     *  2D: number_of_zcoordinates = 1
     */
    DiffuserMapping makeDiffuserMapping(const DiffuserSettings& diffuser_setting, const std::size_t diffuser_index,
                                        const std::size_t number_of_zcoordinates)
    {
        const bool has_intake = diffuser_setting.intake.has_value();
        const std::size_t intake_index = has_intake ? diffuser_index + number_of_zcoordinates : 0;
        const std::size_t number_of_ambient_points = diffuser_setting.ambient_positions.size() * number_of_zcoordinates;
        const std::size_t first_ambient_point_index =
            has_intake ? diffuser_index + 2 * number_of_zcoordinates : diffuser_index + number_of_zcoordinates;

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
    Mesh getMesh2D3D(const std::string_view csumo_mesh_name, const CSumoSettingsReader& csumo_settings,
                     const ZSpecification& z_spec)
    {
        const std::size_t dimensions = z_spec.numberOfDimensions();
        Mesh mesh = {};
        mesh.name = csumo_mesh_name;
        mesh.number_of_zcoordinates = z_spec.numberOfZCoordinates();
        for (const DiffuserSettings& diffuser : csumo_settings.diffusers())
        {
            const std::size_t diffuser_index_mapping = mesh.coordinates.size() / dimensions;
            for (std::size_t i = 0; i < mesh.number_of_zcoordinates; ++i)
            {
                mesh.coordinates.emplace_back(diffuser.position.x_coordinate); // diffuser position x
                mesh.coordinates.emplace_back(diffuser.position.y_coordinate); // diffuser position y
                if (!z_spec.is_2d)
                {
                    mesh.coordinates.emplace_back(z_spec.zCoordinateAt(i)); // z coordinate
                }
            }

            if (diffuser.intake.has_value()) // (optional intake)
            {
                for (std::size_t i = 0; i < mesh.number_of_zcoordinates; ++i)
                {
                    mesh.coordinates.emplace_back(diffuser.intake.value().x_coordinate); // intake point x
                    mesh.coordinates.emplace_back(diffuser.intake.value().y_coordinate); // intake point y
                    if (!z_spec.is_2d)
                    {
                        mesh.coordinates.emplace_back(z_spec.zCoordinateAt(i)); // z coordinate
                    }
                }
            }

            for (const parsing_utils::Point2D& position : diffuser.ambient_positions)
            {
                for (std::size_t i = 0; i < mesh.number_of_zcoordinates; ++i)
                {
                    mesh.coordinates.emplace_back(position.x_coordinate);
                    mesh.coordinates.emplace_back(position.y_coordinate);
                    if (!z_spec.is_2d)
                    {
                        mesh.coordinates.emplace_back(z_spec.zCoordinateAt(i)); // z coordinate
                    }
                }
            }

            mesh.forward_map.emplace_back(
                makeDiffuserMapping(diffuser, diffuser_index_mapping, mesh.number_of_zcoordinates));
        }

        mesh.number_of_nodes = mesh.coordinates.size() / dimensions;
        mesh.vertex_ids.resize(mesh.number_of_nodes);

        return mesh;
    }
} // namespace

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
        Mesh csumo_2d_mesh = getMesh2D3D("csumo_2d_nodes", csumo_settings.value(), ZSpecification{true, 0, 0, 0});
        participant.setMeshVertices(csumo_2d_mesh.name, csumo_2d_mesh.coordinates, csumo_2d_mesh.vertex_ids);
        // Add preCICE quantity data buffers.
        csumo_2d_mesh.quantities[water_levels_id] = std::vector<double>(csumo_2d_mesh.number_of_nodes);
        csumo_2d_mesh.quantities[bed_levels_id] = std::vector<double>(csumo_2d_mesh.number_of_nodes);
        csumo_2d_mesh.quantities[water_depth_id] = std::vector<double>(csumo_2d_mesh.number_of_nodes);

        // Construct 3d mesh
        Mesh csumo_3d_mesh =
            getMesh2D3D("csumo_3d_nodes", csumo_settings.value(), ZSpecification{false, -11.0, 3.0, 1.0});
        participant.setMeshVertices(csumo_3d_mesh.name, csumo_3d_mesh.coordinates, csumo_3d_mesh.vertex_ids);
        // Add preCICE quantity data buffers.
        csumo_3d_mesh.quantities[densities_id] = std::vector<double>(csumo_3d_mesh.number_of_nodes);

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
        double current_time_seconds = 0.0;
        while (participant.isCouplingOngoing())
        {
            double coupling_time_step = participant.getMaxTimeStepSize();

            receiveFFData(participant, csumo_2d_mesh, csumo_3d_mesh, coupling_time_step);
            writeFF2NFFiles(csumo_settings.value(), csumo_2d_mesh, csumo_3d_mesh, current_time_seconds);
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
