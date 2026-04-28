#include "coupling_steps.hpp"

#include <precice/precice.hpp>
#include <algorithm>
#include <format>
#include <print>
#include <ranges>
#include <string_view>
#include <vector>

#include "csumo_mesh_layout.hpp"
#include "csumo_settings_reader.hpp"
#include "FF2NF_writer.hpp"
#include "parsing_types.hpp"

namespace pre_c_sumo
{

    namespace
    {
        /**
         * @brief Decode one 2D coupling point's vertical layer stack from flat preCICE buffers
         *        into a `FarFieldPoint2D` ready for the FF2NF writer.
         *
         * The preCICE 3D mesh stores all points of all diffusers in a single flat buffer,
         * packed as: point 0 layers 0..N-1, point 1 layers 0..N-1, ...
         * This function extracts the slice belonging to `point_index` and converts it.
         *
         * Index arithmetic:
         * @code
         *   global_vertex = point_index * layers_per_point + layer
         *   z   = csumo_3d_coordinates[global_vertex * 3 + 2]
         *   rho = rho_3d[global_vertex]
         * @endcode
         *
         * @param position          X,Y position of this coupling point (from C-SUMO settings).
         * @param csumo_3d_coordinates Flat XYZ buffer of all 3D mesh vertices [x0,y0,z0, x1,y1,z1, ...].
         * @param rho_3d            Flat density buffer, one value per 3D vertex.
         * @param point_index       Zero-based index of this 2D point (its `flat_index` from `CsumoMeshLayout`).
         * @param layers_per_point  Number of vertical layers per 2D point (`PreCICEState::current_3d_layer_count`).
         * @param constituents      Constituent values (e.g. temperature) attached to every layer.
         *
         * @return `FarFieldPoint2D` with:
         *   - `position` set to the provided x,y.
         *   - one `FarFieldLayer` per vertical level carrying z, density, zeroed velocities, and constituents.
         *   - `water_depth` = z_max - z_min across all layers.
         *
         * @note If `layers_per_point <= 0`, returns a single placeholder layer at z=0, rho=1000.
         * @note Out-of-bounds buffer accesses fall back to z=0 or rho=1000 rather than crashing.
         */
        FarFieldPoint2D makePoint2DFrom3D(const parsing_utils::Point2D& position,
                                          const std::vector<double>& csumo_3d_coordinates,
                                          const std::vector<double>& rho_3d, const int point_index,
                                          const int layers_per_point, const std::vector<double>& constituents)
        {
            FarFieldPoint2D point{};
            point.position = position;

            if (layers_per_point <= 0)
            {
                point.layers.push_back(FarFieldLayer{
                    .z_coordinate = 0.0,
                    .x_velocity = 0.0,
                    .y_velocity = 0.0,
                    .density = 1000.0,
                    .constituents = constituents,
                });
                point.water_depth = 0.0;
                return point;
            }

            point.layers.reserve(static_cast<std::size_t>(layers_per_point));
            double z_min = 0.0;
            double z_max = 0.0;

            for (int layer = 0; layer < layers_per_point; ++layer)
            {
                const auto global_index = (point_index * layers_per_point) + layer;
                const auto global_index_size = static_cast<std::size_t>(global_index);
                const auto coord_index = global_index_size * 3;
                const double z =
                    (coord_index + 2) < csumo_3d_coordinates.size() ? csumo_3d_coordinates[coord_index + 2] : 0.0;
                const double rho = global_index_size < rho_3d.size() ? rho_3d[global_index_size] : 1000.0;

                if (layer == 0)
                {
                    z_min = z;
                    z_max = z;
                }
                else
                {
                    z_min = std::min(z_min, z);
                    z_max = std::max(z_max, z);
                }

                point.layers.push_back(FarFieldLayer{
                    .z_coordinate = z,
                    .x_velocity = 0.0,
                    .y_velocity = 0.0,
                    .density = rho,
                    .constituents = constituents,
                });
            }
            point.water_depth = z_max - z_min;
            return point;
        }
    } // namespace

    std::expected<pre_c_sumo::CSumoSettingsReader, parsing_utils::ParseError> readCsumoSettingsFile(
        const std::string_view csumo_settings_file_name)
    {
        std::println("Reading C-SUMO configuration file...");
        auto expectedCsumoSettings = pre_c_sumo::CSumoSettingsReader::fromFile(csumo_settings_file_name);

        if (!expectedCsumoSettings.has_value())
        {
            std::println(stderr, "Error parsing C-SUMO configuration: {}", expectedCsumoSettings.error().message);
            return expectedCsumoSettings;
        }
        const auto csumo_settings = std::move(expectedCsumoSettings).value();
        std::println("Successfully parsed C-SUMO configuration file version: {}", csumo_settings.fileVersion());
        return csumo_settings;
    }

    void receiveFFData(PreCICEState& precice_state)
    {
        std::println("Receiving far-field data...");
        if (precice_state.csumo_3d_node_ids.empty())
        {
            std::println("CSUMO 3D mesh is empty");
            return;
        }

        const auto dt = precice_state.participant->getMaxTimeStepSize();
        precice_state.participant->readData(PreCICEState::csumo_3d_nodes_name, PreCICEState::rho_name,
                                            precice_state.csumo_3d_node_ids, dt, precice_state.rho_3d);
    }

    void writeFF2NFFiles(const CSumoSettingsReader& csumo_settings, const PreCICEState& precice_state,
                         const CsumoMeshLayout& mesh_layout)
    {
        const double current_time_seconds = 0.0;
        const std::string run_id = "FlowFM";
        const std::vector<std::string> constituent_names = {"temperature"}; // TODO: derive from settings
        const int layers_per_point = precice_state.current_3d_layer_count;

        for (const auto& [index, diffuser] : csumo_settings.diffusers() | std::views::enumerate)
        {
            const int diffuser_idx = static_cast<int>(index);
            const auto subgrid_model_nr = diffuser_idx + 1;

            // Look up each role's flat_index from the layout — safe regardless of intake presence
            // or varying ambient counts between diffusers.

            // Based on example for build2DMeshPointsFromSettings
            // For diffuser_idx=1, role=Ambient this filters
            // layout.points down to just {flat_index=5, di=1, Ambient, 0}.
            auto points_for = [&](CsumoMeshLayout::PointRole role) {
                return mesh_layout.points | std::views::filter([diffuser_idx, role](const auto& p) {
                           return p.diffuser_index == diffuser_idx && p.role == role;
                       });
            };

            auto diffuser_range = points_for(CsumoMeshLayout::PointRole::Diffuser);
            const auto diffuser_point = makePoint2DFrom3D(diffuser.position, precice_state.csumo_3d_coordinates,
                                                          precice_state.rho_3d, diffuser_range.begin()->flat_index,
                                                          layers_per_point, diffuser.discharge.constituents);

            std::optional<FarFieldPoint2D> intake_point = std::nullopt;
            auto intake_range = points_for(CsumoMeshLayout::PointRole::Intake);
            if (auto it = intake_range.begin(); it != intake_range.end())
            {
                intake_point =
                    makePoint2DFrom3D(*diffuser.intake, precice_state.csumo_3d_coordinates, precice_state.rho_3d,
                                      it->flat_index, layers_per_point, diffuser.discharge.constituents);
            }

            std::vector<FarFieldPoint2D> ambient_points;
            ambient_points.reserve(diffuser.ambient_positions.size());
            for (const auto& ambient_info : points_for(CsumoMeshLayout::PointRole::Ambient))
            {
                ambient_points.push_back(
                    makePoint2DFrom3D(diffuser.ambient_positions[static_cast<std::size_t>(ambient_info.ambient_index)],
                                      precice_state.csumo_3d_coordinates, precice_state.rho_3d, ambient_info.flat_index,
                                      layers_per_point, diffuser.discharge.constituents));
            }

            const auto ff2nf_filename = diffuser.ff2nf_dir / std::format("FF2NF__{}_SubMod{:03d}_{:.3f}.xml", run_id,
                                                                         subgrid_model_nr, current_time_seconds / 60.0);

            const auto nf2ff_wait_file = diffuser.nf2ff_file.value_or("");

            auto ff2nf_config = FF2NFConfig{
                .ff2nf_filename = ff2nf_filename.string(),
                .wait_for_file = nf2ff_wait_file,
                .ff_run_directory = diffuser.ff_run_dir.string(),
                .run_id = run_id,
                .unique_id = "", // Do not use unique ID, run C-SUMO in different directories for now
                .subgrid_model_nr = subgrid_model_nr,
                .current_time_seconds = current_time_seconds,
                .constituent_names = constituent_names,
                .diffuser = diffuser_point,
                .intake = intake_point,
                .ambient_points = ambient_points,
            };

            const auto result = FF2NFWriter(std::move(ff2nf_config)).toFile(ff2nf_filename);
            if (!result.has_value())
            {
                std::println(stderr, "Error writing FF2NF file: {}", result.error().message);
                continue;
            }
            std::println("Wrote FF2NF file: {}", ff2nf_filename.string());
        }
    }

    void waitForNF2FFFiles(const CSumoSettingsReader& csumo_settings)
    {
        for (const auto& diffuser : csumo_settings.diffusers())
        {
            if (diffuser.nf2ff_file.has_value())
            {
                std::println("Waiting for NF2FF file: {}", diffuser.nf2ff_file.value());
                // Here you would add the actual logic to wait for the NF2FF files to be available
            }
        }
    }

    void readNF2FFFiles(const CSumoSettingsReader& csumo_settings)
    {
        for (const auto& diffuser : csumo_settings.diffusers())
        {
            if (diffuser.nf2ff_file.has_value())
            {
                std::println("Reading NF2FF file: {}", diffuser.nf2ff_file.value());
                // Here you would add the actual logic to read the NF2FF files and extract the necessary data
            }
        }
    }

    void convertNFToSourcesSinks(const CSumoSettingsReader& csumo_settings)
    {
        for (const auto& diffuser : csumo_settings.diffusers())
        {
            std::println("Converting NF data to sources/sinks for diffuser {} ...", diffuser.nf2ff_file.value());
            convertNFSinksToFF();
            convertNFIntakesToFF();
            convertNFSourcesToFF();
        }
    }

    void sendSourcesSinksToFF(const CSumoSettingsReader& csumo_settings)
    {
        std::println("Sending sources/sinks data to far-field...");
        (void)csumo_settings;
    }

    void convertNFSinksToFF() { std::println("Processing sinks..."); }

    void convertNFIntakesToFF() { std::println("Processing intakes..."); }

    void convertNFSourcesToFF()
    {
        if (isDiffuserModelled())
        {
            processSourceLocations();
        }
        else
        {
            createDiffuserModel();
        }
    }

    bool isDiffuserModelled()
    {
        // Placeholder logic to determine if the diffuser is modelled
        return true; // Assume it's modelled for demonstration
    }

    void processSourceLocations() { std::println("Processing source locations..."); }

    void createDiffuserModel() { std::println("Creating diffuser model..."); }

} // namespace pre_c_sumo
