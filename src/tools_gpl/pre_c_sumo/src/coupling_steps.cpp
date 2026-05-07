#include "coupling_steps.hpp"

#include <precice/precice.hpp>
#include <format>
#include <print>
#include <ranges>
#include <string_view>
#include <vector>

#include "csumo_settings_reader.hpp"
#include "FF2NF_writer.hpp"
#include "parsing_types.hpp"

namespace pre_c_sumo
{
    FarFieldPoint2D makePoint(std::size_t index_2d, std::size_t index_3d, Mesh& mesh_2d, Mesh& mesh_3d)
    {
        std::vector<FarFieldLayer> layers;
        for (size_t i = 0; i < mesh_3d.number_of_zcoordinates; ++i)
        {
            layers.emplace_back(FarFieldLayer{
                // 3d.coordinates = (x1, y1, z1, x2, y2, z2, ...): skip "index_3d + i" points, then skip x and y
                .z_coordinate = mesh_3d.coordinates[(index_3d + i) * 3 + 2],
                .x_velocity = 0.0, // TODO: obtain from far-field data
                .y_velocity = 0.0, // TODO: obtain from far-field data
                .density = mesh_3d.quantities[densities_id][index_3d + i],
                .constituents = {0.0, 0.0, 0.0}, // constituents, // TODO: obtain layered data from far-field
            });
        }
        return FarFieldPoint2D{
            // 2d.coordinates = (x1, y1, x2, y2, ...)
            .position = {mesh_2d.coordinates[index_2d * 2], mesh_2d.coordinates[index_2d * 2 + 1]},
            .water_depth = mesh_2d.quantities[water_levels_id][index_2d] + mesh_2d.quantities[bed_levels_id][index_2d],
            .layers = layers,
        };
    }

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
        auto csumo_settings = std::move(expectedCsumoSettings).value();
        std::println("Successfully parsed C-SUMO configuration file version: {}", csumo_settings.fileVersion());
        return csumo_settings;
    }

    void receiveFFData(precice::Participant& participant, Mesh& csumo_2d_mesh, Mesh& csumo_3d_mesh,
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

    void writeFF2NFFiles(const CSumoSettingsReader& csumo_settings, Mesh& csumo_2d_mesh, Mesh& csumo_3d_mesh,
                         double current_time_seconds)
    {
        // TODO: obtain these from the far-field model / coupling state
        const std::string run_id = "FlowFM";
        const std::vector<std::string> constituent_names = {"temperature", "salinity",
                                                            "tracer"}; // TODO: derive from settings

        for (const auto& [index, diffuser] : csumo_settings.diffusers() | std::views::enumerate)
        {
            const auto subgrid_model_nr = static_cast<int>(index + 1);
            const auto mapping_index = static_cast<std::size_t>(index);
            DiffuserMapping& mapping = csumo_2d_mesh.forward_map[mapping_index];

            // Lambda function to obtain the value of a 2D quantity for an ambient point, given the quantity name and
            // the ambient point index (0-based). 3D is handled by the makePoint function, which reads the layered data
            // for all z-coordinates of the point.
            auto get_ambient_value = [&quantities = csumo_2d_mesh.quantities, &m = mapping](
                                         const std::string_view& name, const std::size_t& ambient_point_index) {
                return quantities[name][m.first_ambient_point_index + ambient_point_index];
            };

            // Idem: Lambda function for the diffuser
            auto get_diffuser_value = [&quantities = csumo_2d_mesh.quantities, &m = mapping](
                                          const std::string_view& name) { return quantities[name][m.diffuser_index]; };

            // Idem: Lambda function for the intake (if present)
            auto get_intake_value = [&quantities = csumo_2d_mesh.quantities,
                                     &m = mapping](const std::string_view& name) {
                return m.has_intake ? quantities[name][m.intake_index] : 0.0;
            };

            // Collect all data for the ambient points
            std::vector<FarFieldPoint2D> ambient_points{};
            for (const auto& [position_index, ambient_point] : diffuser.ambient_positions | std::views::enumerate)
            {
                ambient_points.emplace_back(makePoint(
                    position_index + mapping.first_ambient_point_index,
                    (position_index + mapping.first_ambient_point_index) * csumo_3d_mesh.number_of_zcoordinates,
                    csumo_2d_mesh, csumo_3d_mesh));
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
                .diffuser = makePoint(0, 0, csumo_2d_mesh, csumo_3d_mesh),
                .intake = diffuser.intake.has_value() ? std::optional{makePoint(1, csumo_3d_mesh.number_of_zcoordinates,
                                                                                csumo_2d_mesh, csumo_3d_mesh)}
                                                      : std::nullopt,
                .ambient_points = ambient_points,
                .settings_xml_node = diffuser.settings_xml_node,
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
