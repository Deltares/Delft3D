#include "coupling_steps.hpp"

#include <precice/precice.hpp>
#include <format>
#include <print>
#include <ranges>
#include <string_view>
#include <vector>
#include <filesystem>
#include <thread>
#include <chrono>

#include "csumo_settings_reader.hpp"
#include "pre_c_sumo_lib.hpp"
#include "FF2NF_writer.hpp"
#include "NF2FF_reader.hpp"
#include "parsing_types.hpp"
#include "monadic_utils.hpp"

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

            //// Lambda function to obtain the value of a 2D quantity for an ambient point, given the quantity name and
            //// the ambient point index (0-based). 3D is handled by the makePoint function, which reads the layered
            // data / for all z-coordinates of the point.
            // auto get_ambient_value = [&quantities = csumo_2d_mesh.quantities, &m = mapping](
            //                              const std::string_view& name, const std::size_t& ambient_point_index) {
            //     return quantities[name][m.first_ambient_point_index + ambient_point_index];
            // };

            //// Idem: Lambda function for the diffuser
            // auto get_diffuser_value = [&quantities = csumo_2d_mesh.quantities, &m = mapping](
            //                               const std::string_view& name) { return quantities[name][m.diffuser_index];
            //                               };

            //// Idem: Lambda function for the intake (if present)
            // auto get_intake_value = [&quantities = csumo_2d_mesh.quantities,
            //                          &m = mapping](const std::string_view& name) {
            //     return m.has_intake ? quantities[name][m.intake_index] : 0.0;
            // };

            // Collect all data for the ambient points
            std::vector<FarFieldPoint2D> ambient_points{};
            for (const auto& [position_index, ambient_point] : diffuser.ambient_positions | std::views::enumerate)
            {
                const std::size_t ambient_index =
                    static_cast<std::size_t>(position_index) + mapping.first_ambient_point_index;
                ambient_points.emplace_back(makePoint(
                    ambient_index, (ambient_index)*csumo_3d_mesh.number_of_zcoordinates, csumo_2d_mesh, csumo_3d_mesh));
            }

            const auto ff2nf_filename = diffuser.ff2nfFilepath(subgrid_model_nr, current_time_seconds);

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

    void waitForNF2FFFiles(const CSumoSettingsReader& csumo_settings, double current_time_seconds)
    {
        for (const auto& file : csumo_settings.nf2ffFilepaths(current_time_seconds))
        {
            std::println("Waiting for NF2FF file: {}", file.string());
            // Wait for the NF2FF file to be available
            // TODO: Might be necessary to check whether writing the file is finished too
            while (!std::filesystem::exists(file))
            {
                // Throttle CPU load.
                std::this_thread::sleep_for(std::chrono::milliseconds(20));
            }
        }
    }

    const std::vector<pre_c_sumo::NF2FFReader> readNF2FFFiles(const CSumoSettingsReader& csumo_settings,
                                                              double current_time_seconds)
    {
        std::vector<NF2FFReader> nf2ff_readers{};

        for (const auto& nf2ff_filepath : csumo_settings.nf2ffFilepaths(current_time_seconds))
        {
            if (std::filesystem::exists(nf2ff_filepath))
            {
                std::println("Reading NF2FF file: {}", nf2ff_filepath.string());
                auto reader = NF2FFReader::fromFile(nf2ff_filepath);
                if (reader.has_value())
                {
                    nf2ff_readers.emplace_back(std::move(reader.value()));
                }
                else
                {
                    // Error?
                }
            }
        }
        return nf2ff_readers;
    }

    void convertNFToSourcesSinks(const CSumoSettingsReader& csumo_settings)
    {
        for (const auto& diffuser : csumo_settings.diffusers())
        {
            std::println("Converting NF data to sources/sinks for diffuser {} ...", diffuser.nf2ff_file.value());
        }
    }

    void sendSourcesSinksToFF(precice::Participant& participant, SourcesSinks& sources_sinks)
    {
        std::println("Sending dummy sources/sinks data to far-field...");
        // TESTDATA: set sources_sinks data
        sources_sinks.clearData();
        // data:
        sources_sinks.addData(252.500, 350.048, -9.95, -9.45, 1050.000, 350.365, -5.0, -5.0,
                              0.20E+02); // sink 2, source 1
        sources_sinks.addData(252.500, 350.048, -9.95, -9.45, 1050.500, 350.365, -5.0, -5.0,
                              0.20E+02); // sink 2, source 2

        sources_sinks.addData(0.0, 0.0, 0.0, 0.0, 1050.000, 350.365, -5.0, -5.0,
                              0.50E+01); // intake fraction to source 1
        sources_sinks.addData(0.0, 0.0, 0.0, 0.0, 1050.500, 350.365, -5.0, -5.0,
                              0.50E+01);                                               // intake fraction to source 2
        sources_sinks.addData(1500.6, 1000.6, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.10E+02); // intake sink
        participant.writeData("sources_sinks_nodes", "sinks_x", sources_sinks.precice_ids, sources_sinks.sinks_x);
        participant.writeData("sources_sinks_nodes", "sinks_y", sources_sinks.precice_ids, sources_sinks.sinks_y);
        participant.writeData("sources_sinks_nodes", "sinks_z_min", sources_sinks.precice_ids,
                              sources_sinks.sinks_z_min);
        participant.writeData("sources_sinks_nodes", "sinks_z_max", sources_sinks.precice_ids,
                              sources_sinks.sinks_z_max);
        participant.writeData("sources_sinks_nodes", "sources_x", sources_sinks.precice_ids, sources_sinks.sources_x);
        participant.writeData("sources_sinks_nodes", "sources_y", sources_sinks.precice_ids, sources_sinks.sources_y);
        participant.writeData("sources_sinks_nodes", "sources_z_min", sources_sinks.precice_ids,
                              sources_sinks.sources_z_min);
        participant.writeData("sources_sinks_nodes", "sources_z_max", sources_sinks.precice_ids,
                              sources_sinks.sources_z_max);
        participant.writeData("sources_sinks_nodes", "sources_sinks_discharge", sources_sinks.precice_ids,
                              sources_sinks.discharges);
    }

    // TODO: Consider if we should make this a member function of ConnectedSinkSources.
    pre_c_sumo::ConnectedSinkSources convertNFtoConnectedSinkSources(const pre_c_sumo::CSumoSettingsReader& settings,
                                                                     const std::vector<NF2FFReader>& readers)
    {
        ConnectedSinkSources connectedsinksources{};

        for (std::size_t diffuser_index = 0; diffuser_index < readers.size(); diffuser_index++)
        {
            const auto& diffuser = readers[diffuser_index];
            const auto& diffuser_setting = settings.diffusers()[diffuser_index];

            if (!isDiffuserModelled(diffuser))
            {
                createDiffuserModel(); // Placeholder call.
            }

            // Send (created) diffuser
            double source_weight_norm = 0.0;
            for (const auto& source : diffuser.sources())
            {
                source_weight_norm += source.has_weight ? source.weight : 1.0;
            }
            if (source_weight_norm <= 0)
            {
                source_weight_norm = 1.0;
            }

            const auto sinks = diffuser.sinks();
            for (std::size_t sink_index = 1; sink_index < sinks.size(); sink_index++)
            {
                double delta_s = sinks[sink_index].entrainment - sinks[sink_index - 1].entrainment;
                const auto& sink = sinks[sink_index];
                double sink_z_top = -sink.z_coordinate + sink.half_plume_height;
                double sink_z_bottom = -sink.z_coordinate - sink.half_plume_height;

                for (const auto& source : diffuser.sources())
                {
                    double discharge = delta_s * source.entrainment * (source.has_weight ? source.weight : 1.0);
                    double source_z_top = -source.z_coordinate + source.half_plume_height;
                    double source_z_bottom = -source.z_coordinate - source.half_plume_height;
                    connectedsinksources.add_entry(sink.x_coordinate, sink.y_coordinate, sink_z_bottom, sink_z_top,
                                                   source.x_coordinate, source.y_coordinate, source_z_bottom,
                                                   source_z_top, discharge, 0.0, 0.0);
                }
            }

            // Intake
            if (diffuser_setting.intake.has_value())
            {
                const auto& intake = diffuser_setting.intake.value();
                const double intake_flow_rate = diffuser.intakeFlowRate();
                for (const auto& source : diffuser.sources())
                {
                    double discharge =
                        intake_flow_rate * (source.has_weight ? source.weight : 1.0) / source_weight_norm;
                    double source_z_top = -source.z_coordinate + source.half_plume_height;
                    double source_z_bottom = -source.z_coordinate - source.half_plume_height;
                    connectedsinksources.add_entry(0.0, 0.0, 0.0, 0.0, source.x_coordinate, source.y_coordinate,
                                                   source_z_bottom, source_z_top, discharge, 0.0, 0.0);
                }
                // Add the intake itself as a sink.
                connectedsinksources.add_entry(intake.x_coordinate, intake.y_coordinate, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
                                               diffuser.intakeFlowRate(), 0.0, 0.0);
            }
        }
        std::println("connectedsinksources size = {}", connectedsinksources.size());
        return connectedsinksources;
    }

    bool isDiffuserModelled(const NF2FFReader& diffuser)
    {
        // (Placeholder) logic to determine if the diffuser is modelled
        return diffuser.sources().size() > 1;
    }

    // Do we still need this?
    void processSourceLocations() { std::println("Processing source locations..."); }

    void createDiffuserModel() { std::println("Creating diffuser model..."); }

} // namespace pre_c_sumo
