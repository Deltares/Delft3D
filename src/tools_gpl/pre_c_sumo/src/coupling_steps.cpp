#include "coupling_steps.hpp"

#include <precice/precice.hpp>
#include <algorithm>
#include <print>
#include <ranges>
#include <string_view>
#include <vector>
#include <filesystem>
#include <thread>
#include <chrono>
#include <numbers> // for std::numbers::pi
#include <cmath>   // for atan2,sin,cos

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
                    std::println(stderr, "Error reading NF2FF file {}: {}", nf2ff_filepath.string(),
                                 reader.error().message);
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

    /**
     * @brief Convert NF2FF output into connected source/sink entries.
     *
     * For each diffuser this constructs sink-source pairs based on sinks after the first
     * sink point, and optionally intake-related pairs when intake is configured.
     *
     * @param csumoSettings Parsed C-SUMO settings.
     * @param nf2ff_readers NF2FF snapshots for the current coupling time.
     * @return Connected source/sink data ready to write via preCICE.
     */
    ConnectedSinkSources convertNFtoConnectedSinkSources(const CSumoSettingsReader& csumoSettings,
                                                         const std::vector<NF2FFReader>& nf2ff_readers)
    {
        (void)csumoSettings;
        ConnectedSinkSources connectedsinksources{};

        for (std::size_t diffuser_index = 0; diffuser_index < nf2ff_readers.size(); diffuser_index++)
        {
            const auto& diffuser = nf2ff_readers[diffuser_index];
            std::vector<SourceOrSinkData> sources;
            const bool single_nf2ff_source = diffuser.sources().size() == 1;

            // Normalize the source list once: non-modelled diffusers expand a single NF2FF source
            // into a generated DESA track, so all downstream loops must use this converted list
            // instead of the raw diffuser.sources() snapshot.
            if (!isDiffuserModelled(diffuser))
            {
                sources = createDiffuserModel(diffuser);
            }
            else
            {
                sources = diffuser.sources();
            }

            // Send (created) diffuser
            double source_weight_norm = 0.0;
            for (const auto& source : sources)
            {
                source_weight_norm += source.has_weight ? source.weight : 1.0;
            }
            source_weight_norm = std::max(source_weight_norm, 1.0);

            const auto sinks = diffuser.sinks();
            // Match nearfield entrainment behavior: use sink deltas, so the first sink does
            // not create entrainment discharge by itself.
            for (std::size_t sink_index = 1; sink_index < sinks.size(); sink_index++)
            {
                double delta_s = sinks[sink_index].entrainment - sinks[sink_index - 1].entrainment;
                const double source_flow_rate = diffuser.sourceFlowRate();
                const auto& sink = sinks[sink_index];
                double sink_z_top = -sink.z_coordinate + sink.half_plume_height;
                double sink_z_bottom = -sink.z_coordinate - sink.half_plume_height;

                for (const auto& source : sources)
                {
                    double discharge =
                        delta_s * source_flow_rate * (source.has_weight ? source.weight : 1.0) / source_weight_norm;
                    double source_z_top =
                        single_nf2ff_source ? (-source.z_coordinate + source.half_plume_height) : -source.z_coordinate;
                    double source_z_bottom =
                        single_nf2ff_source ? (-source.z_coordinate - source.half_plume_height) : -source.z_coordinate;
                    double source_moment_magnitude = source.has_u ? source.u_magnitude : 0.0;
                    double source_moment_direction = source.has_u ? source.u_direction : 0.0;
                    connectedsinksources.add_entry(sink.x_coordinate, sink.y_coordinate, sink_z_bottom, sink_z_top,
                                                   source.x_coordinate, source.y_coordinate, source_z_bottom,
                                                   source_z_top, discharge, source_moment_magnitude,
                                                   source_moment_direction);
                }
            }

            // Match nearfield dischargeToSrc behavior: add explicit source discharge
            // terms independent of entrainment sink deltas.
            if (!sources.empty())
            {
                const double source_flow_rate = diffuser.sourceFlowRate();
                for (const auto& source : sources)
                {
                    double discharge =
                        source_flow_rate * (source.has_weight ? source.weight : 1.0) / source_weight_norm;
                    double source_z_top =
                        single_nf2ff_source ? (-source.z_coordinate + source.half_plume_height) : -source.z_coordinate;
                    double source_z_bottom =
                        single_nf2ff_source ? (-source.z_coordinate - source.half_plume_height) : -source.z_coordinate;
                    double source_moment_magnitude = source.has_u ? source.u_magnitude : 0.0;
                    double source_moment_direction = source.has_u ? source.u_direction : 0.0;
                    connectedsinksources.add_entry(0.0, 0.0, 0.0, 0.0, source.x_coordinate, source.y_coordinate,
                                                   source_z_bottom, source_z_top, discharge, source_moment_magnitude,
                                                   source_moment_direction);
                }
            }

            // Intake
            const auto intakes = diffuser.intakes();
            if (!intakes.empty())
            {
                const double intake_flow_rate = diffuser.intakeFlowRate();
                double intake_weight_norm = 0.0;
                for (const auto& intake : intakes)
                {
                    intake_weight_norm += intake.has_weight ? intake.weight : 1.0;
                }
                intake_weight_norm = std::max(intake_weight_norm, 1.0);

                // Intakes are sink-only terms (not connected to source points).
                for (const auto& intake : intakes)
                {
                    const double intake_discharge =
                        intake_flow_rate * (intake.has_weight ? intake.weight : 1.0) / intake_weight_norm;
                    connectedsinksources.add_entry(intake.x_coordinate, intake.y_coordinate, -intake.z_coordinate,
                                                   -intake.z_coordinate, 0.0, 0.0, 0.0, 0.0, intake_discharge, 0.0,
                                                   0.0);
                }
            }
        }
        std::println("connectedsinksources size = {}", connectedsinksources.size());
        return connectedsinksources;
    }

    bool isDiffuserModelled(const NF2FFReader& diffuser)
    {
        // (Placeholder) logic to determine if the diffuser is modelled
        return diffuser.sources().size() > 1 || diffuser.sinks().size() == 0;
    }

    // Do we still need this?
    void processSourceLocations() { std::println("Processing source locations..."); }

    // Determine the flow nodes over which to distribute the diluted discharge:
    // Both sink and source point needed for direction connection line.
    // Define the line through the source point, perpendicular to the line connecting the
    // last sink point with the source point. Define the line piece on this line, using
    // the specified source-width. Walk with 1000 steps over this line piece
    std::vector<SourceOrSinkData> createDiffuserModel(const NF2FFReader& diffuser)
    {
        std::println("Creating diffuser model...");
        std::vector<SourceOrSinkData> new_sources;
        constexpr int num_steps = 1000;
        new_sources.reserve(num_steps);

        const auto& sources = diffuser.sources();
        const auto& sinks = diffuser.sinks();

        assert(sources.size() == 1);
        assert(sinks.size() > 0);

        double ang_end = atan2(sources[0].y_coordinate - sinks[sinks.size() - 1].y_coordinate,
                               sources[0].x_coordinate - sinks[sinks.size() - 1].x_coordinate);
        double x_range = sources[0].half_plume_width * cos(std::numbers::pi / 2 - ang_end);
        double y_range = sources[0].half_plume_width * sin(std::numbers::pi / 2 - ang_end);
        double x_start = sources[0].x_coordinate - x_range;
        double y_start = sources[0].y_coordinate - y_range;
        double dx = 2.0 * x_range / (num_steps - 1);
        double dy = 2.0 * y_range / (num_steps - 1);

        for (int i = 0; i < num_steps; i++)
        {
            new_sources.emplace_back(SourceOrSinkData{.x_coordinate = x_start + i * dx,
                                                      .y_coordinate = y_start + i * dy,
                                                      .z_coordinate = sources[0].z_coordinate,
                                                      .entrainment = sources[0].entrainment,
                                                      .half_plume_height = sources[0].half_plume_height,
                                                      .half_plume_width = 0,
                                                      .u_magnitude = sources[0].u_magnitude,
                                                      .u_direction = sources[0].u_direction,
                                                      .weight = 1.0 / num_steps,
                                                      .has_u = sources[0].has_u,
                                                      .has_weight = true});
        }

        return new_sources;
    }

} // namespace pre_c_sumo
