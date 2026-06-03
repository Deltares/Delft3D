#include "coupling_steps.hpp"

#include <precice/precice.hpp>
#include <cmath>
#include <format>
#include <numeric>
#include <print>
#include <ranges>
#include <string_view>
#include <vector>

#include "csumo_settings_reader.hpp"
#include "endpoints.hpp"
#include "pre_c_sumo_lib.hpp"
#include "FF2NF_writer.hpp"
#include "parsing_types.hpp"

namespace pre_c_sumo
{
    namespace
    {
        constexpr double zero_connected_id = 0.0;

        int asEndpointId(const double id)
        {
            return static_cast<int>(std::lround(id));
        }

        void appendEndpointRecord(SourcesSinks& sources_sinks, const Endpoint& endpoint)
        {
            sources_sinks.addCoordinates({endpoint.coordinate_x, endpoint.coordinate_y});
            sources_sinks.addData(static_cast<double>(endpoint.id), static_cast<double>(endpoint.connected_id),
                                  endpoint.vertical_boundary_lower, endpoint.vertical_boundary_upper,
                                  endpoint.discharge);
        }

        void maybeAttachMomentum(Source& source, const SourceOrSinkData& source_point)
        {
            if (source_point.has_u)
            {
                addMomentum(source, Momentum{.velocity_magnitude = source_point.u_magnitude,
                                             .velocity_direction_deg = source_point.u_direction});
            }
        }

        void maybeAttachConstituents(Source& source, const NF2FFReader& nf2ff_reader)
        {
            if (nf2ff_reader.constituentsOperator() != ConstituentsOperator::Absolute)
            {
                return;
            }

            const auto values = nf2ff_reader.constituents();
            Constituents constituents{};
            if (!values.empty())
            {
                constituents.temperature = values[0];
            }
            if (values.size() > 1)
            {
                constituents.salinity = values[1];
            }
            const auto additional_count = std::min(values.size(), std::size_t{2} + constituent_count);
            for (std::size_t i = 2; i < additional_count; ++i)
            {
                constituents.additional_constituents[i - 2] = values[i];
            }
            addConstituents(source, constituents);
        }

        std::pair<double, double> sourceDepthBounds(const SourceOrSinkData& source, const std::size_t source_count)
        {
            if (source_count == 1)
            {
                return {-source.z_coordinate - source.half_plume_height, -source.z_coordinate + source.half_plume_height};
            }
            return {-source.z_coordinate, -source.z_coordinate};
        }
    } // namespace

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
            /// data / for all z-coordinates of the point.
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

    void sendSourcesSinksToFF(precice::Participant& participant, SourcesSinks& sources_sinks)
    {
        std::println("Sending dummy sources/sinks data to far-field...");
        // TESTDATA: set sources_sinks data
        sources_sinks.clearData();
        sources_sinks.addData(1.0, 3.0, -9.95, -9.45, -0.20E+02); // sink 2, source 1
        sources_sinks.addData(2.0, 4.0, -9.95, -9.45, -0.20E+02); // sink 2, source 2
        sources_sinks.addData(3.0, 1.0, -5.0, -5.0, 0.20E+02);    // source 1, sink 2
        sources_sinks.addData(4.0, 2.0, -5.0, -5.0, 0.20E+02);    // source 2, sink 2
        sources_sinks.addData(5.0, 0.0, -5.0, -5.0, 0.50E+01);    // intake fraction to source 1
        sources_sinks.addData(6.0, 0.0, -5.0, -5.0, 0.50E+01);    // intake fraction to source 2
        sources_sinks.addData(7.0, 0.0, 0.0, 0.0, 0.10E+02);      // intake sink
        participant.writeData("sources_sinks_nodes", "sources_sinks_id", sources_sinks.precice_ids, sources_sinks.ids);
        participant.writeData("sources_sinks_nodes", "sources_sinks_connected_id", sources_sinks.precice_ids,
                              sources_sinks.connected_ids);
        participant.writeData("sources_sinks_nodes", "sources_sinks_z_min", sources_sinks.precice_ids,
                              sources_sinks.z_mins);
        participant.writeData("sources_sinks_nodes", "sources_sinks_z_max", sources_sinks.precice_ids,
                              sources_sinks.z_maxs);
        participant.writeData("sources_sinks_nodes", "sources_sinks_discharge", sources_sinks.precice_ids,
                              sources_sinks.discharges);
    }

    void convertNFSinksToFF() { std::println("Processing sinks..."); }

    double convertNFSinksToFF(const NF2FFReader& nf2ff_reader, SourcesSinks& sources_sinks, const double first_record_id,
                              const std::optional<parsing_utils::Point2D>& intake_point)
    {
        auto sources = nf2ff_reader.sources();
        auto sinks = nf2ff_reader.sinks();

        if (sources.empty())
        {
            return first_record_id;
        }

        std::vector<double> source_weights;
        source_weights.reserve(sources.size());
        for (const auto& source : sources)
        {
            source_weights.push_back(source.has_weight ? source.weight : 1.0);
        }

        const double weight_sum = std::accumulate(source_weights.begin(), source_weights.end(), 0.0);
        if (std::abs(weight_sum) > 0.0)
        {
            for (auto& weight : source_weights)
            {
                weight /= weight_sum;
            }
        }

        double next_id = first_record_id;
        const double source_flow_rate = nf2ff_reader.sourceFlowRate();

        // Entrainment: pair each sink segment with each source point.
        for (std::size_t sink_index = 1; sink_index < sinks.size(); ++sink_index)
        {
            const auto& previous_sink = sinks[sink_index - 1];
            const auto& sink = sinks[sink_index];
            const double delta_s = sink.entrainment - previous_sink.entrainment;

            for (std::size_t source_index = 0; source_index < sources.size(); ++source_index)
            {
                const auto& source = sources[source_index];
                const double discharge = delta_s * source_flow_rate * source_weights[source_index];

                const double sink_id = next_id++;
                const double source_id = next_id++;
                const auto [source_z_min, source_z_max] = sourceDepthBounds(source, sources.size());

                const auto sink_endpoint = makeEndpoint(asEndpointId(sink_id), asEndpointId(source_id), sink.x_coordinate,
                                                        sink.y_coordinate, -sink.z_coordinate - sink.half_plume_height,
                                                        -sink.z_coordinate + sink.half_plume_height, -discharge);
                appendEndpointRecord(sources_sinks, sink_endpoint);

                Source source_endpoint{};
                source_endpoint.endpoint =
                    makeEndpoint(asEndpointId(source_id), asEndpointId(sink_id),
                                 source.x_coordinate, source.y_coordinate, source_z_min, source_z_max, discharge);
                maybeAttachMomentum(source_endpoint, source);
                maybeAttachConstituents(source_endpoint, nf2ff_reader);
                appendEndpointRecord(sources_sinks, source_endpoint.endpoint);
            }
        }

        // Discharge at source points.
        for (std::size_t source_index = 0; source_index < sources.size(); ++source_index)
        {
            const auto& source = sources[source_index];
            const auto [source_z_min, source_z_max] = sourceDepthBounds(source, sources.size());
            const double discharge = source_flow_rate * source_weights[source_index];

            Source source_endpoint{};
            source_endpoint.endpoint = makeEndpoint(asEndpointId(next_id++), static_cast<int>(zero_connected_id),
                                                    source.x_coordinate, source.y_coordinate, source_z_min,
                                                    source_z_max, discharge);
            maybeAttachMomentum(source_endpoint, source);
            maybeAttachConstituents(source_endpoint, nf2ff_reader);
            appendEndpointRecord(sources_sinks, source_endpoint.endpoint);
        }

        // Optional intake sink at provided intake position.
        if (intake_point.has_value() && std::abs(nf2ff_reader.intakeFlowRate()) > 0.0)
        {
            const auto intake_endpoint = makeEndpoint(asEndpointId(next_id++), static_cast<int>(zero_connected_id),
                                                      intake_point->x_coordinate, intake_point->y_coordinate, 0.0,
                                                      0.0, nf2ff_reader.intakeFlowRate());
            appendEndpointRecord(sources_sinks, intake_endpoint);
        }

        return next_id;
    }

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
