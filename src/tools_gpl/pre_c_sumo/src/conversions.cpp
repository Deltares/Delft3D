#include "conversions.hpp"

#include <algorithm>
#include <cstddef>
#include <expected>
#include <format>
#include <functional>
#include <limits>
#include <optional>
#include <ranges>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "csumo/data.hpp"
#include "csumo/diffuser.hpp"
#include "csumo/file_paths.hpp"
#include "precice/data.hpp"
#include "precice/sink_source_connections.hpp"
#include "utils/monadic.hpp"

namespace pre_c_sumo
{
    namespace
    {
        std::expected<void, MeshConversionError> validateQuantity(const precice::Mesh& mesh, std::string_view id,
                                                                  std::size_t first, std::size_t count,
                                                                  std::size_t components, std::size_t node_count)
        {
            const auto quantity = mesh.quantities.find(id);
            if (quantity == mesh.quantities.end())
            {
                return std::unexpected(MeshConversionError{std::format("Missing preCICE quantity '{}'.", id)});
            }
            const auto available = quantity->second.size() / components;
            if (quantity->second.size() % components != 0 || available != node_count || first > available ||
                count > available - first)
            {
                return std::unexpected(MeshConversionError{
                    std::format("preCICE quantity '{}' is not aligned with the mesh sample range.", id)});
            }
            return {};
        }

        std::expected<std::vector<double>, precice::SinkSourceConnectionsError> getDepthAveragedIntakeConstituents(
            const precice::Mesh& mesh_3d, const std::size_t diffuser_index)
        {
            const auto intake_index = mesh_3d.forward_map[diffuser_index].intake_index;
            const auto number_of_layers = mesh_3d.number_of_zcoordinates;
            if (number_of_layers == 0)
            {
                return std::unexpected(
                    precice::SinkSourceConnectionsError{"The preCICE intake sample has no vertical layers."});
            }
            std::vector<double> averages;
            averages.reserve(precice::constituent_ids.size());
            for (const auto id : precice::constituent_ids)
            {
                const auto quantity = mesh_3d.quantities.find(id);
                if (quantity == mesh_3d.quantities.end())
                {
                    return std::unexpected(
                        precice::SinkSourceConnectionsError{std::format("Missing preCICE quantity '{}'.", id)});
                }
                const std::span<const double> values(quantity->second);
                if (intake_index > values.size() || number_of_layers > values.size() - intake_index)
                {
                    return std::unexpected(precice::SinkSourceConnectionsError{
                        std::format("preCICE quantity '{}' does not contain the intake layer range.", id)});
                }
                const auto intake_layers = values.subspan(intake_index, number_of_layers);
                averages.push_back(std::ranges::fold_left(intake_layers, 0.0, std::plus{}) /
                                   static_cast<double>(number_of_layers));
            }
            return averages;
        }

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
        precice::DiffuserMapping makeDiffuserMapping(const c_sumo::DiffuserSettings& diffuser_setting,
                                                     const std::size_t diffuser_index,
                                                     const std::size_t number_of_zcoordinates)
        {
            const bool has_intake = diffuser_setting.intake.has_value();
            const std::size_t intake_index = has_intake ? diffuser_index + number_of_zcoordinates : 0;
            const std::size_t first_ambient_point_index =
                has_intake ? diffuser_index + 2 * number_of_zcoordinates : diffuser_index + number_of_zcoordinates;

            const precice::DiffuserMapping diffuser_mapping = {.diffuser_index = diffuser_index,
                                                               .has_intake = has_intake,
                                                               .intake_index = intake_index,
                                                               .first_ambient_point_index = first_ambient_point_index};
            return diffuser_mapping;
        }

    } // namespace

    /**
     * @details Construct both preCICE 2d mesh coordinates and forward mappings from settings.
     * The latter allows us to find the index of values belonging to diffusers, intakes and ambient points
     * in preCICE communication buffers in O(1) time.
     *
     * \image html "preC-SUMO data mapping diagram-2D Mapping.drawio.svg" "2D Data mapping via preCICE" width=1000px
     *
     * \image html "preC-SUMO data mapping diagram-3D Mapping.drawio.svg" "3D Data mapping via preCICE" width=1000px
     *
     *
     */
    precice::Mesh createPreciceMesh(const std::string_view csumo_mesh_name,
                                    const std::vector<c_sumo::DiffuserSettings>& diffusers,
                                    const precice::ZSpecification& z_spec)
    {
        const std::size_t dimensions = z_spec.numberOfDimensions();
        precice::Mesh mesh = {};
        mesh.name = csumo_mesh_name;
        mesh.number_of_zcoordinates = z_spec.numberOfZCoordinates();
        for (const c_sumo::DiffuserSettings& diffuser : diffusers)
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

            for (const c_sumo::Point2D& position : diffuser.ambient_positions)
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

    std::expected<precice::SinkSourceConnections, precice::SinkSourceConnectionsError> convertCSumoToPrecice(
        const std::vector<c_sumo::DiffuserSettings>& diffuser_settings,
        const std::vector<c_sumo::NF2FFData>& near_field_data, const precice::Mesh& csumo_3d_mesh)
    {
        precice::SinkSourceConnections sink_source_connections{};

        if (diffuser_settings.size() != near_field_data.size())
        {
            return std::unexpected(precice::SinkSourceConnectionsError{
                "The number of C-SUMO diffuser settings and NF2FF results must match."});
        }
        for (std::size_t diffuser_index = 0; diffuser_index < near_field_data.size(); diffuser_index++)
        {
            const auto& diffuser = near_field_data[diffuser_index];
            if (diffuser.sources.empty())
            {
                return std::unexpected(precice::SinkSourceConnectionsError{"NF2FF must contain at least one source."});
            }
            if (diffuser.constituents.size() > precice::constituent_ids.size())
            {
                return std::unexpected(precice::SinkSourceConnectionsError{
                    "NF2FF contains more constituents than the preCICE sink/source data supports."});
            }
            std::vector<c_sumo::SinkOrSourceData> sources;
            const bool single_nf2ff_source = diffuser.sources.size() == 1;

            // Normalize the source list once: non-modelled diffusers expand a single NF2FF source
            // into a generated DESA track, so all downstream loops must use this converted list
            // instead of the raw diffuser.sources snapshot.
            if (!c_sumo::isDiffuserModelled(diffuser))
            {
                sources = c_sumo::createDiffuserModel(diffuser);
            }
            else
            {
                sources = diffuser.sources;
            }

            // Send (created) diffuser
            double source_weight_norm = 0.0;
            for (const auto& source : sources)
            {
                source_weight_norm += source.has_weight ? source.weight : 1.0;
            }
            // Intended behavior: source weights are assumed to sum to >= 1.
            // Keep 1.0 as the lower bound so malformed/underspecified input does not amplify discharge.
            source_weight_norm = std::max(source_weight_norm, 1.0);

            const auto sinks = diffuser.sinks;
            // Match nearfield entrainment behavior: use sink deltas, so the first sink does
            // not create entrainment discharge by itself.
            for (std::size_t sink_index = 1; sink_index < sinks.size(); sink_index++)
            {
                double delta_s = sinks[sink_index].entrainment - sinks[sink_index - 1].entrainment;
                if (delta_s < 0.0)
                {
                    return std::unexpected(precice::SinkSourceConnectionsError{"Negative entrainment factor for sink " +
                                                                               std::to_string(sink_index) + ": " +
                                                                               std::to_string(delta_s)});
                }
                const double source_flow_rate = diffuser.source_flow_rate;
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
                    // Momentum is only defined for the source points, not for the entrainment part
                    // Entrainment related momentum should be switched on in D-Flow FM using "NFEntrainmentMomentum = 1"
                    // (not implemented for coupling via preCICE yet)
                    sink_source_connections.add_entry(sink.x_coordinate, sink.y_coordinate, sink_z_bottom, sink_z_top,
                                                      source.x_coordinate, source.y_coordinate, source_z_bottom,
                                                      source_z_top, discharge, 0.0, 0.0, {});
                }
            }

            // Process intakes and set up possibly modified constituents for sources
            auto intakes = diffuser.intakes;
            const double intake_flow_rate = diffuser.intake_flow_rate;
            double intake_weight_norm = 0.0;
            std::vector<double> constituents(diffuser.constituents);
            // Use a practical absolute cutoff: zero or epsilon (~2e-16) is too small
            // for flow magnitudes and would let tiny positive numerical noise trigger
            // fallback intake creation.
            constexpr double minimum_intake_flow_rate = 1e-12;
            if (intake_flow_rate > minimum_intake_flow_rate)
            {
                if (intakes.empty() && diffuser_index < diffuser_settings.size() &&
                    diffuser_settings[diffuser_index].intake.has_value())
                {
                    // Match COSUMO_BMI fallback: when NF2FF has no intake points, use settings XYintake with z=0.0.
                    const auto& intake_xy = diffuser_settings[diffuser_index].intake.value();
                    intakes.emplace_back(c_sumo::IntakeData{.x_coordinate = intake_xy.x_coordinate,
                                                            .y_coordinate = intake_xy.y_coordinate,
                                                            .z_coordinate = 0.0,
                                                            .weight = 0.0,
                                                            .has_weight = false});
                }
                if (!intakes.empty())
                {
                    for (const auto& intake : intakes)
                    {
                        intake_weight_norm += intake.has_weight ? intake.weight : 1.0;
                    }
                    // Intended behavior: intake weights are assumed to sum to >= 1.
                    // Keep 1.0 as the lower bound so malformed/underspecified input does not amplify discharge.
                    intake_weight_norm = std::max(intake_weight_norm, 1.0);

                    // Intakes are sink-only terms (not connected to source points).
                    // Update constituents from diffuser with weighted intake constituents if operator is excess.
                    if (diffuser.constituents_operator == c_sumo::ConstituentsOperator::Excess)
                    {
                        if (diffuser_index >= csumo_3d_mesh.forward_map.size() ||
                            !csumo_3d_mesh.forward_map[diffuser_index].has_intake)
                        {
                            return std::unexpected(precice::SinkSourceConnectionsError{
                                "NF2FF intake constituents require an intake sample in the preCICE mesh."});
                        }
                        ASSIGN_OR_RETURN(const auto intake_constituents,
                                         getDepthAveragedIntakeConstituents(csumo_3d_mesh, diffuser_index));
                        for (const auto& intake : intakes)
                        {
                            for (std::size_t constituent_index = 0; constituent_index < constituents.size();
                                 constituent_index++)
                            {
                                constituents[constituent_index] += intake_constituents[constituent_index] *
                                                                   (intake.has_weight ? intake.weight : 1.0) /
                                                                   intake_weight_norm;
                            }
                        }
                    }
                }
            }

            // Add explicit source discharges independently of entrainment. Weight only the discharge:
            // FM computes each source area as discharge / speed, so the momentum flux is discharge * speed.
            if (!sources.empty())
            {
                const double source_flow_rate = diffuser.source_flow_rate;

                for (const auto& source : sources)
                {
                    double weight_fraction = (source.has_weight ? source.weight : 1.0) / source_weight_norm;
                    double discharge = source_flow_rate * weight_fraction;
                    double source_z_top =
                        single_nf2ff_source ? (-source.z_coordinate + source.half_plume_height) : -source.z_coordinate;
                    double source_z_bottom =
                        single_nf2ff_source ? (-source.z_coordinate - source.half_plume_height) : -source.z_coordinate;
                    double source_speed = source.has_u ? source.u_magnitude : 0.0;
                    double source_moment_direction = source.has_u ? source.u_direction : 0.0;
                    sink_source_connections.add_entry(
                        0.0, 0.0, 0.0, 0.0, source.x_coordinate, source.y_coordinate, source_z_bottom, source_z_top,
                        discharge, source_speed, source_moment_direction, constituents);
                }
            }

            // Dispatch optional intakes after sinks/sources
            if (intake_flow_rate > minimum_intake_flow_rate)
            {
                if (!intakes.empty())
                {
                    // Intakes are sink-only terms (not connected to source points).
                    for (const auto& intake : intakes)
                    {
                        // Dispatch intake discharge.
                        const double intake_discharge =
                            intake_flow_rate * (intake.has_weight ? intake.weight : 1.0) / intake_weight_norm;
                        sink_source_connections.add_entry(intake.x_coordinate, intake.y_coordinate,
                                                          -intake.z_coordinate, -intake.z_coordinate, 0.0, 0.0, 0.0,
                                                          0.0, intake_discharge, 0.0, 0.0, {});
                    }
                }
            }
        }

        return sink_source_connections;
    }

    std::expected<c_sumo::FarFieldPoint2D, MeshConversionError> convertPreciceToCSumoPoint(const std::size_t index_2d,
                                                                                           const std::size_t index_3d,
                                                                                           const precice::Mesh& mesh_2d,
                                                                                           const precice::Mesh& mesh_3d)
    {
        const auto node_count_2d = mesh_2d.coordinates.size() / 2;
        const auto node_count_3d = mesh_3d.coordinates.size() / 3;
        const auto layer_count = mesh_3d.number_of_zcoordinates;
        if (mesh_2d.coordinates.size() % 2 != 0 || mesh_3d.coordinates.size() % 3 != 0 || index_2d >= node_count_2d ||
            layer_count == 0 || index_3d > node_count_3d || layer_count > node_count_3d - index_3d)
        {
            return std::unexpected(MeshConversionError{"preCICE sample coordinates or layer range are invalid."});
        }
        RETURN_IF_ERROR(validateQuantity(mesh_2d, precice::water_levels_id, index_2d, 1, 1, node_count_2d));
        RETURN_IF_ERROR(validateQuantity(mesh_2d, precice::bed_levels_id, index_2d, 1, 1, node_count_2d));
        RETURN_IF_ERROR(
            validateQuantity(mesh_3d, precice::flow_velocities_id, index_3d, layer_count, 3, node_count_3d));
        RETURN_IF_ERROR(validateQuantity(mesh_3d, precice::densities_id, index_3d, layer_count, 1, node_count_3d));
        for (const auto id : precice::constituent_ids)
        {
            RETURN_IF_ERROR(validateQuantity(mesh_3d, id, index_3d, layer_count, 1, node_count_3d));
        }
        for (std::size_t i = 0; i < layer_count; ++i)
        {
            const auto offset = (index_3d + i) * 3;
            if (mesh_3d.coordinates[offset] != mesh_2d.coordinates[index_2d * 2] ||
                mesh_3d.coordinates[offset + 1] != mesh_2d.coordinates[index_2d * 2 + 1])
            {
                return std::unexpected(MeshConversionError{"The 2D and 3D preCICE sample coordinates disagree."});
            }
        }

        std::vector<c_sumo::FarFieldLayer> layers;
        for (std::size_t i = 0; i < mesh_3d.number_of_zcoordinates; ++i)
        {
            // Create a lambda to extract constituent values for the current layer
            const auto from_constituent_id_to_value = [&mesh_3d, layer_index = index_3d + i](const auto id) {
                return mesh_3d.quantities.at(id)[layer_index];
            };
            layers.emplace_back(c_sumo::FarFieldLayer{
                // 3d.coordinates = (x1, y1, z1, x2, y2, z2, ...): skip "index_3d + i" points, then skip x and y
                .z_coordinate = -mesh_3d.coordinates[(index_3d + i) * 3 + 2],
                // 3d.velocities = (vx1, vy1, vz1, vx2, vy2, vz2, ...): skip "index_3d + i" points, then get x and y
                .x_velocity = mesh_3d.quantities.at(precice::flow_velocities_id)[(index_3d + i) * 3 + 0],
                .y_velocity = mesh_3d.quantities.at(precice::flow_velocities_id)[(index_3d + i) * 3 + 1],
                .density = mesh_3d.quantities.at(precice::densities_id)[index_3d + i],
                .constituents = precice::constituent_ids | std::views::transform(from_constituent_id_to_value) |
                                std::ranges::to<std::vector>(),
            });
        }
        return c_sumo::FarFieldPoint2D{
            // 2d.coordinates = (x1, y1, x2, y2, ...)
            .position = {mesh_2d.coordinates[index_2d * 2], mesh_2d.coordinates[index_2d * 2 + 1]},
            .water_depth = mesh_2d.quantities.at(precice::water_levels_id)[index_2d] +
                           mesh_2d.quantities.at(precice::bed_levels_id)[index_2d],
            .layers = layers,
        };
    }

    std::expected<c_sumo::FF2NFConfig, MeshConversionError> convertPreciceToCSumo(
        const c_sumo::DiffuserSettings& diffuser, std::size_t mapping_index, const precice::Mesh& mesh_2d,
        const precice::Mesh& mesh_3d, double current_time_seconds)
    {
        if (mapping_index >= mesh_2d.forward_map.size() || mapping_index >= mesh_3d.forward_map.size() ||
            mapping_index >= static_cast<std::size_t>(std::numeric_limits<int>::max()))
        {
            return std::unexpected(MeshConversionError{"Missing or invalid preCICE diffuser mapping."});
        }
        const auto subgrid_model_nr = static_cast<int>(mapping_index + 1);
        const auto& mapping_2d = mesh_2d.forward_map[mapping_index];
        const auto& mapping_3d = mesh_3d.forward_map[mapping_index];
        if (mapping_2d.has_intake != mapping_3d.has_intake || mapping_2d.has_intake != diffuser.intake.has_value())
        {
            return std::unexpected(MeshConversionError{"The diffuser and preCICE intake mappings disagree."});
        }
        const auto ambient_count = diffuser.ambient_positions.size();
        const auto nodes_2d = mesh_2d.coordinates.size() / 2;
        const auto nodes_3d = mesh_3d.coordinates.size() / 3;
        const auto layers = mesh_3d.number_of_zcoordinates;
        if (ambient_count != 0 && (layers == 0 || mapping_2d.first_ambient_point_index > nodes_2d ||
                                   ambient_count > nodes_2d - mapping_2d.first_ambient_point_index ||
                                   mapping_3d.first_ambient_point_index > nodes_3d ||
                                   ambient_count > (nodes_3d - mapping_3d.first_ambient_point_index) / layers))
        {
            return std::unexpected(MeshConversionError{"preCICE ambient point mappings exceed the mesh."});
        }
        std::vector<c_sumo::FarFieldPoint2D> ambient_points;
        for (std::size_t index = 0; index < ambient_count; ++index)
        {
            ASSIGN_OR_RETURN(auto point, convertPreciceToCSumoPoint(
                                             mapping_2d.first_ambient_point_index + index,
                                             mapping_3d.first_ambient_point_index + index * layers, mesh_2d, mesh_3d));
            ambient_points.emplace_back(std::move(point));
        }
        ASSIGN_OR_RETURN(auto diffuser_point, convertPreciceToCSumoPoint(mapping_2d.diffuser_index,
                                                                         mapping_3d.diffuser_index, mesh_2d, mesh_3d));
        std::optional<c_sumo::FarFieldPoint2D> intake;
        if (mapping_2d.has_intake)
        {
            ASSIGN_OR_RETURN(auto intake_point, convertPreciceToCSumoPoint(mapping_2d.intake_index,
                                                                           mapping_3d.intake_index, mesh_2d, mesh_3d));
            intake = std::move(intake_point);
        }
        return c_sumo::FF2NFConfig{
            .ff2nf_filename = c_sumo::ff2nfFilepath(diffuser, subgrid_model_nr, current_time_seconds).string(),
            .wait_for_file = c_sumo::nf2ffFilepath(diffuser, subgrid_model_nr, current_time_seconds).string(),
            .ff_run_directory = diffuser.ff_run_dir.string(),
            .run_id = "FlowFM",
            .unique_id = "",
            .subgrid_model_nr = subgrid_model_nr,
            .current_time_seconds = current_time_seconds,
            .constituent_names = precice::constituent_ids |
                                 std::views::transform([](const auto id) { return std::string(id); }) |
                                 std::ranges::to<std::vector>(),
            .diffuser = std::move(diffuser_point),
            .intake = std::move(intake),
            .ambient_points = std::move(ambient_points),
            .settings_xml = diffuser.settings_xml,
        };
    }
} // namespace pre_c_sumo
