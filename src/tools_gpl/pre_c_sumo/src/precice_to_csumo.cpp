#include "precice_to_csumo.hpp"
#include "csumo_file_paths.hpp"

#include <functional>
#include <ranges>
#include <span>
#include <utility>
#include <vector>

namespace pre_c_sumo
{
    FarFieldPoint2D convertPreciceToCSumoPoint(const std::size_t index_2d, const std::size_t index_3d,
                                            const PreciceMesh& mesh_2d, const PreciceMesh& mesh_3d)
    {
        std::vector<FarFieldLayer> layers;
        for (std::size_t i = 0; i < mesh_3d.number_of_zcoordinates; ++i)
        {
            // Create a lambda to extract constituent values for the current layer
            const auto from_constituent_id_to_value = [&mesh_3d, layer_index = index_3d + i](const auto id) {
                return mesh_3d.quantities.at(id)[layer_index];
            };
            layers.emplace_back(FarFieldLayer{
                // 3d.coordinates = (x1, y1, z1, x2, y2, z2, ...): skip "index_3d + i" points, then skip x and y
                .z_coordinate = -mesh_3d.coordinates[(index_3d + i) * 3 + 2],
                // 3d.velocities = (vx1, vy1, vz1, vx2, vy2, vz2, ...): skip "index_3d + i" points, then get x and y
                .x_velocity = mesh_3d.quantities.at(flow_velocities_id)[(index_3d + i) * 3 + 0],
                .y_velocity = mesh_3d.quantities.at(flow_velocities_id)[(index_3d + i) * 3 + 1],
                .density = mesh_3d.quantities.at(densities_id)[index_3d + i],
                .constituents = constituent_ids | std::views::transform(from_constituent_id_to_value) |
                                std::ranges::to<std::vector>(),
            });
        }
        return FarFieldPoint2D{
            // 2d.coordinates = (x1, y1, x2, y2, ...)
            .position = {mesh_2d.coordinates[index_2d * 2], mesh_2d.coordinates[index_2d * 2 + 1]},
            .water_depth =
                mesh_2d.quantities.at(water_levels_id)[index_2d] + mesh_2d.quantities.at(bed_levels_id)[index_2d],
            .layers = layers,
        };
    }
    std::vector<double> getIntakeConstituents(const PreciceMesh& mesh_3d, std::size_t diffuser_index)
    {
        const auto index_3d = mesh_3d.forward_map[diffuser_index].intake_index;
        const auto number_of_layers = mesh_3d.number_of_zcoordinates;
        const auto compute_depth_average = [&mesh_3d, index_3d, number_of_layers](const auto id) {
            const std::span<const double> values(mesh_3d.quantities.at(id));
            const auto intake_layers = values.subspan(index_3d, number_of_layers);
            return std::ranges::fold_left(intake_layers, 0.0, std::plus{}) / static_cast<double>(number_of_layers);
        };
        return constituent_ids | std::views::transform(compute_depth_average) | std::ranges::to<std::vector>();
    }

    FF2NFConfig convertPreciceToCSumo(const DiffuserSettings& diffuser, std::size_t mapping_index,
                                     const PreciceMesh& mesh_2d, const PreciceMesh& mesh_3d,
                                     double current_time_seconds)
    {
        const auto subgrid_model_nr = static_cast<int>(mapping_index + 1);
        const auto& mapping = mesh_2d.forward_map[mapping_index];
        const auto& mapping_3d = mesh_3d.forward_map[mapping_index];
        std::vector<FarFieldPoint2D> ambient_points;
        for (std::size_t index = 0; index < diffuser.ambient_positions.size(); ++index)
        {
            ambient_points.emplace_back(convertPreciceToCSumoPoint(
                mapping.first_ambient_point_index + index,
                mapping_3d.first_ambient_point_index + index * mesh_3d.number_of_zcoordinates, mesh_2d, mesh_3d));
        }
        return FF2NFConfig{
            .ff2nf_filename = ff2nfFilepath(diffuser, subgrid_model_nr, current_time_seconds).string(),
            .wait_for_file = nf2ffFilepath(diffuser, subgrid_model_nr, current_time_seconds).string(),
            .ff_run_directory = diffuser.ff_run_dir.string(),
            .run_id = "FlowFM",
            .unique_id = "",
            .subgrid_model_nr = subgrid_model_nr,
            .current_time_seconds = current_time_seconds,
            .constituent_names = constituent_ids |
                                 std::views::transform([](const auto id) { return std::string(id); }) |
                                 std::ranges::to<std::vector>(),
            .diffuser = convertPreciceToCSumoPoint(mapping.diffuser_index, mapping_3d.diffuser_index, mesh_2d, mesh_3d),
            .intake = mapping.has_intake
                          ? std::optional{convertPreciceToCSumoPoint(mapping.intake_index, mapping_3d.intake_index,
                                                                    mesh_2d, mesh_3d)}
                          : std::nullopt,
            .ambient_points = std::move(ambient_points),
            .settings_xml_node = diffuser.settings_xml_node,
        };
    }
} // namespace pre_c_sumo
