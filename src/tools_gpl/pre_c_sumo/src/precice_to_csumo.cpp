#include "precice_to_csumo.hpp"
#include "csumo_file_paths.hpp"
#include "monadic_utils.hpp"

#include <format>
#include <limits>
#include <optional>
#include <ranges>
#include <string>
#include <utility>
#include <vector>

namespace
{
    std::expected<void, pre_c_sumo::MeshConversionError> validateQuantity(const pre_c_sumo::precice::Mesh& mesh,
                                                                          std::string_view id, std::size_t first,
                                                                          std::size_t count, std::size_t components,
                                                                          std::size_t node_count)
    {
        const auto quantity = mesh.quantities.find(id);
        if (quantity == mesh.quantities.end())
        {
            return std::unexpected(pre_c_sumo::MeshConversionError{std::format("Missing preCICE quantity '{}'.", id)});
        }
        const auto available = quantity->second.size() / components;
        if (quantity->second.size() % components != 0 || available != node_count || first > available ||
            count > available - first)
        {
            return std::unexpected(pre_c_sumo::MeshConversionError{
                std::format("preCICE quantity '{}' is not aligned with the mesh sample range.", id)});
        }
        return {};
    }
}

namespace pre_c_sumo
{
    std::expected<c_sumo::FarFieldPoint2D, MeshConversionError> convertPreciceToCSumoPoint(const std::size_t index_2d,
                                                                                           const std::size_t index_3d,
                                                                                           const precice::Mesh& mesh_2d,
                                                                                           const precice::Mesh& mesh_3d)
    {
        const auto node_count_2d = mesh_2d.coordinates.size() / 2;
        const auto node_count_3d = mesh_3d.coordinates.size() / 3;
        const auto layer_count = mesh_3d.number_of_zcoordinates;
        if (mesh_2d.coordinates.size() % 2 != 0 || mesh_3d.coordinates.size() % 3 != 0 ||
            index_2d >= node_count_2d || layer_count == 0 || index_3d > node_count_3d ||
            layer_count > node_count_3d - index_3d)
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
        if (mapping_2d.has_intake != mapping_3d.has_intake ||
            mapping_2d.has_intake != diffuser.intake.has_value())
        {
            return std::unexpected(MeshConversionError{"The diffuser and preCICE intake mappings disagree."});
        }
        const auto ambient_count = diffuser.ambient_positions.size();
        const auto nodes_2d = mesh_2d.coordinates.size() / 2;
        const auto nodes_3d = mesh_3d.coordinates.size() / 3;
        const auto layers = mesh_3d.number_of_zcoordinates;
        if (ambient_count != 0 &&
            (layers == 0 || mapping_2d.first_ambient_point_index > nodes_2d ||
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
        ASSIGN_OR_RETURN(auto diffuser_point,
                         convertPreciceToCSumoPoint(mapping_2d.diffuser_index, mapping_3d.diffuser_index,
                                                    mesh_2d, mesh_3d));
        std::optional<c_sumo::FarFieldPoint2D> intake;
        if (mapping_2d.has_intake)
        {
            ASSIGN_OR_RETURN(auto intake_point,
                             convertPreciceToCSumoPoint(mapping_2d.intake_index, mapping_3d.intake_index,
                                                        mesh_2d, mesh_3d));
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
