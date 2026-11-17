#include <gtest/gtest.h>

#include <cstddef>

#include "precice_data.hpp"
#include "precice_to_csumo.hpp"

TEST(PreciceToCSumoTest, ConvertsPointAndLayersAtNonzeroIndices)
{
    pre_c_sumo::PreciceMesh mesh_2d{};
    mesh_2d.coordinates = {1.0, 2.0, 10.0, 20.0};
    mesh_2d.quantities = {
        {pre_c_sumo::water_levels_id, {0.0, 1.5}},
        {pre_c_sumo::bed_levels_id, {1.0, 8.5}},
    };

    pre_c_sumo::PreciceMesh mesh_3d{};
    mesh_3d.number_of_zcoordinates = 2;
    mesh_3d.coordinates = {1.0, 2.0, -2.0, 1.0, 2.0, -1.0, 10.0, 20.0, -8.0, 10.0, 20.0, -3.0};
    mesh_3d.quantities = {
        {pre_c_sumo::flow_velocities_id, {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.25, -2.5, 99.0, -3.5, 4.75, 88.0}},
        {pre_c_sumo::densities_id, {999.0, 1000.0, 1025.0, 1020.0}},
    };
    for (std::size_t i = 0; i < pre_c_sumo::constituent_ids.size(); ++i)
    {
        mesh_3d.quantities[pre_c_sumo::constituent_ids[i]] = {-1.0, -2.0, 10.0 + static_cast<double>(i),
                                                              20.0 + static_cast<double>(i)};
    }

    const auto point = pre_c_sumo::convertPreciceToCSumoPoint(1, 2, mesh_2d, mesh_3d);

    EXPECT_DOUBLE_EQ(point.position.x_coordinate, 10.0);
    EXPECT_DOUBLE_EQ(point.position.y_coordinate, 20.0);
    EXPECT_DOUBLE_EQ(point.water_depth, 10.0);
    ASSERT_EQ(point.layers.size(), 2);
    EXPECT_DOUBLE_EQ(point.layers[0].z_coordinate, 8.0);
    EXPECT_DOUBLE_EQ(point.layers[0].x_velocity, 1.25);
    EXPECT_DOUBLE_EQ(point.layers[0].y_velocity, -2.5);
    EXPECT_DOUBLE_EQ(point.layers[0].density, 1025.0);
    EXPECT_DOUBLE_EQ(point.layers[1].z_coordinate, 3.0);
    EXPECT_DOUBLE_EQ(point.layers[1].x_velocity, -3.5);
    EXPECT_DOUBLE_EQ(point.layers[1].y_velocity, 4.75);
    EXPECT_DOUBLE_EQ(point.layers[1].density, 1020.0);
    ASSERT_EQ(point.layers[0].constituents.size(), pre_c_sumo::constituent_ids.size());
    ASSERT_EQ(point.layers[1].constituents.size(), pre_c_sumo::constituent_ids.size());
    for (std::size_t i = 0; i < pre_c_sumo::constituent_ids.size(); ++i)
    {
        EXPECT_DOUBLE_EQ(point.layers[0].constituents[i], 10.0 + static_cast<double>(i));
        EXPECT_DOUBLE_EQ(point.layers[1].constituents[i], 20.0 + static_cast<double>(i));
    }
}

TEST(PreciceToCSumoTest, AssemblesDiffuserIntakeAndAmbientProfilesUsingTheirMappings)
{
    pre_c_sumo::PreciceMesh mesh_2d{};
    mesh_2d.coordinates = {0.0, 0.0, 10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 80.0};
    mesh_2d.forward_map = {{0, false, 0, 0, 1}, {1, true, 2, 2, 3}};
    mesh_2d.quantities = {
        {pre_c_sumo::water_levels_id, std::vector<double>(5, 1.0)},
        {pre_c_sumo::bed_levels_id, std::vector<double>(5, 9.0)},
    };
    pre_c_sumo::PreciceMesh mesh_3d{};
    mesh_3d.number_of_zcoordinates = 2;
    mesh_3d.forward_map = {{0, false, 0, 0, 2}, {2, true, 4, 4, 6}};
    for (std::size_t index = 0; index < 10; ++index)
    {
        const auto horizontal_index = index / 2;
        mesh_3d.coordinates.push_back(mesh_2d.coordinates[horizontal_index * 2]);
        mesh_3d.coordinates.push_back(mesh_2d.coordinates[horizontal_index * 2 + 1]);
        mesh_3d.coordinates.push_back(-1.0 - static_cast<double>(index));
    }
    mesh_3d.quantities = {
        {pre_c_sumo::flow_velocities_id, std::vector<double>(30, 1.0)},
        {pre_c_sumo::densities_id, std::vector<double>(10, 1025.0)},
    };
    for (const auto id : pre_c_sumo::constituent_ids)
    {
        mesh_3d.quantities[id] = std::vector<double>(10, 15.0);
    }
    pre_c_sumo::DiffuserSettings diffuser{};
    diffuser.ambient_positions = {{50.0, 60.0}, {70.0, 80.0}};
    diffuser.ff2nf_dir = "FF2NF";
    diffuser.ff_run_dir = "run";

    const auto input = pre_c_sumo::convertPreciceToCSumo(diffuser, 1, mesh_2d, mesh_3d, 90.0);

    EXPECT_EQ(input.subgrid_model_nr, 2);
    EXPECT_DOUBLE_EQ(input.current_time_seconds, 90.0);
    EXPECT_DOUBLE_EQ(input.diffuser.position.x_coordinate, 10.0);
    ASSERT_EQ(input.diffuser.layers.size(), 2u);
    EXPECT_DOUBLE_EQ(input.diffuser.layers[0].z_coordinate, 3.0);
    ASSERT_TRUE(input.intake.has_value());
    EXPECT_DOUBLE_EQ(input.intake->position.x_coordinate, 30.0);
    EXPECT_DOUBLE_EQ(input.intake->layers[0].z_coordinate, 5.0);
    ASSERT_EQ(input.ambient_points.size(), 2u);
    EXPECT_DOUBLE_EQ(input.ambient_points[0].layers[0].z_coordinate, 7.0);
    EXPECT_DOUBLE_EQ(input.ambient_points[1].position.x_coordinate, 70.0);
    EXPECT_DOUBLE_EQ(input.ambient_points[1].layers[0].z_coordinate, 9.0);
    EXPECT_DOUBLE_EQ(input.ambient_points[1].layers[1].z_coordinate, 10.0);
    EXPECT_EQ(input.constituent_names.size(), pre_c_sumo::constituent_ids.size());
}
