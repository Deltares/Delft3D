#include <gtest/gtest.h>

#include <vector>

#include "csumo_to_precice.hpp"

TEST(CSumoToPreciceTest, ConvertsNearFieldRecordsWithoutAnXmlReaderOrParticipant)
{
    const pre_c_sumo::NF2FFData result{
        .source_flow_rate = 10.0,
        .constituents_operator = pre_c_sumo::ConstituentsOperator::Absolute,
        .constituents = {15.0, 2.0},
        .sources =
            {
                {.x_coordinate = 10.0,
                 .y_coordinate = 20.0,
                 .z_coordinate = 5.0,
                 .u_magnitude = 4.0,
                 .u_direction = 90.0,
                 .weight = 1.0,
                 .has_u = true,
                 .has_weight = true},
                {.x_coordinate = 30.0,
                 .y_coordinate = 40.0,
                 .z_coordinate = 6.0,
                 .u_magnitude = 4.0,
                 .u_direction = 180.0,
                 .weight = 3.0,
                 .has_u = true,
                 .has_weight = true},
            },
    };

    const auto converted = pre_c_sumo::convertCSumoToPrecice({}, {result}, {});
    ASSERT_TRUE(converted.has_value()) << converted.error().message;
    ASSERT_EQ(converted->get_number_of_entries(), 2u);
    const auto& data = converted->data();
    EXPECT_EQ(data.source_x_vector, (std::vector<double>{10.0, 30.0}));
    EXPECT_EQ(data.source_y_vector, (std::vector<double>{20.0, 40.0}));
    EXPECT_EQ(data.source_z_bottom_vector, (std::vector<double>{-5.0, -6.0}));
    EXPECT_EQ(data.source_z_top_vector, (std::vector<double>{-5.0, -6.0}));
    EXPECT_EQ(data.discharge_vector, (std::vector<double>{2.5, 7.5}));
    EXPECT_EQ(data.momentum_magnitude_weighted_vector, (std::vector<double>{0.25, 2.25}));
    EXPECT_EQ(data.momentum_direction_vector, (std::vector<double>{90.0, 180.0}));
    EXPECT_EQ(data.constituents_vectors[0], (std::vector<double>{15.0, 15.0}));
    EXPECT_EQ(data.constituents_vectors[1], (std::vector<double>{2.0, 2.0}));
    EXPECT_EQ(data.constituents_vectors.back(), (std::vector<double>{0.0, 0.0}));
}

TEST(CSumoToPreciceTest, AddsDepthAveragedIntakeValuesToExcessConstituents)
{
    const pre_c_sumo::NF2FFData result{
        .intake_flow_rate = 2.0,
        .source_flow_rate = 10.0,
        .constituents_operator = pre_c_sumo::ConstituentsOperator::Excess,
        .constituents = std::vector<double>(pre_c_sumo::constituent_ids.size(), 3.0),
        .intakes = {{.x_coordinate = 1.0, .y_coordinate = 2.0, .z_coordinate = 2.0}},
        .sources = {{.x_coordinate = 10.0, .y_coordinate = 20.0, .z_coordinate = 5.0, .half_plume_height = 1.0}},
    };
    pre_c_sumo::PreciceMesh mesh{};
    mesh.number_of_zcoordinates = 2;
    mesh.forward_map = {{.has_intake = true, .intake_index = 2}};
    for (std::size_t index = 0; index < pre_c_sumo::constituent_ids.size(); ++index)
    {
        mesh.quantities[pre_c_sumo::constituent_ids[index]] = {
            0.0, 0.0, static_cast<double>(index) + 1.0, static_cast<double>(index) + 5.0};
    }

    const auto converted = pre_c_sumo::convertCSumoToPrecice({}, {result}, mesh);
    ASSERT_TRUE(converted.has_value()) << converted.error().message;
    ASSERT_EQ(converted->get_number_of_entries(), 2u);
    const auto& data = converted->data();
    EXPECT_EQ(data.discharge_vector, (std::vector<double>{10.0, 2.0}));
    for (std::size_t index = 0; index < pre_c_sumo::constituent_ids.size(); ++index)
    {
        EXPECT_DOUBLE_EQ(data.constituents_vectors[index][0], static_cast<double>(index) + 6.0);
    }
    EXPECT_DOUBLE_EQ(data.source_z_bottom_vector[0], -6.0);
    EXPECT_DOUBLE_EQ(data.source_z_top_vector[0], -4.0);
    EXPECT_DOUBLE_EQ(data.sink_z_bottom_vector[1], -2.0);
    EXPECT_DOUBLE_EQ(data.sink_z_top_vector[1], -2.0);
}

TEST(CSumoToPreciceTest, RejectsIntakeConstituentsWithoutAnIntakeSample)
{
    const pre_c_sumo::NF2FFData result{
        .intake_flow_rate = 2.0,
        .constituents_operator = pre_c_sumo::ConstituentsOperator::Excess,
        .constituents = {3.0},
        .intakes = {{.x_coordinate = 1.0, .y_coordinate = 2.0, .z_coordinate = 2.0}},
        .sources = {{.x_coordinate = 10.0, .y_coordinate = 20.0, .z_coordinate = 5.0}},
    };
    pre_c_sumo::PreciceMesh mesh{};
    mesh.forward_map = {{.has_intake = false}};

    const auto converted = pre_c_sumo::convertCSumoToPrecice({}, {result}, mesh);

    ASSERT_FALSE(converted.has_value());
    EXPECT_EQ(converted.error().message,
              "NF2FF intake constituents require an intake sample in the preCICE mesh.");
}
