#include <gtest/gtest.h>

#include <vector>

#include "csumo_to_precice.hpp"

TEST(CSumoToPreciceTest, Builds2DAnd3DMeshesAndMappingsForDifferentDiffusers)
{
    const std::vector<pre_c_sumo::c_sumo::DiffuserSettings> diffusers{
        {.position = {1, 2}, .ambient_positions = {{3, 4}, {5, 6}}, .intake = pre_c_sumo::c_sumo::Point2D{7, 8}},
        {.position = {9, 10}, .ambient_positions = {{11, 12}}},
    };

    const auto mesh_2d =
        pre_c_sumo::createPreciceMesh("2d", diffusers, pre_c_sumo::precice::ZSpecification{true, 0, 0, 0});
    const auto mesh_3d =
        pre_c_sumo::createPreciceMesh("3d", diffusers, pre_c_sumo::precice::ZSpecification{false, -2, 2, 0});

    EXPECT_EQ(mesh_2d.coordinates, (std::vector<double>{1, 2, 7, 8, 3, 4, 5, 6, 9, 10, 11, 12}));
    EXPECT_EQ(mesh_3d.coordinates,
              (std::vector<double>{1, 2, -2, 1, 2, 0, 7, 8, -2, 7, 8, 0, 3, 4, -2, 3, 4, 0,
                                   5, 6, -2, 5, 6, 0, 9, 10, -2, 9, 10, 0, 11, 12, -2, 11, 12, 0}));
    EXPECT_EQ(mesh_2d.number_of_nodes, 6u);
    EXPECT_EQ(mesh_3d.number_of_nodes, 12u);
    ASSERT_EQ(mesh_2d.forward_map.size(), 2u);
    ASSERT_EQ(mesh_3d.forward_map.size(), 2u);
    EXPECT_EQ(mesh_2d.forward_map[0].diffuser_index, 0u);
    EXPECT_TRUE(mesh_2d.forward_map[0].has_intake);
    EXPECT_EQ(mesh_2d.forward_map[0].intake_index, 1u);
    EXPECT_EQ(mesh_2d.forward_map[0].first_ambient_point_index, 2u);
    EXPECT_EQ(mesh_2d.forward_map[1].diffuser_index, 4u);
    EXPECT_FALSE(mesh_2d.forward_map[1].has_intake);
    EXPECT_EQ(mesh_2d.forward_map[1].first_ambient_point_index, 5u);
    EXPECT_EQ(mesh_3d.forward_map[0].diffuser_index, 0u);
    EXPECT_TRUE(mesh_3d.forward_map[0].has_intake);
    EXPECT_EQ(mesh_3d.forward_map[0].intake_index, 2u);
    EXPECT_EQ(mesh_3d.forward_map[0].first_ambient_point_index, 4u);
    EXPECT_EQ(mesh_3d.forward_map[1].diffuser_index, 8u);
    EXPECT_FALSE(mesh_3d.forward_map[1].has_intake);
    EXPECT_EQ(mesh_3d.forward_map[1].first_ambient_point_index, 10u);
}

TEST(CSumoToPreciceTest, ConvertsNearFieldRecordsWithoutAnXmlReaderOrParticipant)
{
    const pre_c_sumo::c_sumo::NF2FFData result{
        .source_flow_rate = 10.0,
        .constituents_operator = pre_c_sumo::c_sumo::ConstituentsOperator::Absolute,
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

    const auto converted = pre_c_sumo::convertCSumoToPrecice({pre_c_sumo::c_sumo::DiffuserSettings{}}, {result}, {});
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
    const pre_c_sumo::c_sumo::NF2FFData result{
        .intake_flow_rate = 2.0,
        .source_flow_rate = 10.0,
        .constituents_operator = pre_c_sumo::c_sumo::ConstituentsOperator::Excess,
        .constituents = std::vector<double>(pre_c_sumo::precice::constituent_ids.size(), 3.0),
        .intakes = {{.x_coordinate = 1.0, .y_coordinate = 2.0, .z_coordinate = 2.0}},
        .sources = {{.x_coordinate = 10.0, .y_coordinate = 20.0, .z_coordinate = 5.0, .half_plume_height = 1.0}},
    };
    pre_c_sumo::precice::Mesh mesh{};
    mesh.number_of_zcoordinates = 2;
    mesh.forward_map = {{.has_intake = true, .intake_index = 2}};
    for (std::size_t index = 0; index < pre_c_sumo::precice::constituent_ids.size(); ++index)
    {
        mesh.quantities[pre_c_sumo::precice::constituent_ids[index]] = {0.0, 0.0, static_cast<double>(index) + 1.0,
                                                                        static_cast<double>(index) + 5.0};
    }

    const auto converted = pre_c_sumo::convertCSumoToPrecice({pre_c_sumo::c_sumo::DiffuserSettings{}}, {result}, mesh);
    ASSERT_TRUE(converted.has_value()) << converted.error().message;
    ASSERT_EQ(converted->get_number_of_entries(), 2u);
    const auto& data = converted->data();
    EXPECT_EQ(data.discharge_vector, (std::vector<double>{10.0, 2.0}));
    for (std::size_t index = 0; index < pre_c_sumo::precice::constituent_ids.size(); ++index)
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
    const pre_c_sumo::c_sumo::NF2FFData result{
        .intake_flow_rate = 2.0,
        .constituents_operator = pre_c_sumo::c_sumo::ConstituentsOperator::Excess,
        .constituents = {3.0},
        .intakes = {{.x_coordinate = 1.0, .y_coordinate = 2.0, .z_coordinate = 2.0}},
        .sources = {{.x_coordinate = 10.0, .y_coordinate = 20.0, .z_coordinate = 5.0}},
    };
    pre_c_sumo::precice::Mesh mesh{};
    mesh.forward_map = {{.has_intake = false}};

    const auto converted = pre_c_sumo::convertCSumoToPrecice({pre_c_sumo::c_sumo::DiffuserSettings{}}, {result}, mesh);

    ASSERT_FALSE(converted.has_value());
    EXPECT_EQ(converted.error().message,
              "NF2FF intake constituents require an intake sample in the preCICE mesh.");
}

TEST(CSumoToPreciceTest, RejectsMoreConstituentsThanThePreciceMeshSupports)
{
    for (const auto operation :
         {pre_c_sumo::c_sumo::ConstituentsOperator::Absolute, pre_c_sumo::c_sumo::ConstituentsOperator::Excess})
    {
        const pre_c_sumo::c_sumo::NF2FFData result{
            .intake_flow_rate = 2.0,
            .constituents_operator = operation,
            .constituents = std::vector<double>(pre_c_sumo::precice::constituent_ids.size() + 1, 3.0),
            .intakes = {{.x_coordinate = 1.0, .y_coordinate = 2.0, .z_coordinate = 2.0}},
            .sources = {{.x_coordinate = 10.0, .y_coordinate = 20.0, .z_coordinate = 5.0}},
        };
        pre_c_sumo::precice::Mesh mesh{};
        mesh.number_of_zcoordinates = 1;
        mesh.forward_map = {{.has_intake = true, .intake_index = 0}};
        for (const auto id : pre_c_sumo::precice::constituent_ids)
        {
            mesh.quantities[id] = {1.0};
        }

        const auto converted =
            pre_c_sumo::convertCSumoToPrecice({pre_c_sumo::c_sumo::DiffuserSettings{}}, {result}, mesh);

        ASSERT_FALSE(converted.has_value());
        EXPECT_EQ(converted.error().message,
                  "NF2FF contains more constituents than the preCICE sink/source data supports.");
    }
}

TEST(CSumoToPreciceTest, RejectsInconsistentDiffuserCountsAndInvalidIntakeBuffers)
{
    const pre_c_sumo::c_sumo::NF2FFData result{
        .intake_flow_rate = 1.0,
        .constituents_operator = pre_c_sumo::c_sumo::ConstituentsOperator::Excess,
        .constituents = {3.0},
        .intakes = {{.x_coordinate = 1.0, .y_coordinate = 2.0}},
        .sources = {{.x_coordinate = 10.0, .y_coordinate = 20.0}},
    };
    const auto settings = std::vector<pre_c_sumo::c_sumo::DiffuserSettings>(1);
    EXPECT_FALSE(pre_c_sumo::convertCSumoToPrecice({}, {result}, {}).has_value());
    auto empty_source_result = result;
    empty_source_result.sources.clear();
    EXPECT_FALSE(pre_c_sumo::convertCSumoToPrecice(settings, {empty_source_result}, {}).has_value());
    pre_c_sumo::precice::Mesh mesh{};
    mesh.forward_map = {{.has_intake = true, .intake_index = 1}};
    EXPECT_FALSE(pre_c_sumo::convertCSumoToPrecice(settings, {result}, mesh).has_value());
    mesh.number_of_zcoordinates = 2;
    for (const auto id : pre_c_sumo::precice::constituent_ids)
    {
        mesh.quantities[id] = {0.0, 1.0, 2.0};
    }
    ASSERT_TRUE(pre_c_sumo::convertCSumoToPrecice(settings, {result}, mesh).has_value());
    mesh.quantities.erase(pre_c_sumo::precice::constituent_ids[0]);
    EXPECT_FALSE(pre_c_sumo::convertCSumoToPrecice(settings, {result}, mesh).has_value());
    mesh.quantities[pre_c_sumo::precice::constituent_ids[0]] = {0.0, 1.0};
    EXPECT_FALSE(pre_c_sumo::convertCSumoToPrecice(settings, {result}, mesh).has_value());
}

TEST(CSumoToPreciceTest, ConvertsEntrainmentSourceAndIntakeDischarges)
{
    const pre_c_sumo::c_sumo::NF2FFData result{
        .intake_flow_rate = 10.0,
        .source_flow_rate = 10.0,
        .intakes = {{.x_coordinate = 1.0, .y_coordinate = 2.0}},
        .sources = {{.x_coordinate = 10.0}, {.x_coordinate = 20.0}},
        .sinks = {{.entrainment = 1.0}, {.entrainment = 5.0}},
    };

    const auto converted = pre_c_sumo::convertCSumoToPrecice({pre_c_sumo::c_sumo::DiffuserSettings{}}, {result}, {});

    ASSERT_TRUE(converted.has_value()) << converted.error().message;
    EXPECT_EQ(converted->get_discharge_value(), (std::vector<double>{20.0, 20.0, 5.0, 5.0, 10.0}));
}

TEST(CSumoToPreciceTest, ExpandsSingleSourceBeforeConversion)
{
    const pre_c_sumo::c_sumo::NF2FFData result{
        .source_flow_rate = 10.0,
        .sources = {{.x_coordinate = 100.0, .y_coordinate = 200.0, .half_plume_width = 10.0}},
        .sinks = {{.x_coordinate = 97.0, .y_coordinate = 196.0}},
    };

    const auto converted = pre_c_sumo::convertCSumoToPrecice({pre_c_sumo::c_sumo::DiffuserSettings{}}, {result}, {});

    ASSERT_TRUE(converted.has_value()) << converted.error().message;
    ASSERT_EQ(converted->get_number_of_entries(), 1000u);
    EXPECT_NEAR(converted->data().source_x_vector.front(), 92.0, 1e-10);
    EXPECT_NEAR(converted->data().source_x_vector.back(), 108.0, 1e-10);
    EXPECT_NEAR(converted->get_discharge_value().front(), 0.01, 1e-12);
    EXPECT_NEAR(converted->get_discharge_value().back(), 0.01, 1e-12);
}

TEST(CSumoToPreciceTest, ClampsSourceAndIntakeWeightSumsBelowOne)
{
    const pre_c_sumo::c_sumo::NF2FFData result{
        .intake_flow_rate = 10.0,
        .source_flow_rate = 10.0,
        .intakes = {{.weight = 0.2, .has_weight = true}, {.weight = 0.3, .has_weight = true}},
        .sources = {{.weight = 0.2, .has_weight = true}, {.weight = 0.3, .has_weight = true}},
    };

    const auto converted = pre_c_sumo::convertCSumoToPrecice({pre_c_sumo::c_sumo::DiffuserSettings{}}, {result}, {});

    ASSERT_TRUE(converted.has_value()) << converted.error().message;
    const auto& discharges = converted->get_discharge_value();
    ASSERT_EQ(discharges.size(), 4u);
    EXPECT_NEAR(discharges[0], 2.0, 1e-12);
    EXPECT_NEAR(discharges[1], 3.0, 1e-12);
    EXPECT_NEAR(discharges[2], 2.0, 1e-12);
    EXPECT_NEAR(discharges[3], 3.0, 1e-12);
}

TEST(CSumoToPreciceTest, UsesSettingsIntakeWhenResultHasNone)
{
    pre_c_sumo::c_sumo::DiffuserSettings diffuser;
    diffuser.intake = pre_c_sumo::c_sumo::Point2D{3.0, 4.0};
    const pre_c_sumo::c_sumo::NF2FFData result{
        .intake_flow_rate = 10.0,
        .source_flow_rate = 10.0,
        .sources = {{.x_coordinate = 10.0}, {.x_coordinate = 20.0}},
    };

    const auto converted = pre_c_sumo::convertCSumoToPrecice({diffuser}, {result}, {});

    ASSERT_TRUE(converted.has_value()) << converted.error().message;
    ASSERT_EQ(converted->get_number_of_entries(), 3u);
    EXPECT_DOUBLE_EQ(converted->data().sink_x_vector[2], 3.0);
    EXPECT_DOUBLE_EQ(converted->data().sink_y_vector[2], 4.0);
    EXPECT_DOUBLE_EQ(converted->get_discharge_value()[2], 10.0);
}

TEST(CSumoToPreciceTest, RejectsDecreasingEntrainment)
{
    const pre_c_sumo::c_sumo::NF2FFData result{
        .source_flow_rate = 10.0,
        .sources = {{.x_coordinate = 10.0}, {.x_coordinate = 20.0}},
        .sinks = {{.entrainment = 5.0}, {.entrainment = 2.5}},
    };

    const auto converted = pre_c_sumo::convertCSumoToPrecice({pre_c_sumo::c_sumo::DiffuserSettings{}}, {result}, {});

    ASSERT_FALSE(converted.has_value());
    EXPECT_TRUE(converted.error().message.contains("Negative entrainment factor for sink 1"));
    EXPECT_TRUE(converted.error().message.contains("-2.5"));
}

TEST(CSumoToPreciceTest, ConvertsDiffusersIndependently)
{
    const pre_c_sumo::c_sumo::NF2FFData first{
        .source_flow_rate = 10.0,
        .sources = {{.x_coordinate = 1.0}, {.x_coordinate = 2.0}},
    };
    const pre_c_sumo::c_sumo::NF2FFData second{
        .source_flow_rate = 24.0,
        .sources = {{.x_coordinate = 101.0}, {.x_coordinate = 102.0}},
    };
    const std::vector<pre_c_sumo::c_sumo::DiffuserSettings> settings(2);

    const auto converted = pre_c_sumo::convertCSumoToPrecice(settings, {first, second}, {});

    ASSERT_TRUE(converted.has_value()) << converted.error().message;
    EXPECT_EQ(converted->data().source_x_vector, (std::vector<double>{1.0, 2.0, 101.0, 102.0}));
    EXPECT_EQ(converted->get_discharge_value(), (std::vector<double>{5.0, 5.0, 12.0, 12.0}));
}
