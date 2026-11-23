#include <gtest/gtest.h>

#include "csumo/diffuser.hpp"

TEST(CSumoDiffuserTest, DeterminesWhetherDiffuserIsModelledFromSourcesAndSinks)
{
    pre_c_sumo::c_sumo::NF2FFData diffuser;
    diffuser.sources.resize(1);
    EXPECT_TRUE(pre_c_sumo::c_sumo::isDiffuserModelled(diffuser));

    diffuser.sinks.resize(1);
    EXPECT_FALSE(pre_c_sumo::c_sumo::isDiffuserModelled(diffuser));

    diffuser.sources.resize(2);
    EXPECT_TRUE(pre_c_sumo::c_sumo::isDiffuserModelled(diffuser));

    diffuser.sinks.clear();
    EXPECT_TRUE(pre_c_sumo::c_sumo::isDiffuserModelled(diffuser));
}

TEST(CSumoDiffuserTest, CreatesPerpendicularTrackUsingLastSinkAndPreservesSourceProperties)
{
    pre_c_sumo::c_sumo::NF2FFData diffuser;
    diffuser.sinks = {{.x_coordinate = 99.0, .y_coordinate = 200.0},
                      {.x_coordinate = 97.0, .y_coordinate = 196.0}};
    diffuser.sources = {{.x_coordinate = 100.0,
                         .y_coordinate = 200.0,
                         .z_coordinate = 5.0,
                         .entrainment = 2.0,
                         .half_plume_height = 3.0,
                         .half_plume_width = 10.0,
                         .u_magnitude = 4.0,
                         .u_direction = 45.0,
                         .has_u = true}};

    const auto sources = pre_c_sumo::c_sumo::createDiffuserModel(diffuser);

    ASSERT_EQ(sources.size(), 1000u);
    EXPECT_NEAR(sources.front().x_coordinate, 92.0, 1e-10);
    EXPECT_NEAR(sources.front().y_coordinate, 206.0, 1e-10);
    EXPECT_NEAR(sources.back().x_coordinate, 108.0, 1e-10);
    EXPECT_NEAR(sources.back().y_coordinate, 194.0, 1e-10);
    double total_weight = 0.0;
    for (const auto& source : sources)
    {
        EXPECT_DOUBLE_EQ(source.z_coordinate, 5.0);
        EXPECT_DOUBLE_EQ(source.entrainment, 2.0);
        EXPECT_DOUBLE_EQ(source.half_plume_height, 3.0);
        EXPECT_DOUBLE_EQ(source.half_plume_width, 0.0);
        EXPECT_DOUBLE_EQ(source.u_magnitude, 4.0);
        EXPECT_DOUBLE_EQ(source.u_direction, 45.0);
        EXPECT_DOUBLE_EQ(source.weight, 0.001);
        EXPECT_TRUE(source.has_u);
        EXPECT_TRUE(source.has_weight);
        total_weight += source.weight;
    }
    EXPECT_NEAR(total_weight, 1.0, 1e-12);
}

TEST(CSumoDiffuserTest, ZeroWidthKeepsAllSourcesAtOriginalPositionWithoutVelocity)
{
    pre_c_sumo::c_sumo::NF2FFData diffuser;
    diffuser.sinks = {{.x_coordinate = 1.0, .y_coordinate = 0.0}};
    diffuser.sources = {{.x_coordinate = 2.0, .y_coordinate = 3.0, .z_coordinate = 4.0}};

    const auto sources = pre_c_sumo::c_sumo::createDiffuserModel(diffuser);

    ASSERT_EQ(sources.size(), 1000u);
    for (const auto& source : sources)
    {
        EXPECT_DOUBLE_EQ(source.x_coordinate, 2.0);
        EXPECT_DOUBLE_EQ(source.y_coordinate, 3.0);
        EXPECT_DOUBLE_EQ(source.z_coordinate, 4.0);
        EXPECT_FALSE(source.has_u);
    }
}
