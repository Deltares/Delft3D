#include <gtest/gtest.h>

#include "precice_data.hpp"

TEST(SourcesSinksTest, RegistersPlaceholderVertexWhenThereAreNoEntries)
{
    pre_c_sumo::SourcesSinks sources_sinks;

    sources_sinks.setVertexCount(0);

    EXPECT_EQ(sources_sinks.precice_ids.size(), 1u);
    EXPECT_EQ(sources_sinks.coordinates.size(), 2u);
}

TEST(SourcesSinksTest, SizesCoordinatesForRequestedVertexCount)
{
    pre_c_sumo::SourcesSinks sources_sinks;

    sources_sinks.setVertexCount(3);

    EXPECT_EQ(sources_sinks.precice_ids.size(), 3u);
    EXPECT_EQ(sources_sinks.coordinates.size(), 6u);
}

TEST(SourcesSinksTest, AddDataAppendsValuesToMatchingBuffers)
{
    pre_c_sumo::SourcesSinks sources_sinks;

    sources_sinks.addData(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0);
    sources_sinks.addData(11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0, 19.0);

    EXPECT_EQ(sources_sinks.sinks_x, (std::vector<double>{1.0, 11.0}));
    EXPECT_EQ(sources_sinks.sinks_y, (std::vector<double>{2.0, 12.0}));
    EXPECT_EQ(sources_sinks.sinks_z_min, (std::vector<double>{3.0, 13.0}));
    EXPECT_EQ(sources_sinks.sinks_z_max, (std::vector<double>{4.0, 14.0}));
    EXPECT_EQ(sources_sinks.sources_x, (std::vector<double>{5.0, 15.0}));
    EXPECT_EQ(sources_sinks.sources_y, (std::vector<double>{6.0, 16.0}));
    EXPECT_EQ(sources_sinks.sources_z_min, (std::vector<double>{7.0, 17.0}));
    EXPECT_EQ(sources_sinks.sources_z_max, (std::vector<double>{8.0, 18.0}));
    EXPECT_EQ(sources_sinks.discharges, (std::vector<double>{9.0, 19.0}));
}

TEST(SourcesSinksTest, ClearDataPreservesMeshBuffers)
{
    pre_c_sumo::SourcesSinks sources_sinks;
    sources_sinks.setVertexCount(2);
    sources_sinks.addData(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0);

    sources_sinks.clearData();

    EXPECT_EQ(sources_sinks.coordinates.size(), 4u);
    EXPECT_EQ(sources_sinks.precice_ids.size(), 2u);
    EXPECT_TRUE(sources_sinks.sinks_x.empty());
    EXPECT_TRUE(sources_sinks.sinks_y.empty());
    EXPECT_TRUE(sources_sinks.sinks_z_min.empty());
    EXPECT_TRUE(sources_sinks.sinks_z_max.empty());
    EXPECT_TRUE(sources_sinks.sources_x.empty());
    EXPECT_TRUE(sources_sinks.sources_y.empty());
    EXPECT_TRUE(sources_sinks.sources_z_min.empty());
    EXPECT_TRUE(sources_sinks.sources_z_max.empty());
    EXPECT_TRUE(sources_sinks.discharges.empty());
}

TEST(SourcesSinksTest, ClearRemovesMeshAndDataBuffers)
{
    pre_c_sumo::SourcesSinks sources_sinks;
    sources_sinks.setVertexCount(2);
    sources_sinks.addData(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0);

    sources_sinks.clear();

    EXPECT_TRUE(sources_sinks.coordinates.empty());
    EXPECT_TRUE(sources_sinks.precice_ids.empty());
    EXPECT_TRUE(sources_sinks.sinks_x.empty());
    EXPECT_TRUE(sources_sinks.sinks_y.empty());
    EXPECT_TRUE(sources_sinks.sinks_z_min.empty());
    EXPECT_TRUE(sources_sinks.sinks_z_max.empty());
    EXPECT_TRUE(sources_sinks.sources_x.empty());
    EXPECT_TRUE(sources_sinks.sources_y.empty());
    EXPECT_TRUE(sources_sinks.sources_z_min.empty());
    EXPECT_TRUE(sources_sinks.sources_z_max.empty());
    EXPECT_TRUE(sources_sinks.discharges.empty());
}

TEST(ZSpecificationTest, UsesTwoDimensionsAndOneZCoordinateIn2D)
{
    const pre_c_sumo::ZSpecification z_spec{true, -10.0, 2.0, 10.0};

    EXPECT_EQ(z_spec.numberOfDimensions(), 2u);
    EXPECT_EQ(z_spec.numberOfZCoordinates(), 1u);
    EXPECT_DOUBLE_EQ(z_spec.zCoordinateAt(0), 0.0);
    EXPECT_DOUBLE_EQ(z_spec.zCoordinateAt(3), 0.0);
}

TEST(ZSpecificationTest, Calculates3DCoordinateCountAndValues)
{
    const pre_c_sumo::ZSpecification z_spec{false, -10.0, 2.5, 0.0};

    EXPECT_EQ(z_spec.numberOfDimensions(), 3u);
    EXPECT_EQ(z_spec.numberOfZCoordinates(), 5u);
    EXPECT_DOUBLE_EQ(z_spec.zCoordinateAt(0), -10.0);
    EXPECT_DOUBLE_EQ(z_spec.zCoordinateAt(4), 0.0);
}
