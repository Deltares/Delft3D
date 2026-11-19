#include <gtest/gtest.h>

#include "precice_data.hpp"

TEST(PreciceSinkSourceConnectionsMeshTest, RegistersPlaceholderVertexWhenThereAreNoEntries)
{
    pre_c_sumo::precice::SinkSourceConnectionsMesh sink_source_connections_mesh;

    sink_source_connections_mesh.setVertexCount(0);

    EXPECT_EQ(sink_source_connections_mesh.precice_ids.size(), 1u);
    EXPECT_EQ(sink_source_connections_mesh.coordinates.size(), 2u);
}

TEST(PreciceSinkSourceConnectionsMeshTest, SizesCoordinatesForRequestedVertexCount)
{
    pre_c_sumo::precice::SinkSourceConnectionsMesh sink_source_connections_mesh;

    sink_source_connections_mesh.setVertexCount(3);

    EXPECT_EQ(sink_source_connections_mesh.precice_ids.size(), 3u);
    EXPECT_EQ(sink_source_connections_mesh.coordinates.size(), 6u);
}

TEST(PreciceZSpecificationTest, UsesTwoDimensionsAndOneZCoordinateIn2D)
{
    const pre_c_sumo::precice::ZSpecification z_spec{true, -10.0, 2.0, 10.0};

    EXPECT_EQ(z_spec.numberOfDimensions(), 2u);
    EXPECT_EQ(z_spec.numberOfZCoordinates(), 1u);
    EXPECT_DOUBLE_EQ(z_spec.zCoordinateAt(0), 0.0);
    EXPECT_DOUBLE_EQ(z_spec.zCoordinateAt(3), 0.0);
}

TEST(PreciceZSpecificationTest, Calculates3DCoordinateCountAndValues)
{
    const pre_c_sumo::precice::ZSpecification z_spec{false, -10.0, 2.5, 0.0};

    EXPECT_EQ(z_spec.numberOfDimensions(), 3u);
    EXPECT_EQ(z_spec.numberOfZCoordinates(), 5u);
    EXPECT_DOUBLE_EQ(z_spec.zCoordinateAt(0), -10.0);
    EXPECT_DOUBLE_EQ(z_spec.zCoordinateAt(4), 0.0);
}
