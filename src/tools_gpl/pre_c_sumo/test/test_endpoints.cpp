#include <gtest/gtest.h>

#include "endpoints.hpp"

namespace
{
    /// Returns a Source initialised with a fixed, representative set of values.
    /// @param discharge Override the discharge to test negative-discharge paths.
    pre_c_sumo::Source makeTestSource(double discharge = 12.5)
    {
        pre_c_sumo::Source source{};
        pre_c_sumo::makeEndpoint(source, 7, 100.5, 200.75, -4.0, -2.0, discharge, 11);
        return source;
    }

    /// Returns a Sink initialised with a fixed, representative set of values.
    pre_c_sumo::Sink makeTestSink()
    {
        pre_c_sumo::Sink sink{};
        pre_c_sumo::makeEndpoint(sink, 3, 10.0, 20.0, -6.0, -6.0, -5.25);
        return sink;
    }
} // namespace

TEST(EndpointsTest, DefaultConstructEndpointHasZeroInitializedFields)
{
    const pre_c_sumo::Endpoint endpoint{};

    EXPECT_EQ(endpoint.id, 0);
    EXPECT_EQ(endpoint.connected_id, -1);
    EXPECT_DOUBLE_EQ(endpoint.coordinate_x, 0.0);
    EXPECT_DOUBLE_EQ(endpoint.coordinate_y, 0.0);
    EXPECT_DOUBLE_EQ(endpoint.vertical_boundary_lower, 0.0);
    EXPECT_DOUBLE_EQ(endpoint.vertical_boundary_upper, 0.0);
    EXPECT_DOUBLE_EQ(endpoint.discharge, 0.0);
}

TEST(EndpointsTest, DefaultConstructSourceOptionalsAreEmpty)
{
    const pre_c_sumo::Source source{};

    EXPECT_FALSE(source.momentum.has_value());
    EXPECT_FALSE(source.constituents.has_value());
}

TEST(EndpointsTest, MakeEndpointConstructsSourceWithDefaulTestValues)
{
    const pre_c_sumo::Source source = makeTestSource();

    EXPECT_EQ(source.id, 7);
    EXPECT_EQ(source.connected_id, 11);
    EXPECT_DOUBLE_EQ(source.coordinate_x, 100.5);
    EXPECT_DOUBLE_EQ(source.coordinate_y, 200.75);
    EXPECT_DOUBLE_EQ(source.vertical_boundary_lower, -4.0);
    EXPECT_DOUBLE_EQ(source.vertical_boundary_upper, -2.0);
    EXPECT_DOUBLE_EQ(source.discharge, 12.5);
    EXPECT_FALSE(source.momentum.has_value());
    EXPECT_FALSE(source.constituents.has_value());
}

TEST(EndpointsTest, MakeEndpointConstructsSinkWithDefaulTestValues)
{
    const pre_c_sumo::Sink sink = makeTestSink();

    EXPECT_EQ(sink.id, 3);
    EXPECT_EQ(sink.connected_id, -1);
    EXPECT_DOUBLE_EQ(sink.coordinate_x, 10.0);
    EXPECT_DOUBLE_EQ(sink.coordinate_y, 20.0);
    EXPECT_DOUBLE_EQ(sink.vertical_boundary_lower, -6.0);
    EXPECT_DOUBLE_EQ(sink.vertical_boundary_upper, -6.0);
    EXPECT_DOUBLE_EQ(sink.discharge, -5.25);
}

TEST(EndpointsTest, AddMomentumAttachesToNonNegativeDischargeSource)
{
    pre_c_sumo::Source source = makeTestSource();

    const bool attached = pre_c_sumo::addMomentum(source, pre_c_sumo::makeMomentum(2.1, 135.0));

    EXPECT_TRUE(attached);
    ASSERT_TRUE(source.momentum.has_value());
    EXPECT_DOUBLE_EQ(source.momentum->velocity_magnitude, 2.1);
    EXPECT_DOUBLE_EQ(source.momentum->velocity_direction_deg, 135.0);
}

TEST(EndpointsTest, AddMomentumRejectsNegativeDischargeSource)
{
    pre_c_sumo::Source source = makeTestSource(-12.5);

    const bool attached = pre_c_sumo::addMomentum(source, pre_c_sumo::makeMomentum(2.1, 135.0));

    EXPECT_FALSE(attached);
    EXPECT_FALSE(source.momentum.has_value());
}

TEST(EndpointsTest, AddConstituentsAttachesToNonNegativeDischargeSource)
{
    pre_c_sumo::Source source = makeTestSource();

    const bool attached = pre_c_sumo::addConstituents(
        source, pre_c_sumo::makeConstituents(18.5, 30.2, {1.0, 2.0, 3.0}));

    EXPECT_TRUE(attached);
    ASSERT_TRUE(source.constituents.has_value());
    EXPECT_DOUBLE_EQ(source.constituents->temperature, 18.5);
    EXPECT_DOUBLE_EQ(source.constituents->salinity, 30.2);
}

TEST(EndpointsTest, AddConstituentsRejectsNegativeDischargeSource)
{
    pre_c_sumo::Source source = makeTestSource(-12.5);

    const bool attached = pre_c_sumo::addConstituents(
        source, pre_c_sumo::makeConstituents(18.5, 30.2, {1.0, 2.0, 3.0}));

    EXPECT_FALSE(attached);
    EXPECT_FALSE(source.constituents.has_value());
}

TEST(EndpointsTest, SourceCanStoreMomentumAndConstituents)
{
    pre_c_sumo::Source source = makeTestSource();

    source.momentum = pre_c_sumo::makeMomentum(2.1, 135.0);
    source.constituents = pre_c_sumo::makeConstituents(18.5, 30.2, {1.0, 2.0, 3.0});

    ASSERT_TRUE(source.momentum.has_value());
    EXPECT_DOUBLE_EQ(source.momentum->velocity_magnitude, 2.1);
    EXPECT_DOUBLE_EQ(source.momentum->velocity_direction_deg, 135.0);

    ASSERT_TRUE(source.constituents.has_value());
    EXPECT_DOUBLE_EQ(source.constituents->temperature, 18.5);
    EXPECT_DOUBLE_EQ(source.constituents->salinity, 30.2);
    ASSERT_EQ(source.constituents->additional_constituents.size(), pre_c_sumo::constituent_count);
    EXPECT_DOUBLE_EQ(source.constituents->additional_constituents[0], 1.0);
    EXPECT_DOUBLE_EQ(source.constituents->additional_constituents[1], 2.0);
    EXPECT_DOUBLE_EQ(source.constituents->additional_constituents[2], 3.0);
    EXPECT_DOUBLE_EQ(source.constituents->additional_constituents[3], 0.0);
    EXPECT_DOUBLE_EQ(source.constituents->additional_constituents[4], 0.0);
}

TEST(EndpointsTest, SourceAndSinkConnectedToEachOther)
{
    pre_c_sumo::Source source{};
    source.id = 1;
    source.connected_id = 2;

    pre_c_sumo::Sink sink{};
    sink.id = source.connected_id;
    sink.connected_id = source.id;

    const pre_c_sumo::Endpoint& source_endpoint = source;
    const pre_c_sumo::Endpoint& sink_endpoint = sink;

    EXPECT_EQ(source_endpoint.id, 1);
    EXPECT_EQ(source_endpoint.connected_id, 2);
    EXPECT_EQ(sink_endpoint.id, 2);
    EXPECT_EQ(sink_endpoint.connected_id, 1);
}
