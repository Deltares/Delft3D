#include <gtest/gtest.h>

#include "connected_sinks_sources.hpp"

TEST(ConnectedSinkSourcesTest, CanInitializeAddItemAndClear)
{
    pre_c_sumo::ConnectedSinkSources connected = {};
    std::size_t size = connected.get_number_of_entries();
    ASSERT_EQ(size, 0);
    connected.add_entry(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, {});
    size = connected.get_number_of_entries();
    ASSERT_EQ(size, 1);
    connected.add_entry(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, {});
    size = connected.get_number_of_entries();
    ASSERT_EQ(size, 2);
    connected.clear();
    size = connected.get_number_of_entries();
    ASSERT_EQ(size, 0);
}

TEST(ConnectedSinkSourcesTest, KeepsAttributesAlignedAndClearsAllBuffersBeforeReuse)
{
    pre_c_sumo::ConnectedSinkSources connected;
    connected.add_entry(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, {12, 13});
    connected.add_entry(21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, {32});

    const auto& data = connected.data();
    EXPECT_EQ(data.sink_x_vector, (std::vector<double>{1, 21}));
    EXPECT_EQ(data.sink_y_vector, (std::vector<double>{2, 22}));
    EXPECT_EQ(data.sink_z_bottom_vector, (std::vector<double>{3, 23}));
    EXPECT_EQ(data.sink_z_top_vector, (std::vector<double>{4, 24}));
    EXPECT_EQ(data.source_x_vector, (std::vector<double>{5, 25}));
    EXPECT_EQ(data.source_y_vector, (std::vector<double>{6, 26}));
    EXPECT_EQ(data.source_z_bottom_vector, (std::vector<double>{7, 27}));
    EXPECT_EQ(data.source_z_top_vector, (std::vector<double>{8, 28}));
    EXPECT_EQ(data.discharge_vector, (std::vector<double>{9, 29}));
    EXPECT_EQ(data.momentum_magnitude_weighted_vector, (std::vector<double>{10, 30}));
    EXPECT_EQ(data.momentum_direction_vector, (std::vector<double>{11, 31}));
    EXPECT_EQ(data.constituents_vectors[0], (std::vector<double>{12, 32}));
    EXPECT_EQ(data.constituents_vectors[1], (std::vector<double>{13, 0}));
    for (std::size_t index = 2; index < data.constituents_vectors.size(); ++index)
    {
        EXPECT_EQ(data.constituents_vectors[index], (std::vector<double>{0, 0}));
    }

    connected.clear();
    connected.add_entry(41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, {});
    EXPECT_EQ(connected.get_number_of_entries(), 1u);
    EXPECT_EQ(data.sink_x_vector, (std::vector<double>{41}));
    EXPECT_EQ(data.sink_y_vector, (std::vector<double>{42}));
    EXPECT_EQ(data.sink_z_bottom_vector, (std::vector<double>{43}));
    EXPECT_EQ(data.sink_z_top_vector, (std::vector<double>{44}));
    EXPECT_EQ(data.source_x_vector, (std::vector<double>{45}));
    EXPECT_EQ(data.source_y_vector, (std::vector<double>{46}));
    EXPECT_EQ(data.source_z_bottom_vector, (std::vector<double>{47}));
    EXPECT_EQ(data.source_z_top_vector, (std::vector<double>{48}));
    EXPECT_EQ(data.discharge_vector, (std::vector<double>{49}));
    EXPECT_EQ(data.momentum_magnitude_weighted_vector, (std::vector<double>{50}));
    EXPECT_EQ(data.momentum_direction_vector, (std::vector<double>{51}));
    for (const auto& constituents : data.constituents_vectors)
    {
        EXPECT_EQ(constituents, (std::vector<double>{0}));
    }
}
