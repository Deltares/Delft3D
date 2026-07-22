#include <gtest/gtest.h>

#include <string_view>

#include "test_utilities.hpp"
#include "connected_sinks_sources.hpp"

TEST(ConnectedSinkSourcesTest, CanInitializeAddItemAndClear)
{
    pre_c_sumo::ConnectedSinkSources connected = {};
    std::size_t size = connected.get_number_of_entries();
    ASSERT_EQ(size, 0);
    connected.add_entry(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
    size = connected.get_number_of_entries();
    ASSERT_EQ(size, 1);
    connected.add_entry(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
    size = connected.get_number_of_entries();
    ASSERT_EQ(size, 2);
    connected.clear();
    size = connected.get_number_of_entries();
    ASSERT_EQ(size, 0);
}
