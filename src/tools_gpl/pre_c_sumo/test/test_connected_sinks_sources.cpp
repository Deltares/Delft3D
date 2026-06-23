#include <gtest/gtest.h>

#include <string_view>

#include "test_utilities.hpp"
#include "connected_sinks_sources.hpp"

namespace
{

} // namespace

TEST(ConnectedSinkSourcesTest, CanInitializeAddItemAndClear)
{
    pre_c_sumo::ConnectedSinkSources connected = {};
    ASSERT_EQ(connected.size(), 0);
    connected.add_entry(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
    ASSERT_EQ(connected.size(), 1);
    connected.add_entry(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
    ASSERT_EQ(connected.size(), 2);
    connected.clear();
    ASSERT_EQ(connected.size(), 0);
}
