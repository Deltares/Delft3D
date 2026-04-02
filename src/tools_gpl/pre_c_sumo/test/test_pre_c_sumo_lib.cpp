#include <gtest/gtest.h>

#include "pre_c_sumo_lib.hpp"

// Basic test to verify the library function executes successfully
TEST(CsumoPreciceLibTest, BasicFunctionCallReturnsZero)
{
    int result = pre_c_sumo::run();
    EXPECT_EQ(result, 0);
}
