#include <gtest/gtest.h>

#include "preC-SUMO_lib.hpp"

// Basic test to verify the library function executes successfully
TEST(CsumoPreciceLibTest, BasicFunctionCallReturnsZero)
{
    int result = preC-SUMO::run();
    EXPECT_EQ(result, 0);
}
