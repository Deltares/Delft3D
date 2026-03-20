#include <gtest/gtest.h>

#include "csumo_precice_lib.hpp"

// Basic test to verify the library function executes successfully
TEST(CsumoPreciceLibTest, BasicFunctionCallReturnsZero)
{
    int result = csumo_precice::run();
    EXPECT_EQ(result, 0);
}
