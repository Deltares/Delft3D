#include <gtest/gtest.h>

#include "csumo_precice_lib.hpp"

// Basic test to verify the library function executes successfully
TEST(CsumoPreciceLibTest, BasicFunctionCallReturnsZero)
{
    int result = csumo_precice::csumo_precice();
    EXPECT_EQ(result, 0);
}

// Main function for running all tests
int main(int argc, char** argv)
{
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
