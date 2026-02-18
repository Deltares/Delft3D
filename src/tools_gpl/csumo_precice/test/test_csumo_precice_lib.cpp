#include <gtest/gtest.h>
#include "csumo_precice_lib.hpp"

// Test fixture for csumo_precice_lib tests
class CsumoPreciceLibTest : public ::testing::Test {
protected:
    void SetUp() override {
        // Setup code that runs before each test
    }

    void TearDown() override {
        // Cleanup code that runs after each test
    }
};

// Basic test to verify the library function executes successfully
TEST_F(CsumoPreciceLibTest, BasicFunctionCallReturnsZero) {
    int result = csumo_precice::csumo_precice();
    EXPECT_EQ(result, 0);
}


// Main function for running all tests
int main(int argc, char **argv) {
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
