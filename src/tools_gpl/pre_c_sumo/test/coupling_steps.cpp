#include <gtest/gtest.h>

#include "test_utilities.hpp"
#include "coupling_steps.hpp"

// This test verifies that the do_timeloop function allows the loop to run the expected number of times.
// Since do_timeloop uses a static variable to count iterations, we can call it in a loop and count how many times
// it returns true.
TEST(CsumoPreciceCouplingStepsTest, DoTimeloopRunsExpectedNumberOfIterations)
{
    int iteration_count = 0;
    while (pre_c_sumo::doTimeloop())
    {
        iteration_count++;
    }
    EXPECT_EQ(iteration_count, 2); // We expect it to run 2 iterations based on the implementation
}

// This test verifies that the read_csumo_config_file function returns an error when given an invalid file path.
TEST(CsumoPreciceCouplingStepsTest, ReadCosumoConfigFile)
{
    auto result = pre_c_sumo::readCsumoSettingsFile("non_existent_file.xml");
    EXPECT_FALSE(result.has_value());
    EXPECT_PRED2(test_utilities::contains, result.error().message, "Cannot open file: ");
}

// This test verifies that the receive_ff_data function executes without throwing an exception.
TEST(CsumoPreciceCouplingStepsTest, ReceiveNFData) { EXPECT_NO_THROW(pre_c_sumo::receiveFFData()); }
