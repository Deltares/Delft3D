#include <gtest/gtest.h>

#include "test_utilities.hpp"
#include "pre_c_sumo_lib.hpp"
namespace
{
    const auto starts_with = [](const std::string& message, const std::string& prefix) {
        return message.find(prefix) == 0;
    };
    const auto contains = [](const std::string& message, const std::string& substring) {
        return message.find(substring) != std::string::npos;
    };
} // namespace

// Basic test to verify the library function executes successfully
TEST(CsumoPreciceLibTest, BasicFunctionCallReturnsZero)
{
    int result = pre_c_sumo::run();
    EXPECT_EQ(result, 0);
}

TEST(CsumoPreciceLibTest, DoTimeloopRunsExpectedNumberOfIterations)
{
    // This test verifies that the do_timeloop function allows the loop to run the expected number of times.
    // Since do_timeloop uses a static variable to count iterations, we can call it in a loop and count how many times
    // it returns true.
    int iteration_count = 0;
    while (pre_c_sumo::do_timeloop())
    {
        iteration_count++;
    }
    EXPECT_EQ(iteration_count, 2); // We expect it to run 2 iterations based on the implementation
}

TEST(CsumoPreciceLibTest, ReadCosumoConfigFile)
{
    // This test verifies that the read_csumo_config_file function returns an error when given an invalid file path.
    auto result = pre_c_sumo::read_csumo_config_file("non_existent_file.xml");
    EXPECT_FALSE(result.has_value());
    EXPECT_PRED2(contains, result.error().message, "Cannot open file: ");
}
