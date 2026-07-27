#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_TEST_UTILITIES_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_TEST_UTILITIES_HPP

#include <gtest/gtest.h>

namespace test_utilities
{
    // TODO: no copy paste please, consider moving these to a shared test utilities header if needed in multiple places
    const auto starts_with = [](const std::string& message, const std::string& prefix) {
        return message.find(prefix) == 0;
    };
    const auto contains = [](const std::string& message, const std::string& substring) {
        return message.find(substring) != std::string::npos;
    };
} // namespace test_utilities

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_TEST_UTILITIES_HPP
