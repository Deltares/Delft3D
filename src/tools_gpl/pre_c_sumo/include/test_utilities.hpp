#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_TEST_UTILITIES_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_TEST_UTILITIES_HPP

#include <gtest/gtest.h>

namespace test_utilities
{
    // TODO: no copy paste please, consider moving these to a shared test utilities header if needed in multiple places
    /**
     * @anchor test_utilities_starts_with
     * @brief Returns true when a message begins with the given prefix.
     * @param message Text to inspect.
     * @param prefix Expected leading substring.
     * @return True when the message starts with the prefix.
     */
    const auto starts_with = [](const std::string& message, const std::string& prefix) {
        return message.find(prefix) == 0;
    };

    /**
     * @anchor test_utilities_contains
     * @brief Returns true when a message contains the given substring.
     * @param message Text to inspect.
     * @param substring Substring to look for.
     * @return True when the substring is found inside the message.
     */
    const auto contains = [](const std::string& message, const std::string& substring) {
        return message.find(substring) != std::string::npos;
    };
} // namespace test_utilities

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_TEST_UTILITIES_HPP
