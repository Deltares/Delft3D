#include <gtest/gtest.h>

#include <array>
#include <string_view>
#include <vector>

#include "parsing_utils.hpp"

TEST(ParsingUtilsTest, ParseDoubleVectorAcceptsWhitespaceAndScientificNotation)
{
    const auto values = parsing_utils::parseDoubleVector("\n 1.25\t-2.5e+1\r\n 3E-2 \n", "values");

    ASSERT_TRUE(values.has_value()) << values.error().message;
    EXPECT_EQ(*values, (std::vector<double>{1.25, -25.0, 0.03}));
}

TEST(ParsingUtilsTest, ParseDoubleVectorRejectsPartiallyParsedTokens)
{
    constexpr std::array<std::string_view, 8> inputs = {
        "1junk", "1.2.3", "1e", "1e+", "1e-", "1e2junk", "10 20x 30", "1.0, 2.0"};

    for (const auto text : inputs)
    {
        SCOPED_TRACE(text);
        const auto values = parsing_utils::parseDoubleVector(text, "values");
        EXPECT_FALSE(values.has_value());
    }
}
