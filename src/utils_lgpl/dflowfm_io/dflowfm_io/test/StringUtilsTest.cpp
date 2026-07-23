#include <gtest/gtest.h>

#include <dflowfm_io/StringUtils.h>

namespace dflowfm_io::test
{

    // -------------------------------------------------------------------------
    // iequals
    // -------------------------------------------------------------------------

    TEST(StringUtilsTest, IEquals_IdenticalStrings_ReturnsTrue)
    {
        EXPECT_TRUE(iequals("hello", "hello"));
    }

    TEST(StringUtilsTest, IEquals_EmptyStrings_ReturnsTrue)
    {
        EXPECT_TRUE(iequals("", ""));
    }

    TEST(StringUtilsTest, IEquals_DifferentCase_ReturnsTrue)
    {
        EXPECT_TRUE(iequals("Hello", "hello"));
        EXPECT_TRUE(iequals("HELLO", "hello"));
        EXPECT_TRUE(iequals("hElLo", "HeLlO"));
    }

    TEST(StringUtilsTest, IEquals_AllUppercase_ReturnsTrue)
    {
        EXPECT_TRUE(iequals("HELLO", "HELLO"));
    }

    TEST(StringUtilsTest, IEquals_DifferentContent_ReturnsFalse)
    {
        EXPECT_FALSE(iequals("hello", "world"));
    }

    TEST(StringUtilsTest, IEquals_DifferentLength_ReturnsFalse)
    {
        EXPECT_FALSE(iequals("hello", "hell"));
        EXPECT_FALSE(iequals("hell", "hello"));
    }

    TEST(StringUtilsTest, IEquals_EmptyAndNonEmpty_ReturnsFalse)
    {
        EXPECT_FALSE(iequals("", "hello"));
        EXPECT_FALSE(iequals("hello", ""));
    }

    TEST(StringUtilsTest, IEquals_StringsWithDigits_ReturnsTrue)
    {
        EXPECT_TRUE(iequals("abc123", "ABC123"));
    }

    TEST(StringUtilsTest, IEquals_StringsWithDigits_DifferentDigits_ReturnsFalse)
    {
        EXPECT_FALSE(iequals("abc123", "abc456"));
    }

    TEST(StringUtilsTest, IEquals_StringsWithSpecialCharacters_ReturnsTrue)
    {
        EXPECT_TRUE(iequals("hello_world", "HELLO_WORLD"));
    }

    // -------------------------------------------------------------------------
    // tolower
    // -------------------------------------------------------------------------

    TEST(StringUtilsTest, ToLower_AllUppercase_ReturnsAllLowercase)
    {
        EXPECT_EQ(tolower("HELLO"), "hello");
    }

    TEST(StringUtilsTest, ToLower_AllLowercase_ReturnsSameString)
    {
        EXPECT_EQ(tolower("hello"), "hello");
    }

    TEST(StringUtilsTest, ToLower_MixedCase_ReturnsAllLowercase)
    {
        EXPECT_EQ(tolower("HeLlO"), "hello");
    }

    TEST(StringUtilsTest, ToLower_EmptyString_ReturnsEmptyString)
    {
        EXPECT_EQ(tolower(""), "");
    }

    TEST(StringUtilsTest, ToLower_StringWithDigits_DigitsUnchanged)
    {
        EXPECT_EQ(tolower("ABC123"), "abc123");
    }

    TEST(StringUtilsTest, ToLower_StringWithSpecialCharacters_SpecialCharactersUnchanged)
    {
        EXPECT_EQ(tolower("HELLO_WORLD"), "hello_world");
    }

    TEST(StringUtilsTest, ToLower_DoesNotModifyInput)
    {
        const std::string_view input = "HELLO";
        tolower(input);

        EXPECT_EQ(input, "HELLO");
    }

    TEST(StringUtilsTest, ToLower_ReturnsNewString)
    {
        const std::string result1 = tolower("HELLO");
        const std::string result2 = tolower("HELLO");

        EXPECT_EQ(result1, result2);
    }

    // -------------------------------------------------------------------------
    // FormatKey
    // -------------------------------------------------------------------------

    TEST(MduSchemaTest, FormatKey_ProducesLowercaseDotSeparatedKey)
    {
        EXPECT_EQ(FormatKey("General", "FileType"), "general.filetype");
    }

    TEST(MduSchemaTest, FormatKey_AlreadyLowerCase_Unchanged)
    {
        EXPECT_EQ(FormatKey("general", "filetype"), "general.filetype");
    }

} // namespace dflowfm_io::test