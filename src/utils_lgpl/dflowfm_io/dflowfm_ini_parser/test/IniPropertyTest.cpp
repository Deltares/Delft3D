#include <gtest/gtest.h>
#include <stdexcept>
#include <string>
#include <vector>

#include "ini/IniProperty.h"
#include "ini/IniValueConverter.h"

namespace ini::test
{
    TEST(IniPropertyTest, GetValue_RawValueIsPreservedForExplicitConversion)
    {
        const IniProperty property("TestKey", " 42 ");

        EXPECT_EQ(property.GetValue(), " 42 ");
        EXPECT_EQ(IniValueConverter::IntegerFromString(property.GetValue()), 42);
    }

    TEST(IniPropertyTest, GetValue_InvalidValueIsPreservedAndNamedConverterThrows)
    {
        const IniProperty property("TestKey", "not-a-number");

        EXPECT_EQ(property.GetValue(), "not-a-number");
        EXPECT_THROW(IniValueConverter::FloatingPointFromString(property.GetValue()), std::invalid_argument);
    }

    TEST(IniPropertyTest, SetValue_TimePointUsesExplicitNamedConverters)
    {
        IniProperty property("TestKey");
        const auto date = std::chrono::sys_days{std::chrono::year{2023} / 8 / 14};

        property.SetValue(IniValueConverter::TimePointToString(date, TimePointFormat::IsoDate));

        EXPECT_EQ(property.GetValue(), "2023-08-14");
        EXPECT_EQ(IniValueConverter::TimePointFromString(property.GetValue(), TimePointFormat::IsoDate), date);
        EXPECT_THROW(IniValueConverter::TimePointFromString(property.GetValue(), TimePointFormat::SlashDate),
                     std::invalid_argument);
    }

    // -------------------------------------------------------------------------
    // Constructors
    // -------------------------------------------------------------------------

    TEST(IniPropertyTest, Constructor_EmptyKey_ThrowsInvalidArgument)
    {
        EXPECT_THROW(IniProperty(""), std::invalid_argument);
    }

    TEST(IniPropertyTest, Constructor_ValidKey_InitializesProperties)
    {
        IniProperty property("TestKey");

        EXPECT_EQ(property.GetValue(), "");
        EXPECT_EQ(property.GetComment(), "");
        EXPECT_EQ(property.GetLineNumber(), 0);
    }

    TEST(IniPropertyTest, Constructor_ValidValues_InitializesProperties)
    {
        IniProperty property("TestProperty", "TestValue", "TestComment");

        EXPECT_EQ(property.GetKey(), "TestProperty");
        EXPECT_EQ(property.GetValue(), "TestValue");
        EXPECT_EQ(property.GetComment(), "TestComment");
    }

    TEST(IniPropertyTest, Constructor_EmptyValueAndComment_InitializesProperties)
    {
        IniProperty property("TestProperty", "", "");

        EXPECT_EQ(property.GetKey(), "TestProperty");
        EXPECT_EQ(property.GetValue(), "");
        EXPECT_EQ(property.GetComment(), "");
    }

    TEST(IniPropertyTest, Constructor_EmptyKeyAndValidProperty_ThrowsInvalidArgument)
    {
        IniProperty other("TestKey");

        EXPECT_THROW(IniProperty("", std::move(other)), std::invalid_argument);
    }

    TEST(IniPropertyTest, Constructor_ValidProperty_InitializesProperties)
    {
        IniProperty other("TestKey", "TestValue", "TestComment");
        other.SetLineNumber(10);

        IniProperty property("OtherKey", std::move(other));

        EXPECT_EQ(property.GetKey(), "OtherKey");
        EXPECT_EQ(property.GetValue(), "TestValue");
        EXPECT_EQ(property.GetComment(), "TestComment");
        EXPECT_EQ(property.GetLineNumber(), 10);
    }

    // -------------------------------------------------------------------------
    // Create - key/value
    // -------------------------------------------------------------------------

    TEST(IniPropertyTest, Create_EmptyKey_ThrowsInvalidArgument)
    {
        EXPECT_THROW(IniProperty("", std::string("TestValue")), std::invalid_argument);
    }

    TEST(IniPropertyTest, Create_EmptyStringValue_CreatesPropertyWithEmptyValue)
    {
        IniProperty property = IniProperty("TestKey", std::string(""));

        EXPECT_EQ(property.GetKey(), "TestKey");
        EXPECT_EQ(property.GetValue(), "");
    }

    TEST(IniPropertyTest, Create_ValidIntValue_CreatesProperty)
    {
        IniProperty property = IniProperty("TestKey", IniValueConverter::IntegerToString(42));

        EXPECT_EQ(property.GetKey(), "TestKey");
        EXPECT_EQ(property.GetValue(), "42");
    }

    TEST(IniPropertyTest, Create_ValidFloatValue_CreatesProperty)
    {
        IniProperty property = IniProperty("TestKey", IniValueConverter::FloatingPointToString(1.01f));

        EXPECT_EQ(property.GetKey(), "TestKey");
        EXPECT_EQ(property.GetValue(), "1.01");
    }

    TEST(IniPropertyTest, Create_ValidDoubleValue_CreatesProperty)
    {
        IniProperty property = IniProperty("TestKey", IniValueConverter::FloatingPointToString(2.71));

        EXPECT_EQ(property.GetKey(), "TestKey");
        EXPECT_EQ(property.GetValue(), "2.71");
    }

    TEST(IniPropertyTest, Create_ValidStringValue_CreatesProperty)
    {
        IniProperty property = IniProperty("TestKey", std::string("TestValue"));

        EXPECT_EQ(property.GetKey(), "TestKey");
        EXPECT_EQ(property.GetValue(), "TestValue");
    }

    // -------------------------------------------------------------------------
    // Create - key/values
    // -------------------------------------------------------------------------

    TEST(IniPropertyTest, Create_EmptyKeyAndEmptyValues_ThrowsInvalidArgument)
    {
        EXPECT_THROW(IniProperty("", IniValueConverter::StringListToString(std::vector<std::string>{})), std::invalid_argument);
    }

    TEST(IniPropertyTest, Create_EmptyValues_ThrowsInvalidArgument)
    {
        IniProperty property = IniProperty("TestKey", IniValueConverter::StringListToString(std::vector<std::string>{}));

        EXPECT_EQ(property.GetKey(), "TestKey");
        EXPECT_EQ(property.GetValue(), "");
    }

    TEST(IniPropertyTest, Create_ValidValuesAndSpaceSeparator_CreatesProperty)
    {
        IniProperty property = IniProperty("TestKey", IniValueConverter::IntegerListToString(std::vector<int>{8, 2, 3}, ' '));

        EXPECT_EQ(property.GetKey(), "TestKey");
        EXPECT_EQ(property.GetValue(), "8 2 3");
    }

    TEST(IniPropertyTest, Create_ValidValuesAndSemicolonSeparator_CreatesProperty)
    {
        IniProperty property = IniProperty("TestKey", IniValueConverter::IntegerListToString(std::vector<int>{8, 2, 3}, ';'));

        EXPECT_EQ(property.GetKey(), "TestKey");
        EXPECT_EQ(property.GetValue(), "8;2;3");
    }

    // -------------------------------------------------------------------------
    // HasValue
    // -------------------------------------------------------------------------

    TEST(IniPropertyTest, HasValue_ValidValue_ReturnsTrue)
    {
        IniProperty property("TestKey", "TestValue");

        EXPECT_TRUE(property.HasValue());
    }

    TEST(IniPropertyTest, HasValue_EmptyValue_ReturnsFalse)
    {
        IniProperty property("TestKey", "");

        EXPECT_FALSE(property.HasValue());
    }

    // -------------------------------------------------------------------------
    // HasComment
    // -------------------------------------------------------------------------

    TEST(IniPropertyTest, HasComment_EmptyComment_ReturnsFalse)
    {
        IniProperty property("TestKey", "TestValue", "");

        EXPECT_FALSE(property.HasComment());
    }

    TEST(IniPropertyTest, HasComment_ValidComment_ReturnsTrue)
    {
        IniProperty property("TestKey", "TestValue", "TestComment");

        EXPECT_TRUE(property.HasComment());
    }

    // -------------------------------------------------------------------------
    // SetValue
    // -------------------------------------------------------------------------

    TEST(IniPropertyTest, SetValue_EmptyStringValue_ValueIsSetToEmptyString)
    {
        IniProperty property("TestKey");

        property.SetValue(std::string(""));

        EXPECT_EQ(property.GetValue(), "");
    }

    TEST(IniPropertyTest, SetValue_ValidIntValue_UpdatesValue)
    {
        IniProperty property("TestKey");

        property.SetValue(IniValueConverter::IntegerToString(11));

        EXPECT_EQ(property.GetValue(), "11");
    }

    TEST(IniPropertyTest, SetValue_ValidFloatValue_UpdatesValue)
    {
        IniProperty property("TestKey");

        property.SetValue(IniValueConverter::FloatingPointToString(12.33f));

        EXPECT_EQ(property.GetValue(), "12.33");
    }

    TEST(IniPropertyTest, SetValue_ValidDoubleValue_UpdatesValue)
    {
        IniProperty property("TestKey");

        property.SetValue(IniValueConverter::FloatingPointToString(0.123));

        EXPECT_EQ(property.GetValue(), "0.123");
    }

    TEST(IniPropertyTest, SetValue_ValidStringValue_UpdatesValue)
    {
        IniProperty property("TestKey");

        property.SetValue(std::string("TestValue"));

        EXPECT_EQ(property.GetValue(), "TestValue");
    }

    // -------------------------------------------------------------------------
    // SetValues
    // -------------------------------------------------------------------------

    TEST(IniPropertyTest, SetValues_EmptyCollection_ValueIsSetToEmptyString)
    {
        IniProperty property("TestKey");

        property.SetValue(IniValueConverter::IntegerListToString(std::vector<int>{}));

        EXPECT_EQ(property.GetValue(), "");
    }

    TEST(IniPropertyTest, SetValues_ValidValuesAndSpaceSeparator_UpdatesValue)
    {
        IniProperty property("TestKey");

        property.SetValue(IniValueConverter::IntegerListToString(std::vector<int>{5, 1, 8}, ' '));

        EXPECT_EQ(property.GetValue(), "5 1 8");
    }

    TEST(IniPropertyTest, SetValues_ValidValuesAndSemicolonSeparator_UpdatesValue)
    {
        IniProperty property("TestKey");

        property.SetValue(IniValueConverter::IntegerListToString(std::vector<int>{5, 1, 8}, ';'));

        EXPECT_EQ(property.GetValue(), "5;1;8");
    }

    // -------------------------------------------------------------------------
    // IsKeyEqualTo
    // -------------------------------------------------------------------------

    TEST(IniPropertyTest, IsKeyEqualTo_EmptyKey_ThrowsInvalidArgument)
    {
        IniProperty property("TestKey");

        EXPECT_THROW(property.IsKeyEqualTo(""), std::invalid_argument);
    }

    TEST(IniPropertyTest, IsKeyEqualTo_SameLowercaseKey_ReturnsTrue)
    {
        IniProperty property("TestKey");

        EXPECT_TRUE(property.IsKeyEqualTo("testkey"));
    }

    TEST(IniPropertyTest, IsKeyEqualTo_SameMixedCaseKey_ReturnsTrue)
    {
        IniProperty property("TestKey");

        EXPECT_TRUE(property.IsKeyEqualTo("TestKey"));
    }

    TEST(IniPropertyTest, IsKeyEqualTo_SameUppercaseKey_ReturnsTrue)
    {
        IniProperty property("TestKey");

        EXPECT_TRUE(property.IsKeyEqualTo("TESTKEY"));
    }

    TEST(IniPropertyTest, IsKeyEqualTo_DifferentKey_ReturnsFalse)
    {
        IniProperty property("TestKey");

        EXPECT_FALSE(property.IsKeyEqualTo("OtherKey"));
    }

    // -------------------------------------------------------------------------
    // Equality
    // -------------------------------------------------------------------------

    TEST(IniPropertyTest, Equality_SamePropertyReference_ReturnsTrue)
    {
        IniProperty property("TestKey");

        EXPECT_EQ(property, property);
    }

    TEST(IniPropertyTest, Equality_SamePropertiesCaseInsensitive_ReturnsTrue)
    {
        IniProperty property1("TestKey", "TestValue", "TestComment");
        IniProperty property2("TESTKEY", "TESTVALUE", "TESTCOMMENT");

        EXPECT_EQ(property1, property2);
    }

    TEST(IniPropertyTest, Equality_DifferentValues_ReturnsFalse)
    {
        IniProperty property1("TestKey", "TestValue");
        IniProperty property2("TestKey", "OtherValue");

        EXPECT_NE(property1, property2);
    }

    TEST(IniPropertyTest, Equality_DifferentKeys_ReturnsFalse)
    {
        IniProperty property1("TestKey");
        IniProperty property2("OtherKey");

        EXPECT_NE(property1, property2);
    }

} // namespace ini::test