#include <gtest/gtest.h>
#include <stdexcept>
#include <string>
#include <vector>

#include "ini/IniProperty.h"

namespace ini::test
{

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
        EXPECT_THROW(IniProperty::Create("", std::string("TestValue")), std::invalid_argument);
    }

    TEST(IniPropertyTest, Create_EmptyStringValue_CreatesPropertyWithEmptyValue)
    {
        IniProperty property = IniProperty::Create("TestKey", std::string(""));

        EXPECT_EQ(property.GetKey(), "TestKey");
        EXPECT_EQ(property.GetValue(), "");
    }

    TEST(IniPropertyTest, Create_ValidIntValue_CreatesProperty)
    {
        IniProperty property = IniProperty::Create("TestKey", 42);

        EXPECT_EQ(property.GetKey(), "TestKey");
        EXPECT_EQ(property.GetValue(), "42");
    }

    TEST(IniPropertyTest, Create_ValidFloatValue_CreatesProperty)
    {
        IniProperty property = IniProperty::Create("TestKey", 1.01f);

        EXPECT_EQ(property.GetKey(), "TestKey");
        EXPECT_EQ(property.GetValue(), "1.0100000e+00");
    }

    TEST(IniPropertyTest, Create_ValidDoubleValue_CreatesProperty)
    {
        IniProperty property = IniProperty::Create("TestKey", 2.71);

        EXPECT_EQ(property.GetKey(), "TestKey");
        EXPECT_EQ(property.GetValue(), "2.7100000e+00");
    }

    TEST(IniPropertyTest, Create_ValidStringValue_CreatesProperty)
    {
        IniProperty property = IniProperty::Create("TestKey", std::string("TestValue"));

        EXPECT_EQ(property.GetKey(), "TestKey");
        EXPECT_EQ(property.GetValue(), "TestValue");
    }

    // -------------------------------------------------------------------------
    // Create - key/values
    // -------------------------------------------------------------------------

    TEST(IniPropertyTest, Create_EmptyKeyAndEmptyValues_ThrowsInvalidArgument)
    {
        EXPECT_THROW(IniProperty::Create("", std::vector<std::string>{}), std::invalid_argument);
    }

    TEST(IniPropertyTest, Create_EmptyValues_ThrowsInvalidArgument)
    {
        IniProperty property = IniProperty::Create("TestKey", std::vector<std::string>{});

        EXPECT_EQ(property.GetKey(), "TestKey");
        EXPECT_EQ(property.GetValue(), "");
    }

    TEST(IniPropertyTest, Create_ValidValuesAndSpaceSeparator_CreatesProperty)
    {
        IniProperty property = IniProperty::Create("TestKey", std::vector<int>{8, 2, 3}, ' ');

        EXPECT_EQ(property.GetKey(), "TestKey");
        EXPECT_EQ(property.GetValue(), "8 2 3");
    }

    TEST(IniPropertyTest, Create_ValidValuesAndSemicolonSeparator_CreatesProperty)
    {
        IniProperty property = IniProperty::Create("TestKey", std::vector<int>{8, 2, 3}, ';');

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
    // TryGetValue
    // -------------------------------------------------------------------------

    TEST(IniPropertyTest, TryGetValue_EmptyValue_ReturnsNullopt)
    {
        IniProperty property("TestKey", "");

        auto result = property.TryGetValue<double>();

        EXPECT_FALSE(result.has_value());
    }

    TEST(IniPropertyTest, TryGetValue_ValidIntValue_ReturnsConvertedValue)
    {
        IniProperty property("TestKey", "42");

        auto result = property.TryGetValue<int>();

        EXPECT_TRUE(result.has_value());
        EXPECT_EQ(*result, 42);
    }

    TEST(IniPropertyTest, TryGetValue_ValidFloatValue_ReturnsConvertedValue)
    {
        IniProperty property("TestKey", "1.0100000e+00");

        auto result = property.TryGetValue<float>();

        EXPECT_TRUE(result.has_value());
        EXPECT_FLOAT_EQ(*result, 1.01f);
    }

    TEST(IniPropertyTest, TryGetValue_ValidDoubleValue_ReturnsConvertedValue)
    {
        IniProperty property("TestKey", "2.7100000e+00");

        auto result = property.TryGetValue<double>();

        EXPECT_TRUE(result.has_value());
        EXPECT_DOUBLE_EQ(*result, 2.71);
    }

    TEST(IniPropertyTest, TryGetValue_ValidStringValue_ReturnsConvertedValue)
    {
        IniProperty property("TestKey", "TestValue");

        auto result = property.TryGetValue<std::string>();

        EXPECT_TRUE(result.has_value());
        EXPECT_EQ(*result, "TestValue");
    }

    TEST(IniPropertyTest, TryGetValue_InvalidIntValue_ReturnsNullopt)
    {
        IniProperty property("TestKey", "TestValue");

        auto result = property.TryGetValue<int>();

        EXPECT_FALSE(result.has_value());
    }

    TEST(IniPropertyTest, TryGetValue_InvalidFloatValue_ReturnsNullopt)
    {
        IniProperty property("TestKey", "TestValue");

        auto result = property.TryGetValue<float>();

        EXPECT_FALSE(result.has_value());
    }

    TEST(IniPropertyTest, TryGetValue_InvalidDoubleValue_ReturnsNullopt)
    {
        IniProperty property("TestKey", "TestValue");

        auto result = property.TryGetValue<double>();

        EXPECT_FALSE(result.has_value());
    }

    // -------------------------------------------------------------------------
    // TryGetValues
    // -------------------------------------------------------------------------

    TEST(IniPropertyTest, TryGetValues_EmptyValue_ReturnsNullopt)
    {
        IniProperty property("TestKey", "");

        auto result = property.TryGetValues<double>();

        EXPECT_FALSE(result.has_value());
    }

    TEST(IniPropertyTest, TryGetValues_ValidSingleIntValue_ReturnsConvertedCollection)
    {
        IniProperty property("TestKey", "10");

        auto result = property.TryGetValues<int>();

        EXPECT_TRUE(result.has_value());
        EXPECT_EQ(*result, (std::vector<int>{10}));
    }

    TEST(IniPropertyTest, TryGetValues_ValidMultiValueAndSpaceDelimiter_ReturnsConvertedCollection)
    {
        IniProperty property("TestKey", "10 20 30");

        auto result = property.TryGetValues<int>(' ');

        EXPECT_TRUE(result.has_value());
        EXPECT_EQ(*result, (std::vector<int>{10, 20, 30}));
    }

    TEST(IniPropertyTest, TryGetValues_ValidMultiValueAndSemicolonDelimiter_ReturnsConvertedCollection)
    {
        IniProperty property("TestKey", "10;20;30");

        auto result = property.TryGetValues<int>(';');

        EXPECT_TRUE(result.has_value());
        EXPECT_EQ(*result, (std::vector<int>{10, 20, 30}));
    }

    TEST(IniPropertyTest, TryGetValues_ValidMultiValueAndNewlineDelimiter_ReturnsConvertedCollection)
    {
        IniProperty property("TestKey", "3 6 \n 9 \r\n 12");

        auto result = property.TryGetValues<int>();

        EXPECT_TRUE(result.has_value());
        EXPECT_EQ(*result, (std::vector<int>{3, 6, 9, 12}));
    }

    TEST(IniPropertyTest, TryGetValues_InvalidFormattedValue_ReturnsNullopt)
    {
        IniProperty property("TestKey", "TestValue");

        auto result = property.TryGetValues<int>();

        EXPECT_FALSE(result.has_value());
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

        property.SetValue(11);

        EXPECT_EQ(property.GetValue(), "11");
    }

    TEST(IniPropertyTest, SetValue_ValidFloatValue_UpdatesValue)
    {
        IniProperty property("TestKey");

        property.SetValue(12.33f);

        EXPECT_EQ(property.GetValue(), "1.2330000e+01");
    }

    TEST(IniPropertyTest, SetValue_ValidDoubleValue_UpdatesValue)
    {
        IniProperty property("TestKey");

        property.SetValue(0.123);

        EXPECT_EQ(property.GetValue(), "1.2300000e-01");
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

        property.SetValues(std::vector<int>{});

        EXPECT_EQ(property.GetValue(), "");
    }

    TEST(IniPropertyTest, SetValues_ValidValuesAndSpaceSeparator_UpdatesValue)
    {
        IniProperty property("TestKey");

        property.SetValues(std::vector<int>{5, 1, 8}, ' ');

        EXPECT_EQ(property.GetValue(), "5 1 8");
    }

    TEST(IniPropertyTest, SetValues_ValidValuesAndSemicolonSeparator_UpdatesValue)
    {
        IniProperty property("TestKey");

        property.SetValues(std::vector<int>{5, 1, 8}, ';');

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