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
    // Create
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
    // CreateFromCollection
    // -------------------------------------------------------------------------

    TEST(IniPropertyTest, CreateFromCollection_EmptyKey_ThrowsInvalidArgument)
    {
        EXPECT_THROW(IniProperty::CreateFromCollection("", std::vector<std::string>{}), std::invalid_argument);
    }

    TEST(IniPropertyTest, CreateFromCollection_EmptyValues_CreatesPropertyWithEmptyValue)
    {
        IniProperty property = IniProperty::CreateFromCollection("TestKey", std::vector<std::string>{});

        EXPECT_EQ(property.GetKey(), "TestKey");
        EXPECT_EQ(property.GetValue(), "");
    }

    TEST(IniPropertyTest, CreateFromCollection_ValidValuesAndSpaceSeparator_CreatesProperty)
    {
        IniProperty property = IniProperty::CreateFromCollection("TestKey", std::vector<int>{8, 2, 3}, ' ');

        EXPECT_EQ(property.GetKey(), "TestKey");
        EXPECT_EQ(property.GetValue(), "8 2 3");
    }

    TEST(IniPropertyTest, CreateFromCollection_ValidValuesAndSemicolonSeparator_CreatesProperty)
    {
        IniProperty property = IniProperty::CreateFromCollection("TestKey", std::vector<int>{8, 2, 3}, ';');

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
    // TryGetConvertedValue
    // -------------------------------------------------------------------------

    TEST(IniPropertyTest, TryGetConvertedValue_EmptyValue_ReturnsFalseAndDefaultValue)
    {
        IniProperty property("TestKey", "");

        double convertedValue = 1.0;
        bool result = property.TryGetConvertedValue(convertedValue);

        EXPECT_FALSE(result);
        EXPECT_EQ(convertedValue, 0.0);
    }

    TEST(IniPropertyTest, TryGetConvertedValue_ValidIntValue_ReturnsTrueAndConvertedValue)
    {
        IniProperty property("TestKey", "42");

        int convertedValue = 0;
        bool result = property.TryGetConvertedValue(convertedValue);

        EXPECT_TRUE(result);
        EXPECT_EQ(convertedValue, 42);
    }

    TEST(IniPropertyTest, TryGetConvertedValue_ValidDoubleValue_ReturnsTrueAndConvertedValue)
    {
        IniProperty property("TestKey", "2.7100000e+00");

        double convertedValue = 0.0;
        bool result = property.TryGetConvertedValue(convertedValue);

        EXPECT_TRUE(result);
        EXPECT_DOUBLE_EQ(convertedValue, 2.71);
    }

    TEST(IniPropertyTest, TryGetConvertedValue_ValidStringValue_ReturnsTrueAndConvertedValue)
    {
        IniProperty property("TestKey", "TestValue");

        std::string convertedValue;
        bool result = property.TryGetConvertedValue(convertedValue);

        EXPECT_TRUE(result);
        EXPECT_EQ(convertedValue, "TestValue");
    }

    TEST(IniPropertyTest, TryGetConvertedValue_InvalidIntValue_ReturnsFalseAndDefaultValue)
    {
        IniProperty property("TestKey", "TestValue");

        int convertedValue = 99;
        bool result = property.TryGetConvertedValue(convertedValue);

        EXPECT_FALSE(result);
        EXPECT_EQ(convertedValue, 0);
    }

    TEST(IniPropertyTest, TryGetConvertedValue_InvalidDoubleValue_ReturnsFalseAndDefaultValue)
    {
        IniProperty property("TestKey", "TestValue");

        double convertedValue = 99.0;
        bool result = property.TryGetConvertedValue(convertedValue);

        EXPECT_FALSE(result);
        EXPECT_EQ(convertedValue, 0.0);
    }

    TEST(IniPropertyTest, TryGetConvertedValue_InvalidFloatValue_ReturnsFalseAndDefaultValue)
    {
        IniProperty property("TestKey", "TestValue");

        float convertedValue = 99.0f;
        bool result = property.TryGetConvertedValue(convertedValue);

        EXPECT_FALSE(result);
        EXPECT_EQ(convertedValue, 0.0f);
    }

    // -------------------------------------------------------------------------
    // TryGetConvertedValueCollection
    // -------------------------------------------------------------------------

    TEST(IniPropertyTest, TryGetConvertedValueCollection_EmptyValue_ReturnsFalseAndEmptyCollection)
    {
        IniProperty property("TestKey", "");

        std::vector<double> convertedValues;
        bool result = property.TryGetConvertedValueCollection(convertedValues);

        EXPECT_FALSE(result);
        EXPECT_TRUE(convertedValues.empty());
    }

    TEST(IniPropertyTest, TryGetConvertedValueCollection_ValidSingleIntValue_ReturnsTrueAndConvertedCollection)
    {
        IniProperty property("TestKey", "10");

        std::vector<int> convertedValues;
        bool result = property.TryGetConvertedValueCollection(convertedValues);

        EXPECT_TRUE(result);
        EXPECT_EQ(convertedValues, (std::vector<int>{10}));
    }

    TEST(IniPropertyTest,
         TryGetConvertedValueCollection_ValidMultiValueAndSpaceDelimiter_ReturnsTrueAndConvertedCollection)
    {
        IniProperty property("TestKey", "10 20 30");

        std::vector<int> convertedValues;
        bool result = property.TryGetConvertedValueCollection(convertedValues, ' ');

        EXPECT_TRUE(result);
        EXPECT_EQ(convertedValues, (std::vector<int>{10, 20, 30}));
    }

    TEST(IniPropertyTest,
         TryGetConvertedValueCollection_ValidMultiValueAndSemicolonDelimiter_ReturnsTrueAndConvertedCollection)
    {
        IniProperty property("TestKey", "10;20;30");

        std::vector<int> convertedValues;
        bool result = property.TryGetConvertedValueCollection(convertedValues, ';');

        EXPECT_TRUE(result);
        EXPECT_EQ(convertedValues, (std::vector<int>{10, 20, 30}));
    }

    TEST(IniPropertyTest,
         TryGetConvertedValueCollection_ValidMultiValueAndNewlineDelimiter_ReturnsTrueAndConvertedCollection)
    {
        IniProperty property("TestKey", "3 6 \n 9 \r\n 12");

        std::vector<int> convertedValues;
        bool result = property.TryGetConvertedValueCollection(convertedValues);

        EXPECT_TRUE(result);
        EXPECT_EQ(convertedValues, (std::vector<int>{3, 6, 9, 12}));
    }

    TEST(IniPropertyTest, TryGetConvertedValueCollection_InvalidFormattedValue_ReturnsFalseAndEmptyCollection)
    {
        IniProperty property("TestKey", "TestValue");

        std::vector<int> convertedValues;
        bool result = property.TryGetConvertedValueCollection(convertedValues);

        EXPECT_FALSE(result);
        EXPECT_TRUE(convertedValues.empty());
    }

    // -------------------------------------------------------------------------
    // SetConvertedValue
    // -------------------------------------------------------------------------

    TEST(IniPropertyTest, SetConvertedValue_EmptyStringValue_ValueIsSetToEmptyString)
    {
        IniProperty property("TestKey");

        property.SetConvertedValue(std::string(""));

        EXPECT_EQ(property.GetValue(), "");
    }

    TEST(IniPropertyTest, SetConvertedValue_ValidIntValue_UpdatesValue)
    {
        IniProperty property("TestKey");

        property.SetConvertedValue(11);

        EXPECT_EQ(property.GetValue(), "11");
    }

    TEST(IniPropertyTest, SetConvertedValue_ValidDoubleValue_UpdatesValue)
    {
        IniProperty property("TestKey");

        property.SetConvertedValue(0.123);

        EXPECT_EQ(property.GetValue(), "1.2300000e-01");
    }

    TEST(IniPropertyTest, SetConvertedValue_ValidStringValue_UpdatesValue)
    {
        IniProperty property("TestKey");

        property.SetConvertedValue(std::string("TestValue"));

        EXPECT_EQ(property.GetValue(), "TestValue");
    }

    // -------------------------------------------------------------------------
    // SetConvertedValueFromCollection
    // -------------------------------------------------------------------------

    TEST(IniPropertyTest, SetConvertedValueFromCollection_EmptyCollection_ValueIsSetToEmptyString)
    {
        IniProperty property("TestKey");

        property.SetConvertedValueFromCollection(std::vector<int>{});

        EXPECT_EQ(property.GetValue(), "");
    }

    TEST(IniPropertyTest, SetConvertedValueFromCollection_ValidValuesAndSpaceSeparator_UpdatesValue)
    {
        IniProperty property("TestKey");

        property.SetConvertedValueFromCollection(std::vector<int>{5, 1, 8}, ' ');

        EXPECT_EQ(property.GetValue(), "5 1 8");
    }

    TEST(IniPropertyTest, SetConvertedValueFromCollection_ValidValuesAndSemicolonSeparator_UpdatesValue)
    {
        IniProperty property("TestKey");

        property.SetConvertedValueFromCollection(std::vector<int>{5, 1, 8}, ';');

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