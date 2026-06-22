#include <gtest/gtest.h>
#include <stdexcept>
#include <string>
#include <vector>

#include "ini/IniProperty.h"
#include "ini/IniSection.h"

namespace ini::test
{

    // -------------------------------------------------------------------------
    // Constructors
    // -------------------------------------------------------------------------

    TEST(IniSectionTest, Constructor_EmptyName_ThrowsInvalidArgument)
    {
        EXPECT_THROW(IniSection(""), std::invalid_argument);
    }

    TEST(IniSectionTest, Constructor_ValidName_InitializesProperties)
    {
        IniSection section("TestSection");

        EXPECT_EQ(section.GetName(), "TestSection");
        EXPECT_TRUE(section.empty());
        EXPECT_TRUE(section.GetComments().empty());
        EXPECT_EQ(section.GetLineNumber(), 0);
        EXPECT_EQ(section.size(), 0);
        EXPECT_EQ(section.GetComments().size(), 0);
    }

    TEST(IniSectionTest, Constructor_EmptyNameAndValidSection_ThrowsInvalidArgument)
    {
        IniSection section("TestSection");

        EXPECT_THROW(IniSection("", std::move(section)), std::invalid_argument);
    }

    TEST(IniSectionTest, Constructor_ValidSection_InitializesProperties)
    {
        IniSection section("TestSection");
        section.SetLineNumber(2);
        IniProperty property("TestProperty", "TestValue");
        section.AddProperty(property);
        section.AddComment("TestComment");

        IniSection copiedSection("OtherSection", std::move(section));

        EXPECT_EQ(copiedSection.GetName(), "OtherSection");
        EXPECT_EQ(copiedSection.GetLineNumber(), 2);
        ASSERT_EQ(copiedSection.size(), 1);
        EXPECT_EQ(copiedSection[0], property);
        ASSERT_EQ(copiedSection.GetComments().size(), 1);
        EXPECT_EQ(copiedSection.GetComments()[0], "TestComment");
    }

    // -------------------------------------------------------------------------
    // Iterators
    // -------------------------------------------------------------------------

    TEST(IniSectionTest, Iterator_NoProperties_BeginEqualsEnd)
    {
        IniSection section("TestSection");

        EXPECT_EQ(section.begin(), section.end());
    }

    TEST(IniSectionTest, Iterator_WithProperties_IteratesInOrder)
    {
        IniSection section("TestSection");
        section.AddProperty("Key1", "Value1");
        section.AddProperty("Key2", "Value2");
        section.AddProperty("Key3", "Value3");

        auto it = section.begin();
        EXPECT_EQ((it++)->GetKey(), "Key1");
        EXPECT_EQ((it++)->GetKey(), "Key2");
        EXPECT_EQ((it++)->GetKey(), "Key3");
        EXPECT_EQ(it, section.end());
    }

    // -------------------------------------------------------------------------
    // Size / Empty
    // -------------------------------------------------------------------------

    TEST(IniSectionTest, Size_NoProperties_ReturnsZero)
    {
        IniSection section("TestSection");

        EXPECT_EQ(section.size(), 0);
    }

    TEST(IniSectionTest, Size_WithProperties_ReturnsPropertyCount)
    {
        IniSection section("TestSection");
        section.AddProperty("Key1", "Value1");
        section.AddProperty("Key2", "Value2");

        EXPECT_EQ(section.size(), 2);
    }

    TEST(IniSectionTest, Empty_NoProperties_ReturnsTrue)
    {
        IniSection section("TestSection");

        EXPECT_TRUE(section.empty());
    }

    TEST(IniSectionTest, Empty_WithProperties_ReturnsFalse)
    {
        IniSection section("TestSection");
        section.AddProperty("Key1", "Value1");

        EXPECT_FALSE(section.empty());
    }

    // -------------------------------------------------------------------------
    // AddProperty - key/value
    // -------------------------------------------------------------------------

    TEST(IniSectionTest, AddProperty_EmptyKey_ThrowsInvalidArgument)
    {
        IniSection section("TestSection");

        EXPECT_THROW(section.AddProperty("", "Value"), std::invalid_argument);
    }

    TEST(IniSectionTest, AddProperty_ValidStringValue_AddsPropertyWithValue)
    {
        IniSection section("TestSection");

        IniProperty& addedProperty = section.AddProperty("TestKey", "TestValue");

        EXPECT_EQ(addedProperty.GetKey(), "TestKey");
        EXPECT_EQ(addedProperty.GetValue(), "TestValue");
        ASSERT_EQ(section.size(), 1);
        EXPECT_EQ(section[0], addedProperty);
    }

    TEST(IniSectionTest, AddProperty_ValidIntValue_AddsPropertyWithValue)
    {
        IniSection section("TestSection");

        IniProperty& addedProperty = section.AddProperty("TestKey", 42);

        EXPECT_EQ(addedProperty.GetKey(), "TestKey");
        EXPECT_EQ(addedProperty.GetValue(), "42");
        ASSERT_EQ(section.size(), 1);
        EXPECT_EQ(section[0], addedProperty);
    }

    TEST(IniSectionTest, AddProperty_ValidFloatValue_AddsPropertyWithValue)
    {
        IniSection section("TestSection");

        IniProperty& addedProperty = section.AddProperty("TestKey", 1.22f);

        EXPECT_EQ(addedProperty.GetKey(), "TestKey");
        EXPECT_EQ(addedProperty.GetValue(), "1.22");
        ASSERT_EQ(section.size(), 1);
        EXPECT_EQ(section[0], addedProperty);
    }

    TEST(IniSectionTest, AddProperty_ValidDoubleValue_AddsPropertyWithValue)
    {
        IniSection section("TestSection");

        IniProperty& addedProperty = section.AddProperty("TestKey", 2.71);

        EXPECT_EQ(addedProperty.GetKey(), "TestKey");
        EXPECT_EQ(addedProperty.GetValue(), "2.71");
        ASSERT_EQ(section.size(), 1);
        EXPECT_EQ(section[0], addedProperty);
    }

    TEST(IniSectionTest, AddProperty_SamePropertyKey_AddsBothProperties)
    {
        IniSection section("TestSection");

        section.AddProperty("TestKey", "TestValue");
        section.AddProperty("TestKey", "TestValue");

        ASSERT_EQ(section.size(), 2);
        EXPECT_EQ(section[0].GetKey(), "TestKey");
        EXPECT_EQ(section[1].GetKey(), "TestKey");
    }

    // -------------------------------------------------------------------------
    // AddProperty - IniProperty
    // -------------------------------------------------------------------------

    TEST(IniSectionTest, AddProperty_ValidProperty_AddsProperty)
    {
        IniSection section("TestSection");
        IniProperty property("TestKey", "TestValue");

        IniProperty& addedProperty = section.AddProperty(property);

        ASSERT_EQ(section.size(), 1);
        ASSERT_EQ(section[0], property);
        EXPECT_EQ(addedProperty, property);
    }

    TEST(IniSectionTest, AddProperty_SamePropertyAdded_AddsBothProperties)
    {
        IniSection section("TestSection");
        IniProperty property1("TestKey", "TestValue");
        IniProperty property2("TestKey", "TestValue");

        section.AddProperty(property1);
        section.AddProperty(property2);

        ASSERT_EQ(section.size(), 2);
        EXPECT_EQ(section[0], property1);
        EXPECT_EQ(section[1], property2);
    }

    TEST(IniSectionTest, AddProperty_ValidProperties_AddsPropertyAndPreservesOrder)
    {
        IniSection section("TestSection");
        IniProperty property1("TestKey1", "TestValue1");
        IniProperty property2("TestKey2", "TestValue2");
        IniProperty property3("TestKey3", "TestValue3");

        section.AddProperty(property3);
        section.AddProperty(property2);
        section.AddProperty(property1);

        ASSERT_EQ(section.size(), 3);
        EXPECT_EQ(section[0], property3);
        EXPECT_EQ(section[1], property2);
        EXPECT_EQ(section[2], property1);
    }

    // -------------------------------------------------------------------------
    // AddPropertyIf
    // -------------------------------------------------------------------------

    TEST(IniSectionTest, AddPropertyIf_EmptyKey_ThrowsInvalidArgument)
    {
        IniSection section("TestSection");

        EXPECT_THROW(
            section.AddPropertyIf("", "TestValue",
                                  std::function<bool(const std::string&)>([](const std::string&) { return true; })),
            std::invalid_argument);
    }

    TEST(IniSectionTest, AddPropertyIf_ConditionIsFalse_DoesNotAddProperty)
    {
        IniSection section("TestSection");

        section.AddPropertyIf("TestKey", "TestValue",
                              std::function<bool(const std::string&)>([](const std::string&) { return false; }));

        EXPECT_TRUE(section.empty());
    }

    TEST(IniSectionTest, AddPropertyIf_ConditionIsTrue_AddsProperty)
    {
        IniSection section("TestSection");

        section.AddPropertyIf("TestKey", "TestValue",
                              std::function<bool(const std::string&)>([](const std::string&) { return true; }));

        ASSERT_EQ(section.size(), 1);
        EXPECT_EQ(section[0].GetKey(), "TestKey");
        EXPECT_EQ(section[0].GetValue(), "TestValue");
    }

    // -------------------------------------------------------------------------
    // AddProperty - key/values
    // -------------------------------------------------------------------------

    TEST(IniSectionTest, AddProperty_EmptyKeyAndValidValues_ThrowsInvalidArgument)
    {
        IniSection section("TestSection");

        EXPECT_THROW(section.AddProperty("", std::vector<int>{1, 2, 4}), std::invalid_argument);
    }

    TEST(IniSectionTest, AddProperty_EmptyValues_AddsPropertyWithEmptyValue)
    {
        IniSection section("TestSection");

        IniProperty& addedProperty = section.AddProperty("TestKey", std::vector<std::string>{});

        EXPECT_EQ(addedProperty.GetKey(), "TestKey");
        EXPECT_TRUE(addedProperty.GetValue().empty());
        ASSERT_EQ(section.size(), 1);
        EXPECT_EQ(section[0], addedProperty);
    }

    TEST(IniSectionTest, AddProperty_ValidValuesAndSpaceSeparator_AddsPropertyWithMultiValue)
    {
        IniSection section("TestSection");

        IniProperty& addedProperty = section.AddProperty("TestKey", std::vector<int>{8, 9, 2}, ' ');

        EXPECT_EQ(addedProperty.GetKey(), "TestKey");
        EXPECT_EQ(addedProperty.GetValue(), "8 9 2");
        ASSERT_EQ(section.size(), 1);
        EXPECT_EQ(section[0], addedProperty);
    }

    TEST(IniSectionTest, AddProperty_ValidValuesAndSemicolonSeparator_AddsPropertyWithMultiValue)
    {
        IniSection section("TestSection");

        IniProperty& addedProperty = section.AddProperty("TestKey", std::vector<int>{8, 9, 2}, ';');

        EXPECT_EQ(addedProperty.GetKey(), "TestKey");
        EXPECT_EQ(addedProperty.GetValue(), "8;9;2");
        ASSERT_EQ(section.size(), 1);
        EXPECT_EQ(section[0], addedProperty);
    }

    // -------------------------------------------------------------------------
    // AddProperties - key/values
    // -------------------------------------------------------------------------

    TEST(IniSectionTest, AddProperties_EmptyKey_ThrowsInvalidArgument)
    {
        IniSection section("TestSection");

        EXPECT_THROW(section.AddProperties("", std::vector<std::string>{"TestValue"}), std::invalid_argument);
    }

    TEST(IniSectionTest, AddProperties_EmptyValues_AddsNoProperties)
    {
        IniSection section("TestSection");

        section.AddProperties("TestKey", std::vector<std::string>{});

        EXPECT_TRUE(section.empty());
    }

    TEST(IniSectionTest, AddProperties_ValidIntValues_AddsProperties)
    {
        IniSection section("TestSection");

        section.AddProperties("TestKey", std::vector<int>{12, 34});

        ASSERT_EQ(section.size(), 2);
        EXPECT_EQ(section[0].GetKey(), "TestKey");
        EXPECT_EQ(section[0].GetValue(), "12");
        EXPECT_EQ(section[1].GetKey(), "TestKey");
        EXPECT_EQ(section[1].GetValue(), "34");
    }

    TEST(IniSectionTest, AddProperties_ValidFloatValues_AddsProperties)
    {
        IniSection section("TestSection");

        section.AddProperties("TestKey", std::vector<float>{0.1f, 0.2f});

        ASSERT_EQ(section.size(), 2);
        EXPECT_EQ(section[0].GetKey(), "TestKey");
        EXPECT_EQ(section[0].GetValue(), "0.1");
        EXPECT_EQ(section[1].GetKey(), "TestKey");
        EXPECT_EQ(section[1].GetValue(), "0.2");
    }

    TEST(IniSectionTest, AddProperties_ValidDoubleValues_AddsProperties)
    {
        IniSection section("TestSection");

        section.AddProperties("TestKey", std::vector<double>{10.1, 20.2});

        ASSERT_EQ(section.size(), 2);
        EXPECT_EQ(section[0].GetKey(), "TestKey");
        EXPECT_EQ(section[0].GetValue(), "10.1");
        EXPECT_EQ(section[1].GetKey(), "TestKey");
        EXPECT_EQ(section[1].GetValue(), "20.2");
    }

    TEST(IniSectionTest, AddProperties_ValidStringValues_AddsProperties)
    {
        IniSection section("TestSection");

        section.AddProperties("TestKey", std::vector<std::string>{"TestValue", "TestValue"});

        ASSERT_EQ(section.size(), 2);
        EXPECT_EQ(section[0].GetKey(), "TestKey");
        EXPECT_EQ(section[0].GetValue(), "TestValue");
        EXPECT_EQ(section[1].GetKey(), "TestKey");
        EXPECT_EQ(section[1].GetValue(), "TestValue");
    }

    // -------------------------------------------------------------------------
    // AddProperties - IniProperty collection
    // -------------------------------------------------------------------------

    TEST(IniSectionTest, AddProperties_EmptyCollection_AddsNoProperties)
    {
        IniSection section("TestSection");

        section.AddProperties({});

        EXPECT_TRUE(section.empty());
    }

    TEST(IniSectionTest, AddProperties_ValidProperties_AddsProperties)
    {
        IniSection section("TestSection");
        IniProperty property1("TestKey1", "TestValue1");
        IniProperty property2("TestKey2", "TestValue2");

        section.AddProperties({property1, property2});

        ASSERT_EQ(section.size(), 2);
        EXPECT_EQ(section[0], property1);
        EXPECT_EQ(section[1], property2);
    }

    // -------------------------------------------------------------------------
    // HasProperty
    // -------------------------------------------------------------------------

    TEST(IniSectionTest, HasProperty_EmptyKey_ThrowsInvalidArgument)
    {
        IniSection section("TestSection");

        EXPECT_THROW(section.HasProperty(""), std::invalid_argument);
    }

    class IniSectionHasPropertyCaseInsensitiveTest : public ::testing::TestWithParam<std::string>
    {
    };

    TEST_P(IniSectionHasPropertyCaseInsensitiveTest, HasProperty_ExistingKeyCaseInsensitive_ReturnsTrue)
    {
        IniSection section("ectionName");
        section.AddProperty("TestKey", "TestValue");

        EXPECT_TRUE(section.HasProperty(GetParam()));
    }

    INSTANTIATE_TEST_SUITE_P(IniSectionTest, IniSectionHasPropertyCaseInsensitiveTest,
                             ::testing::Values("testkey", "TestKey", "TESTKEY"));

    TEST(IniSectionTest, HasProperty_PropertyDoesNotExist_ReturnsFalse)
    {
        IniSection section("ectionName");
        section.AddProperty("TestKey", "TestValue");

        EXPECT_FALSE(section.HasProperty("OtherKey"));
    }

    // -------------------------------------------------------------------------
    // GetProperty
    // -------------------------------------------------------------------------

    TEST(IniSectionTest, GetProperty_EmptyKey_ThrowsInvalidArgument)
    {
        IniSection section("TestSection");

        EXPECT_THROW(section.GetProperty(""), std::invalid_argument);
    }

    class IniSectionGetPropertyCaseInsensitiveTest : public ::testing::TestWithParam<std::string>
    {
    };

    TEST_P(IniSectionGetPropertyCaseInsensitiveTest, GetProperty_ExistingKeyCaseInsensitive_ReturnsProperty)
    {
        IniSection section("TestSection");
        section.AddProperty("TestKey", "TestValue");

        const IniProperty& foundProperty = section.GetProperty(GetParam());

        EXPECT_EQ(foundProperty.GetKey(), "TestKey");
    }

    INSTANTIATE_TEST_SUITE_P(IniSectionTest, IniSectionGetPropertyCaseInsensitiveTest,
                             ::testing::Values("testkey", "TestKey", "TESTKEY"));

    TEST(IniSectionTest, GetProperty_NonExistingKey_ThrowsOutOfRange)
    {
        IniSection section("TestSection");
        section.AddProperty("TestKey", "TestValue");

        EXPECT_THROW(section.GetProperty("NonExistingKey"), std::out_of_range);
    }

    // -------------------------------------------------------------------------
    // GetPropertyValue - string
    // -------------------------------------------------------------------------

    TEST(IniSectionTest, GetPropertyValue_EmptyKey_ThrowsInvalidArgument)
    {
        IniSection section("TestSection");

        EXPECT_THROW(section.GetPropertyValue(""), std::invalid_argument);
    }

    class IniSectionGetPropertyValueCaseInsensitiveTest : public ::testing::TestWithParam<std::string>
    {
    };

    TEST_P(IniSectionGetPropertyValueCaseInsensitiveTest, GetPropertyValue_ExistingKeyCaseInsensitive_ReturnsValue)
    {
        IniSection section("TestSection");
        section.AddProperty("TestKey", "TestValue");

        const std::string value = section.GetPropertyValue(GetParam());

        EXPECT_EQ(value, "TestValue");
    }

    INSTANTIATE_TEST_SUITE_P(IniSectionTest, IniSectionGetPropertyValueCaseInsensitiveTest,
                             ::testing::Values("testkey", "TestKey", "TESTKEY"));

    TEST(IniSectionTest, GetPropertyValue_NonExistingKey_ReturnsDefaultValue)
    {
        IniSection section("TestSection");
        section.AddProperty("TestKey", "TestValue");

        const std::string value = section.GetPropertyValue("NonExistentKey", "DefaultValue");

        EXPECT_EQ(value, "DefaultValue");
    }

    // -------------------------------------------------------------------------
    // GetPropertyValue - generic
    // -------------------------------------------------------------------------

    TEST(IniSectionTest, GetPropertyValueGeneric_EmptyKey_ThrowsInvalidArgument)
    {
        IniSection section("TestSection");

        EXPECT_THROW(section.GetPropertyValue<std::string>(""), std::invalid_argument);
    }

    TEST(IniSectionTest, GetPropertyValueGeneric_NonExistingKey_ReturnsDefaultValue)
    {
        IniSection section("TestSection");
        section.AddProperty("TestKey", "TestValue");

        const std::string value = section.GetPropertyValue<std::string>("NonExistentKey", "DefaultValue");

        EXPECT_EQ(value, "DefaultValue");
    }

    class IniSectionGetPropertyValueGenericCaseInsensitiveTest : public ::testing::TestWithParam<std::string>
    {
    };

    TEST_P(IniSectionGetPropertyValueGenericCaseInsensitiveTest,
           GetPropertyValueGeneric_ExistingKeyCaseInsensitive_ReturnsValue)
    {
        IniSection section("TestSection");
        section.AddProperty("TestKey", "TestValue");

        const std::string value = section.GetPropertyValue<std::string>(GetParam());

        EXPECT_EQ(value, "TestValue");
    }

    INSTANTIATE_TEST_SUITE_P(IniSectionTest, IniSectionGetPropertyValueGenericCaseInsensitiveTest,
                             ::testing::Values("testkey", "TestKey", "TESTKEY"));

    TEST(IniSectionTest, GetPropertyValueGeneric_EmptyValue_ReturnsDefaultValue)
    {
        IniSection section("ectionName");
        section.AddProperty("TestKey", "");

        const std::string value = section.GetPropertyValue<std::string>("TestKey", "DefaultValue");

        EXPECT_EQ(value, "DefaultValue");
    }

    TEST(IniSectionTest, GetPropertyValueGeneric_ValidIntValue_ReturnsConvertedValue)
    {
        IniSection section("ectionName");
        section.AddProperty("TestKey", "42");

        const int value = section.GetPropertyValue<int>("TestKey");

        EXPECT_EQ(value, 42);
    }

    TEST(IniSectionTest, GetPropertyValueGeneric_ValidFloatValue_ReturnsConvertedValue)
    {
        IniSection section("ectionName");
        section.AddProperty("TestKey", "1.1300000e+00");

        const double value = section.GetPropertyValue<float>("TestKey");

        EXPECT_DOUBLE_EQ(value, 1.13f);
    }

    TEST(IniSectionTest, GetPropertyValueGeneric_ValidDoubleValue_ReturnsConvertedValue)
    {
        IniSection section("ectionName");
        section.AddProperty("TestKey", "2.7100000e+00");

        const double value = section.GetPropertyValue<double>("TestKey");

        EXPECT_DOUBLE_EQ(value, 2.71);
    }

    TEST(IniSectionTest, GetPropertyValueGeneric_ValidStringValue_ReturnsConvertedValue)
    {
        IniSection section("ectionName");
        section.AddProperty("TestKey", "TestValue");

        const std::string value = section.GetPropertyValue<std::string>("TestKey");

        EXPECT_EQ(value, "TestValue");
    }

    TEST(IniSectionTest, GetPropertyValueGeneric_InvalidIntValue_ReturnsDefault)
    {
        IniSection section("ectionName");
        section.AddProperty("TestKey", "TestValue");

        const int value = section.GetPropertyValue<int>("TestKey", -1);

        EXPECT_EQ(value, -1);
    }

    TEST(IniSectionTest, GetPropertyValueGeneric_InvalidFloatValue_ReturnsDefault)
    {
        IniSection section("ectionName");
        section.AddProperty("TestKey", "TestValue");

        const float value = section.GetPropertyValue<float>("TestKey", -999.0f);

        EXPECT_FLOAT_EQ(value, -999.0f);
    }

    TEST(IniSectionTest, GetPropertyValueGeneric_InvalidDoubleValue_ReturnsDefault)
    {
        IniSection section("ectionName");
        section.AddProperty("TestKey", "TestValue");

        const double value = section.GetPropertyValue<double>("TestKey", -999.0);

        EXPECT_DOUBLE_EQ(value, -999.0);
    }

    // -------------------------------------------------------------------------
    // GetAllPropertyValues - string
    // -------------------------------------------------------------------------

    TEST(IniSectionTest, GetAllPropertyValues_EmptyKey_ThrowsInvalidArgument)
    {
        IniSection section("TestSection");

        EXPECT_THROW(section.GetAllPropertyValues(""), std::invalid_argument);
    }

    TEST(IniSectionTest, GetAllPropertyValues_NonExistingKey_ReturnsEmptyCollection)
    {
        IniSection section("TestSection");
        section.AddProperty("TestKey", "TestValue");

        const std::vector<std::string> values = section.GetAllPropertyValues("NonExistentKey");

        EXPECT_TRUE(values.empty());
    }

    TEST(IniSectionTest, GetAllPropertyValues_EmptyValues_ReturnsEmptyCollection)
    {
        IniSection section("ectionName");
        section.AddProperty("TestKey", "");
        section.AddProperty("TestKey", "");

        const std::vector<std::string> values = section.GetAllPropertyValues("TestKey");

        EXPECT_TRUE(values.empty());
    }

    class IniSectionGetAllPropertyValuesCaseInsensitiveTest : public ::testing::TestWithParam<std::string>
    {
    };

    TEST_P(IniSectionGetAllPropertyValuesCaseInsensitiveTest,
           GetAllPropertyValues_ExistingKeyCaseInsensitive_ReturnsValues)
    {
        IniSection section("TestSection");
        section.AddProperty("TestKey", "TestValue1");
        section.AddProperty("TestKey", "TestValue2");

        const std::vector<std::string> values = section.GetAllPropertyValues(GetParam());

        const std::vector<std::string> expected = {"TestValue1", "TestValue2"};
        EXPECT_EQ(values, expected);
    }

    INSTANTIATE_TEST_SUITE_P(IniSectionTest, IniSectionGetAllPropertyValuesCaseInsensitiveTest,
                             ::testing::Values("testkey", "TestKey", "TESTKEY"));

    // -------------------------------------------------------------------------
    // GetAllPropertyValues - generic
    // -------------------------------------------------------------------------

    TEST(IniSectionTest, GetAllPropertyValuesGeneric_EmptyKey_ThrowsInvalidArgument)
    {
        IniSection section("TestSection");
        section.AddProperty("TestKey", "TestValue1");

        EXPECT_THROW(section.GetAllPropertyValues<int>(""), std::invalid_argument);
    }

    TEST(IniSectionTest, GetAllPropertyValuesGeneric_NonExistingKey_ReturnsEmptyCollection)
    {
        IniSection section("TestSection");
        section.AddProperty("TestKey", "TestValue");

        const std::vector<int> values = section.GetAllPropertyValues<int>("NonExistentKey");

        EXPECT_TRUE(values.empty());
    }

    TEST(IniSectionTest, GetAllPropertyValuesGeneric_EmptyValues_ReturnsEmptyCollection)
    {
        IniSection section("ectionName");
        section.AddProperty("TestKey", "");
        section.AddProperty("TestKey", "");

        const std::vector<int> values = section.GetAllPropertyValues<int>("TestKey");

        EXPECT_TRUE(values.empty());
    }

    TEST(IniSectionTest, GetAllPropertyValuesGeneric_ValidIntValues_ReturnsConvertedValues)
    {
        IniSection section("ectionName");
        section.AddProperty("TestKey", "1");
        section.AddProperty("TestKey", "9");

        const std::vector<int> values = section.GetAllPropertyValues<int>("TestKey");

        const std::vector<int> expected = {1, 9};
        EXPECT_EQ(values, expected);
    }

    TEST(IniSectionTest, GetAllPropertyValuesGeneric_ValidFloatValues_ReturnsConvertedValues)
    {
        IniSection section("ectionName");
        section.AddProperty("TestKey", "0.1000000e+00");
        section.AddProperty("TestKey", "0.2000000e+00");

        const std::vector<float> values = section.GetAllPropertyValues<float>("TestKey");

        ASSERT_EQ(values.size(), 2);
        EXPECT_FLOAT_EQ(values[0], 0.1f);
        EXPECT_FLOAT_EQ(values[1], 0.2f);
    }

    TEST(IniSectionTest, GetAllPropertyValuesGeneric_ValidDoubleValues_ReturnsConvertedValues)
    {
        IniSection section("ectionName");
        section.AddProperty("TestKey", "3.0200000e+00");
        section.AddProperty("TestKey", "5.1100000e+00");

        const std::vector<double> values = section.GetAllPropertyValues<double>("TestKey");

        ASSERT_EQ(values.size(), 2);
        EXPECT_DOUBLE_EQ(values[0], 3.02);
        EXPECT_DOUBLE_EQ(values[1], 5.11);
    }

    TEST(IniSectionTest, GetAllPropertyValuesGeneric_ValidStringValues_ReturnsConvertedValues)
    {
        IniSection section("ectionName");
        section.AddProperty("TestKey", "TestValue");
        section.AddProperty("TestKey", "TestValue");

        const std::vector<std::string> values = section.GetAllPropertyValues<std::string>("TestKey");

        const std::vector<std::string> expected = {"TestValue", "TestValue"};
        EXPECT_EQ(values, expected);
    }

    TEST(IniSectionTest, GetAllPropertyValuesGeneric_InvalidFormattedValue_ReturnsEmptyCollection)
    {
        IniSection section("ectionName");
        section.AddProperty("TestKey", "TestValue1");
        section.AddProperty("TestKey", "TestValue2");

        const std::vector<double> values = section.GetAllPropertyValues<double>("TestKey");

        EXPECT_TRUE(values.empty());
    }

    // -------------------------------------------------------------------------
    // GetPropertyValues
    // -------------------------------------------------------------------------

    TEST(IniSectionTest, GetPropertyValues_EmptyKey_ThrowsInvalidArgument)
    {
        IniSection section("TestSection");

        EXPECT_THROW(section.GetPropertyValues<std::string>(""), std::invalid_argument);
    }

    class IniSectionGetPropertyValuesCaseInsensitiveTest : public ::testing::TestWithParam<std::string>
    {
    };

    TEST_P(IniSectionGetPropertyValuesCaseInsensitiveTest,
           GetPropertyValues_ExistingKeyCaseInsensitive_ReturnsConvertedValues)
    {
        IniSection section("TestSection");
        section.AddProperty("TestKey", "1 2 3");

        const std::vector<int> values = section.GetPropertyValues<int>(GetParam());

        const std::vector<int> expected = {1, 2, 3};
        EXPECT_EQ(values, expected);
    }

    INSTANTIATE_TEST_SUITE_P(IniSectionTest, IniSectionGetPropertyValuesCaseInsensitiveTest,
                             ::testing::Values("testkey", "TestKey", "TESTKEY"));

    TEST(IniSectionTest, GetPropertyValues_NonExistingKey_ReturnsEmptyCollection)
    {
        IniSection section("TestSection");
        section.AddProperty("TestKey", "TestValue");

        const std::vector<std::string> values = section.GetPropertyValues<std::string>("NonExistentKey");

        EXPECT_TRUE(values.empty());
    }

    TEST(IniSectionTest, GetPropertyValues_EmptyValue_ReturnsEmptyCollection)
    {
        IniSection section("ectionName");
        section.AddProperty("TestKey", "");

        const std::vector<std::string> values = section.GetPropertyValues<std::string>("TestKey");

        EXPECT_TRUE(values.empty());
    }

    TEST(IniSectionTest, GetPropertyValues_ValidFormattedValueAndSpaceDelimiter_ReturnsConvertedValues)
    {
        IniSection section("ectionName");
        section.AddProperty("TestKey", "10 20 30");

        const std::vector<int> values = section.GetPropertyValues<int>("TestKey", ' ');

        const std::vector<int> expected = {10, 20, 30};
        EXPECT_EQ(values, expected);
    }

    TEST(IniSectionTest, GetPropertyValues_ValidFormattedValueAndSemicolonDelimiter_ReturnsConvertedValues)
    {
        IniSection section("ectionName");
        section.AddProperty("TestKey", "10;20;30");

        const std::vector<int> values = section.GetPropertyValues<int>("TestKey", ';');

        const std::vector<int> expected = {10, 20, 30};
        EXPECT_EQ(values, expected);
    }

    TEST(IniSectionTest, GetPropertyValues_ValidFormattedValueAndNewlineDelimiter_ReturnsConvertedValues)
    {
        IniSection section("ectionName");
        section.AddProperty("TestKey", "3 6 \n 9 \r\n 12");

        const std::vector<int> values = section.GetPropertyValues<int>("TestKey");

        const std::vector<int> expected = {3, 6, 9, 12};
        EXPECT_EQ(values, expected);
    }

    TEST(IniSectionTest, GetPropertyValues_InvalidFormattedValue_ReturnsEmptyCollection)
    {
        IniSection section("ectionName");
        section.AddProperty("TestKey", "TestValue");

        const std::vector<int> values = section.GetPropertyValues<int>("TestKey");

        EXPECT_TRUE(values.empty());
    }

    // -------------------------------------------------------------------------
    // SetPropertyValue - key/value
    // -------------------------------------------------------------------------

    TEST(IniSectionTest, SetPropertyValue_EmptyKeyAndValidValue_ThrowsInvalidArgument)
    {
        IniSection section("TestSection");

        EXPECT_THROW(section.SetPropertyValue("", "TestValue"), std::invalid_argument);
    }

    class IniSectionSetPropertyValueCaseInsensitiveTest : public ::testing::TestWithParam<std::string>
    {
    };

    TEST_P(IniSectionSetPropertyValueCaseInsensitiveTest,
           SetPropertyValue_ExistingKeyCaseInsensitive_UpdatesPropertyValue)
    {
        IniSection section("TestSection");
        section.AddProperty("TestKey", "TestValue");

        IniProperty& updatedProperty = section.SetPropertyValue(GetParam(), "UpdatedValue");

        EXPECT_EQ(updatedProperty.GetKey(), "TestKey");
        EXPECT_EQ(updatedProperty.GetValue(), "UpdatedValue");
        ASSERT_EQ(section.size(), 1);
        EXPECT_EQ(section[0], updatedProperty);
    }

    INSTANTIATE_TEST_SUITE_P(IniSectionTest, IniSectionSetPropertyValueCaseInsensitiveTest,
                             ::testing::Values("testkey", "TestKey", "TESTKEY"));

    TEST(IniSectionTest, SetPropertyValue_ExistingKeyAndEmptyValue_UpdatesPropertyToEmptyValue)
    {
        IniSection section("TestSection");
        section.AddProperty("TestKey", "TestValue");

        IniProperty& updatedProperty = section.SetPropertyValue("TestKey", "");

        EXPECT_EQ(updatedProperty.GetKey(), "TestKey");
        EXPECT_TRUE(updatedProperty.GetValue().empty());
        ASSERT_EQ(section.size(), 1);
        EXPECT_EQ(section[0], updatedProperty);
    }

    TEST(IniSectionTest, SetPropertyValue_NonExistingKeyAndValidValue_AddsPropertyWithValue)
    {
        IniSection section("TestSection");

        IniProperty& addedProperty = section.SetPropertyValue("TestKey", "TestValue");

        EXPECT_EQ(addedProperty.GetKey(), "TestKey");
        EXPECT_EQ(addedProperty.GetValue(), "TestValue");
        ASSERT_EQ(section.size(), 1);
        EXPECT_EQ(section[0], addedProperty);
    }

    TEST(IniSectionTest, SetPropertyValue_NonExistingKeyAndEmptyValue_AddsPropertyWithEmptyValue)
    {
        IniSection section("TestSection");
        section.AddProperty("TestKey", "TestValue");

        IniProperty& addedProperty = section.SetPropertyValue("TestKey", "");

        EXPECT_EQ(addedProperty.GetKey(), "TestKey");
        EXPECT_TRUE(addedProperty.GetValue().empty());
        ASSERT_EQ(section.size(), 1);
        EXPECT_EQ(section[0], addedProperty);
    }

    // -------------------------------------------------------------------------
    // SetPropertyValues - key/values
    // -------------------------------------------------------------------------

    TEST(IniSectionTest, SetPropertyValues_EmptyKeyAndValidValues_ThrowsInvalidArgument)
    {
        IniSection section("TestSection");

        EXPECT_THROW(section.SetPropertyValues("", std::vector<int>{1, 2, 2}), std::invalid_argument);
    }

    class IniSectionSetPropertyValuesCaseInsensitiveTest : public ::testing::TestWithParam<std::string>
    {
    };

    TEST_P(IniSectionSetPropertyValuesCaseInsensitiveTest,
           SetPropertyValues_ExistingKeyCaseInsensitive_UpdatesPropertyValue)
    {
        IniSection section("TestSection");
        section.AddProperty("TestKey", "TestValue");

        IniProperty& updatedProperty = section.SetPropertyValues(GetParam(), std::vector<int>{9, 2, 9});

        EXPECT_EQ(updatedProperty.GetKey(), "TestKey");
        EXPECT_EQ(updatedProperty.GetValue(), "9 2 9");
        ASSERT_EQ(section.size(), 1);
        EXPECT_EQ(section[0], updatedProperty);
    }

    INSTANTIATE_TEST_SUITE_P(IniSectionTest, IniSectionSetPropertyValuesCaseInsensitiveTest,
                             ::testing::Values("testkey", "TestKey", "TESTKEY"));

    TEST(IniSectionTest, SetPropertyValues_ExistingKeyAndValidValuesAndSpaceSeparator_UpdatesPropertyValue)
    {
        IniSection section("TestSection");
        section.AddProperty("TestKey", "TestValue");

        IniProperty& updatedProperty = section.SetPropertyValues("TestKey", std::vector<int>{1, 1, 1}, ' ');

        EXPECT_EQ(updatedProperty.GetKey(), "TestKey");
        EXPECT_EQ(updatedProperty.GetValue(), "1 1 1");
        ASSERT_EQ(section.size(), 1);
        EXPECT_EQ(section[0], updatedProperty);
    }

    TEST(IniSectionTest, SetPropertyValues_ExistingKeyAndValidValuesAndSemicolonSeparator_UpdatesPropertyValue)
    {
        IniSection section("TestSection");
        section.AddProperty("TestKey", "TestValue");

        IniProperty& updatedProperty = section.SetPropertyValues("TestKey", std::vector<int>{1, 1, 1}, ';');

        EXPECT_EQ(updatedProperty.GetKey(), "TestKey");
        EXPECT_EQ(updatedProperty.GetValue(), "1;1;1");
        ASSERT_EQ(section.size(), 1);
        EXPECT_EQ(section[0], updatedProperty);
    }

    TEST(IniSectionTest, SetPropertyValues_ExistingKeyAndEmptyValues_UpdatesPropertyToEmptyValue)
    {
        IniSection section("TestSection");
        section.AddProperty("TestKey", "TestValue");

        IniProperty& updatedProperty = section.SetPropertyValues("TestKey", std::vector<int>{});

        EXPECT_EQ(updatedProperty.GetKey(), "TestKey");
        EXPECT_TRUE(updatedProperty.GetValue().empty());
        ASSERT_EQ(section.size(), 1);
        EXPECT_EQ(section[0], updatedProperty);
    }

    TEST(IniSectionTest, SetPropertyValues_NonExistingKeyAndValidValuesAndSpaceSeparator_AddsPropertyWithValue)
    {
        IniSection section("TestSection");

        IniProperty& addedProperty = section.SetPropertyValues("TestKey", std::vector<int>{9, 2, 1}, ' ');

        EXPECT_EQ(addedProperty.GetKey(), "TestKey");
        EXPECT_EQ(addedProperty.GetValue(), "9 2 1");
        ASSERT_EQ(section.size(), 1);
        EXPECT_EQ(section[0], addedProperty);
    }

    TEST(IniSectionTest, SetPropertyValues_NonExistingKeyAndValidValuesAndSemicolonSeparator_AddsPropertyWithValue)
    {
        IniSection section("TestSection");

        IniProperty& addedProperty = section.SetPropertyValues("TestKey", std::vector<int>{9, 2, 1}, ';');

        EXPECT_EQ(addedProperty.GetKey(), "TestKey");
        EXPECT_EQ(addedProperty.GetValue(), "9;2;1");
        ASSERT_EQ(section.size(), 1);
        EXPECT_EQ(section[0], addedProperty);
    }

    TEST(IniSectionTest, SetPropertyValues_NonExistingKeyAndEmptyValues_AddsPropertyWithEmptyValue)
    {
        IniSection section("TestSection");
        section.AddProperty("TestKey", "TestValue");

        IniProperty& addedProperty = section.SetPropertyValues("TestKey", std::vector<double>{});

        EXPECT_EQ(addedProperty.GetKey(), "TestKey");
        EXPECT_TRUE(addedProperty.GetValue().empty());
        ASSERT_EQ(section.size(), 1);
        EXPECT_EQ(section[0], addedProperty);
    }

    // -------------------------------------------------------------------------
    // RemoveProperty
    // -------------------------------------------------------------------------

    TEST(IniSectionTest, RemoveProperty_ExistingProperty_RemovesProperty)
    {
        IniSection section("TestSection");
        IniProperty property("TestKey");
        section.AddProperty(property);

        section.RemoveProperty(property);

        EXPECT_TRUE(section.empty());
    }

    TEST(IniSectionTest, RemoveProperty_SamePropertyDifferentInstance_RemovesProperty)
    {
        IniSection section("TestSection");
        IniProperty property1("TestKey");
        IniProperty property2("TestKey");

        section.AddProperty(property1);
        section.RemoveProperty(property2);

        EXPECT_TRUE(section.empty());
    }

    TEST(IniSectionTest, RemoveProperty_DifferentProperty_DoesNotRemoveProperty)
    {
        IniSection section("TestSection");
        IniProperty property1("TestKey");
        IniProperty property2("OtherKey");

        section.AddProperty(property1);
        section.RemoveProperty(property2);

        ASSERT_EQ(section.size(), 1);
        EXPECT_EQ(section[0], property1);
    }

    TEST(IniSectionTest, RemoveProperty_ExistingProperty_PreservesOrder)
    {
        IniSection section("TestSection");
        section.AddProperty("Key1", "Value1");
        section.AddProperty("Key2", "Value2");
        section.AddProperty("Key1", "Value3");

        section.RemoveProperty(section[0]);

        section.AddProperty("Key3", "Value4");

        ASSERT_EQ(section.size(), 3);
        EXPECT_EQ(section[0].GetKey(), "Key2");
        EXPECT_EQ(section[1].GetKey(), "Key1");
        EXPECT_EQ(section[2].GetKey(), "Key3");
    }

    // -------------------------------------------------------------------------
    // RemoveAllProperties - by key
    // -------------------------------------------------------------------------

    TEST(IniSectionTest, RemoveAllProperties_EmptyKey_ThrowsInvalidArgument)
    {
        IniSection section("TestSection");

        EXPECT_THROW(section.RemoveAllProperties(""), std::invalid_argument);
    }

    class IniSectionRemoveAllPropertiesCaseInsensitiveTest : public ::testing::TestWithParam<std::string>
    {
    };

    TEST_P(IniSectionRemoveAllPropertiesCaseInsensitiveTest,
           RemoveAllProperties_ExistingKeyCaseInsensitive_RemovesMatchingProperties)
    {
        IniSection section("TestSection");
        section.AddProperty("Key1", "Value1");
        section.AddProperty("Key2", "Value2");
        section.AddProperty("Key1", "Value3");

        section.RemoveAllProperties(GetParam());

        EXPECT_FALSE(section.HasProperty("Key1"));
        EXPECT_EQ(section.size(), 1);
    }

    INSTANTIATE_TEST_SUITE_P(IniSectionTest, IniSectionRemoveAllPropertiesCaseInsensitiveTest,
                             ::testing::Values("key1", "Key1", "KEY1"));

    TEST(IniSectionTest, RemoveAllProperties_NonExistingKey_DoesNothing)
    {
        IniSection section("TestSection");
        section.AddProperty("TestKey", "Value1");

        section.RemoveAllProperties("NonExistingKey");

        EXPECT_EQ(section.size(), 1);
    }

    // -------------------------------------------------------------------------
    // RemoveAllProperties - by predicate
    // -------------------------------------------------------------------------

    TEST(IniSectionTest, RemoveAllProperties_PredicateMatches_RemovesMatchingProperties)
    {
        IniSection section("TestSection");
        section.AddProperty("Key1", "Value1");
        section.AddProperty("Key2", "Value2");
        section.AddProperty("Key1", "Value1");

        section.RemoveAllProperties([](const IniProperty& p) { return p.GetValue() == "Value1"; });

        EXPECT_FALSE(section.HasProperty("Key1"));
        EXPECT_EQ(section.size(), 1);
    }

    TEST(IniSectionTest, RemoveAllProperties_PredicateDoesNotMatch_DoesNothing)
    {
        IniSection section("TestSection");
        section.AddProperty("TestKey", "Value1");

        section.RemoveAllProperties([](const IniProperty&) { return false; });

        EXPECT_EQ(section.size(), 1);
    }

    // -------------------------------------------------------------------------
    // ClearProperties
    // -------------------------------------------------------------------------

    TEST(IniSectionTest, ClearProperties_WithProperties_RemovesAllProperties)
    {
        IniSection section("TestSection");
        section.AddProperty("Key1", "Value1");
        section.AddProperty("Key2", "Value2");

        section.ClearProperties();

        EXPECT_TRUE(section.empty());
    }

    TEST(IniSectionTest, ClearProperties_WithoutProperties_DoesNothing)
    {
        IniSection section("TestSection");

        section.ClearProperties();

        EXPECT_TRUE(section.empty());
    }

    // -------------------------------------------------------------------------
    // RenameProperties
    // -------------------------------------------------------------------------

    class IniSectionRenamePropertiesInvalidKeyTest
        : public ::testing::TestWithParam<std::pair<std::string, std::string>>
    {
    };

    TEST_P(IniSectionRenamePropertiesInvalidKeyTest, RenameProperties_KeyIsEmpty_ThrowsInvalidArgument)
    {
        IniSection section("TestSection");
        auto [oldKey, newKey] = GetParam();

        EXPECT_THROW(section.RenameProperties(oldKey, newKey), std::invalid_argument);
    }

    INSTANTIATE_TEST_SUITE_P(IniSectionTest, IniSectionRenamePropertiesInvalidKeyTest,
                             ::testing::Values(std::make_pair("", "TestProperty"), std::make_pair("TestProperty", "")));

    class IniSectionRenamePropertiesCaseInsensitiveTest : public ::testing::TestWithParam<std::string>
    {
    };

    TEST_P(IniSectionRenamePropertiesCaseInsensitiveTest, RenameProperties_ExistingKeyCaseInsensitive_KeyRenamed)
    {
        IniSection section("TestSection");
        section.AddProperty("Key1", "Value1");
        section.AddProperty("Key2", "Value2");
        section.AddProperty("Key1", "Value3");

        section.RenameProperties(GetParam(), "NewKey");

        ASSERT_EQ(section.size(), 3);
        EXPECT_EQ(section[0].GetKey(), "NewKey");
        EXPECT_EQ(section[1].GetKey(), "Key2");
        EXPECT_EQ(section[2].GetKey(), "NewKey");
    }

    INSTANTIATE_TEST_SUITE_P(IniSectionTest, IniSectionRenamePropertiesCaseInsensitiveTest,
                             ::testing::Values("key1", "Key1", "KEY1"));

    TEST(IniSectionTest, RenameProperties_NonExistingKey_NoChanges)
    {
        IniSection section("ectionName");
        section.AddProperty("Key1", "Value1");
        section.AddProperty("Key2", "Value2");

        section.RenameProperties("NonExistentKey", "NewKey");

        EXPECT_EQ(section[0].GetKey(), "Key1");
        EXPECT_EQ(section[1].GetKey(), "Key2");
    }

    // -------------------------------------------------------------------------
    // AddComment
    // -------------------------------------------------------------------------

    TEST(IniSectionTest, AddComment_EmptyComment_AddsComment)
    {
        IniSection section("TestSection");

        section.AddComment("");

        ASSERT_EQ(section.GetComments().size(), 1);
        EXPECT_EQ(section.GetComments()[0], "");
    }

    TEST(IniSectionTest, AddComment_WhitespaceComment_AddsComment)
    {
        IniSection section("TestSection");

        section.AddComment("  ");

        ASSERT_EQ(section.GetComments().size(), 1);
        EXPECT_EQ(section.GetComments()[0], "  ");
    }

    TEST(IniSectionTest, AddComment_ValidComment_AddsComment)
    {
        IniSection section("TestSection");

        section.AddComment("TestComment");

        ASSERT_EQ(section.GetComments().size(), 1);
        EXPECT_EQ(section.GetComments()[0], "TestComment");
    }

    // -------------------------------------------------------------------------
    // AddComments
    // -------------------------------------------------------------------------

    TEST(IniSectionTest, AddComments_ValidComments_AddsComments)
    {
        IniSection section("TestSection");

        section.AddComments({"TestComment1", "TestComment2"});

        const std::vector<std::string> expected = {"TestComment1", "TestComment2"};
        EXPECT_EQ(section.GetComments(), expected);
    }

    // -------------------------------------------------------------------------
    // RemoveComment
    // -------------------------------------------------------------------------

    TEST(IniSectionTest, RemoveComment_EmptyComment_ThrowsInvalidArgument)
    {
        IniSection section("TestSection");

        EXPECT_THROW(section.RemoveComment(""), std::invalid_argument);
    }

    TEST(IniSectionTest, RemoveComment_ExistingComment_RemovesComment)
    {
        IniSection section("TestSection");
        section.AddComment("TestComment");

        section.RemoveComment("TestComment");

        EXPECT_TRUE(section.GetComments().empty());
    }

    TEST(IniSectionTest, RemoveComment_DifferentComment_DoesNotRemoveComment)
    {
        IniSection section("TestSection");
        section.AddComment("TestComment");

        section.RemoveComment("OtherComment");

        ASSERT_EQ(section.GetComments().size(), 1);
        EXPECT_EQ(section.GetComments()[0], "TestComment");
    }

    // -------------------------------------------------------------------------
    // ClearComments
    // -------------------------------------------------------------------------

    TEST(IniSectionTest, ClearComments_WithComments_RemovesAllComments)
    {
        IniSection section("TestSection");
        section.AddComment("Comment1");
        section.AddComment("Comment2");

        section.ClearComments();

        EXPECT_TRUE(section.GetComments().empty());
    }

    TEST(IniSectionTest, ClearComments_WithoutComments_DoesNothing)
    {
        IniSection section("TestSection");

        section.ClearComments();

        EXPECT_TRUE(section.GetComments().empty());
    }

    // -------------------------------------------------------------------------
    // IsNameEqualTo
    // -------------------------------------------------------------------------

    TEST(IniSectionTest, IsNameEqualTo_EmptyName_ThrowsInvalidArgument)
    {
        IniSection section("TestSection");

        EXPECT_THROW(section.IsNameEqualTo(""), std::invalid_argument);
    }

    class IniSectionIsNameEqualToCaseInsensitiveTest : public ::testing::TestWithParam<std::string>
    {
    };

    TEST_P(IniSectionIsNameEqualToCaseInsensitiveTest, IsNameEqualTo_SameCaseInsensitiveName_ReturnsTrue)
    {
        IniSection section("TestSection");

        EXPECT_TRUE(section.IsNameEqualTo(GetParam()));
    }

    INSTANTIATE_TEST_SUITE_P(IniSectionTest, IniSectionIsNameEqualToCaseInsensitiveTest,
                             ::testing::Values("testsection", "TestSection", "TESTSECTION"));

    TEST(IniSectionTest, IsNameEqualTo_DifferentName_ReturnsFalse)
    {
        IniSection section("TestSection");

        EXPECT_FALSE(section.IsNameEqualTo("OtherSection"));
    }

    // -------------------------------------------------------------------------
    // Equality
    // -------------------------------------------------------------------------

    TEST(IniSectionTest, Equals_SameSectionReference_ReturnsTrue)
    {
        IniSection section("TestSection");

        EXPECT_TRUE(section == section);
    }

    TEST(IniSectionTest, Equals_SameSectionsCaseInsensitive_ReturnsTrue)
    {
        IniSection section1("TestSection");
        section1.SetLineNumber(3);
        section1.AddProperty("TestKey", "TestValue");
        section1.AddComment("TestComment");

        IniSection section2("TESTSECTION");
        section2.SetLineNumber(3);
        section2.AddProperty("TestKey", "TestValue");
        section2.AddComment("TestComment");

        EXPECT_TRUE(section1 == section2);
    }

    TEST(IniSectionTest, Equals_DifferentSections_ReturnsFalse)
    {
        IniSection section1("TestSection");
        IniSection section2("OtherSection");

        EXPECT_FALSE(section1 == section2);
    }

    TEST(IniSectionTest, Equals_SameSectionsDifferentProperties_ReturnsFalse)
    {
        IniSection section1("TestSection");
        IniSection section2("TestSection");

        section1.AddProperty("TestKey", "TestValue");
        section2.AddProperty("TestKey", "OtherValue");

        EXPECT_FALSE(section1 == section2);
    }

    TEST(IniSectionTest, Equals_SameSectionsDifferentComments_ReturnsFalse)
    {
        IniSection section1("TestSection");
        IniSection section2("TestSection");

        section1.AddComment("TestComment");
        section1.AddComment("OtherComment");

        EXPECT_FALSE(section1 == section2);
    }

    TEST(IniSectionTest, NotEquals_DifferentSections_ReturnsTrue)
    {
        IniSection section1("TestSection");
        IniSection section2("OtherSection");

        EXPECT_TRUE(section1 != section2);
    }

    // -------------------------------------------------------------------------
    // SetName / ToString equivalent
    // -------------------------------------------------------------------------

    TEST(IniSectionTest, GetName_ReturnsName)
    {
        IniSection section("TestSection");

        EXPECT_EQ(section.GetName(), "TestSection");
    }

} // namespace ini::test