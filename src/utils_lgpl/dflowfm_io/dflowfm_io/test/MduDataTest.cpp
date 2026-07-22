#include <gtest/gtest.h>

#include <dflowfm_io/MduData.h>
#include <dflowfm_io/StringUtils.h>

#include "MduTestData.h"

namespace dflowfm_io::test
{

    // -------------------------------------------------------------------------
    // CreateFromSchema
    // -------------------------------------------------------------------------

    TEST(MduDataTest, CreateFromSchema_PopulatesDataEntries)
    {
        const MduData data = MduData::CreateFromSchema(TestSchema());

        EXPECT_FALSE(data.data_entries.empty());
    }

    TEST(MduDataTest, CreateFromSchema_ContainsAllPropertiesWithDefaults)
    {
        const MduData data = MduData::CreateFromSchema(TestSchema());

        for (const auto& sectionSchema : TestSchema().Sections())
            for (const auto& propertySchema : sectionSchema.properties)
                if (!propertySchema.default_value.empty())
                {
                    const std::string key = FormatKey(sectionSchema.name, propertySchema.key);
                    EXPECT_TRUE(data.hasValue(key));
                }
    }

    TEST(MduDataTest, CreateFromSchema_DoesNotContainPropertiesWithoutDefaults)
    {
        const MduData data = MduData::CreateFromSchema(TestSchema());

        for (const auto& sectionSchema : TestSchema().Sections())
            for (const auto& propertySchema : sectionSchema.properties)
                if (propertySchema.default_value.empty())
                {
                    const std::string key = FormatKey(sectionSchema.name, propertySchema.key);
                    EXPECT_FALSE(data.hasValue(key));
                }
    }

    TEST(MduDataTest, CreateFromSchema_IntPropertyHasCorrectDefaultValue)
    {
        const MduData data = MduData::CreateFromSchema(TestSchema());

        const std::string key = FormatKey("numerics", "maxNonLinearIterations");
        EXPECT_EQ(data.getValueAs<int>(key), 100);
    }

    TEST(MduDataTest, CreateFromSchema_FloatPropertyHasCorrectDefaultValue)
    {
        const MduData data = MduData::CreateFromSchema(TestSchema());

        const std::string key = FormatKey("geometry", "bedLevUni");
        EXPECT_DOUBLE_EQ(data.getValueAs<double>(key), -5.0);
    }

    TEST(MduDataTest, CreateFromSchema_StringPropertyHasCorrectDefaultValue)
    {
        const MduData data = MduData::CreateFromSchema(TestSchema());

        const std::string key = FormatKey("general", "fileVersion");
        EXPECT_EQ(data.getValueAs<std::string>(key), "1.09");
    }

    // -------------------------------------------------------------------------
    // hasValue
    // -------------------------------------------------------------------------

    TEST(MduDataTest, HasValue_ExistingKey_ReturnsTrue)
    {
        MduData data;
        data.data_entries["somekey"] = 1;

        EXPECT_TRUE(data.hasValue("somekey"));
    }

    TEST(MduDataTest, HasValue_AbsentKey_ReturnsFalse)
    {
        const MduData data;

        EXPECT_FALSE(data.hasValue("somekey"));
    }

    TEST(MduDataTest, HasValue_KeyStoredLowercase_LookupCaseInsensitive)
    {
        MduData data;
        data.data_entries["somekey"] = 1;

        EXPECT_TRUE(data.hasValue("SomeKey"));
        EXPECT_TRUE(data.hasValue("SOMEKEY"));
        EXPECT_TRUE(data.hasValue("somekey"));
    }

    TEST(MduDataTest, HasValue_KeyStoredUppercase_LookupCaseInsensitive)
    {
        MduData data;
        data.data_entries["somekey"] = 1;

        EXPECT_TRUE(data.hasValue("SOMEKEY"));
    }

    // -------------------------------------------------------------------------
    // getValueAs (const)
    // -------------------------------------------------------------------------

    TEST(MduDataTest, GetValueAs_ExistingIntKey_ReturnsCorrectValue)
    {
        MduData data;
        data.data_entries["somekey"] = 42;

        EXPECT_EQ(data.getValueAs<int>("somekey"), 42);
    }

    TEST(MduDataTest, GetValueAs_ExistingFloatKey_ReturnsCorrectValue)
    {
        MduData data;
        data.data_entries["somekey"] = 3.14;

        EXPECT_DOUBLE_EQ(data.getValueAs<double>("somekey"), 3.14);
    }

    TEST(MduDataTest, GetValueAs_ExistingStringKey_ReturnsCorrectValue)
    {
        MduData data;
        data.data_entries["somekey"] = std::string{"hello"};

        EXPECT_EQ(data.getValueAs<std::string>("somekey"), "hello");
    }

    TEST(MduDataTest, GetValueAs_ExistingBoolKey_ReturnsCorrectValue)
    {
        MduData data;
        data.data_entries["somekey"] = true;

        EXPECT_TRUE(data.getValueAs<bool>("somekey"));
    }

    TEST(MduDataTest, GetValueAs_ExistingPathKey_ReturnsCorrectValue)
    {
        MduData data;
        data.data_entries["somekey"] = std::filesystem::path{"some/file.txt"};

        EXPECT_EQ(data.getValueAs<std::filesystem::path>("somekey"), std::filesystem::path{"some/file.txt"});
    }

    TEST(MduDataTest, GetValueAs_ExistingEnumKey_ReturnsCorrectValue)
    {
        MduData data;
        data.data_entries["somekey"] = EnumValue{3};

        EXPECT_EQ(data.getValueAs<EnumValue>("somekey").value, 3);
    }

    TEST(MduDataTest, GetValueAs_ExistingDateTimeKey_ReturnsCorrectValue)
    {
        MduData data;
        const auto now = std::chrono::system_clock::now();
        data.data_entries["somekey"] = now;

        EXPECT_EQ(data.getValueAs<std::chrono::system_clock::time_point>("somekey"), now);
    }

    TEST(MduDataTest, GetValueAs_ExistingStringListKey_ReturnsCorrectValue)
    {
        MduData data;
        const std::vector<std::string> value{"a", "b", "c"};
        data.data_entries["somekey"] = value;

        EXPECT_EQ(data.getValueAs<std::vector<std::string>>("somekey"), value);
    }

    TEST(MduDataTest, GetValueAs_ExistingPathListKey_ReturnsCorrectValue)
    {
        MduData data;
        const std::vector<std::filesystem::path> value{"a.txt", "b.txt"};
        data.data_entries["somekey"] = value;

        EXPECT_EQ(data.getValueAs<std::vector<std::filesystem::path>>("somekey"), value);
    }

    TEST(MduDataTest, GetValueAs_ExistingFloatListKey_ReturnsCorrectValue)
    {
        MduData data;
        const std::vector<double> value{0.1, 0.2, 0.3};
        data.data_entries["somekey"] = value;

        const auto& result = data.getValueAs<std::vector<double>>("somekey");
        ASSERT_EQ(result.size(), 3u);
        EXPECT_DOUBLE_EQ(result[0], 0.1);
        EXPECT_DOUBLE_EQ(result[1], 0.2);
        EXPECT_DOUBLE_EQ(result[2], 0.3);
    }

    TEST(MduDataTest, GetValueAs_CaseInsensitiveLookup_ReturnsCorrectValue)
    {
        MduData data;
        data.data_entries["somekey"] = 7;

        EXPECT_EQ(data.getValueAs<int>("SomeKey"), 7);
        EXPECT_EQ(data.getValueAs<int>("SOMEKEY"), 7);
    }

    TEST(MduDataTest, GetValueAs_AbsentKey_ThrowsRuntimeError)
    {
        const MduData data;

        EXPECT_THROW(data.getValueAs<int>("missing"), std::runtime_error);
    }

    TEST(MduDataTest, GetValueAs_WrongType_ThrowsBadVariantAccess)
    {
        MduData data;
        data.data_entries["somekey"] = 1;

        EXPECT_THROW(data.getValueAs<std::string>("somekey"), std::bad_variant_access);
    }

    // -------------------------------------------------------------------------
    // getValueAs (mutable)
    // -------------------------------------------------------------------------

    TEST(MduDataTest, GetValueAsMutable_ExistingKey_CanModifyValue)
    {
        MduData data;
        data.data_entries["somekey"] = 1;

        data.getValueAs<int>("somekey") = 99;

        EXPECT_EQ(data.getValueAs<int>("somekey"), 99);
    }

    TEST(MduDataTest, GetValueAsMutable_AbsentKey_ThrowsRuntimeError)
    {
        MduData data;

        EXPECT_THROW(data.getValueAs<int>("missing"), std::runtime_error);
    }

    // -------------------------------------------------------------------------
    // setValue
    // -------------------------------------------------------------------------

    TEST(MduDataTest, SetValue_ExistingIntKey_UpdatesValue)
    {
        MduData data;
        data.data_entries["somekey"] = 1;

        data.setValue<int>("somekey", 42);

        EXPECT_EQ(data.getValueAs<int>("somekey"), 42);
    }

    TEST(MduDataTest, SetValue_ExistingFloatKey_UpdatesValue)
    {
        MduData data;
        data.data_entries["somekey"] = 1.0;

        data.setValue<double>("somekey", 2.5);

        EXPECT_DOUBLE_EQ(data.getValueAs<double>("somekey"), 2.5);
    }

    TEST(MduDataTest, SetValue_ExistingStringKey_UpdatesValue)
    {
        MduData data;
        data.data_entries["somekey"] = std::string{"old"};

        data.setValue<std::string>("somekey", "new");

        EXPECT_EQ(data.getValueAs<std::string>("somekey"), "new");
    }

    TEST(MduDataTest, SetValue_ExistingBoolKey_UpdatesValue)
    {
        MduData data;
        data.data_entries["somekey"] = false;

        data.setValue<bool>("somekey", true);

        EXPECT_TRUE(data.getValueAs<bool>("somekey"));
    }

    TEST(MduDataTest, SetValue_ExistingPathKey_UpdatesValue)
    {
        MduData data;
        data.data_entries["somekey"] = std::filesystem::path{"old.txt"};

        data.setValue<std::filesystem::path>("somekey", std::filesystem::path{"new.txt"});

        EXPECT_EQ(data.getValueAs<std::filesystem::path>("somekey"), std::filesystem::path{"new.txt"});
    }

    TEST(MduDataTest, SetValue_ExistingEnumKey_UpdatesValue)
    {
        MduData data;
        data.data_entries["somekey"] = EnumValue{1};

        data.setValue<EnumValue>("somekey", EnumValue{2});

        EXPECT_EQ(data.getValueAs<EnumValue>("somekey").value, 2);
    }

    TEST(MduDataTest, SetValue_ExistingDateTimeKey_UpdatesValue)
    {
        MduData data;
        data.data_entries["somekey"] = std::chrono::system_clock::time_point{};

        const auto newValue = std::chrono::system_clock::now();
        data.setValue<std::chrono::system_clock::time_point>("somekey", newValue);

        EXPECT_EQ(data.getValueAs<std::chrono::system_clock::time_point>("somekey"), newValue);
    }

    TEST(MduDataTest, SetValue_ExistingStringListKey_UpdatesValue)
    {
        MduData data;
        data.data_entries["somekey"] = std::vector<std::string>{"old"};

        const std::vector<std::string> newValue{"a", "b"};
        data.setValue<std::vector<std::string>>("somekey", newValue);

        EXPECT_EQ(data.getValueAs<std::vector<std::string>>("somekey"), newValue);
    }

    TEST(MduDataTest, SetValue_ExistingPathListKey_UpdatesValue)
    {
        MduData data;
        data.data_entries["somekey"] = std::vector<std::filesystem::path>{"old.txt"};

        const std::vector<std::filesystem::path> newValue{"a.txt", "b.txt"};
        data.setValue<std::vector<std::filesystem::path>>("somekey", newValue);

        EXPECT_EQ(data.getValueAs<std::vector<std::filesystem::path>>("somekey"), newValue);
    }

    TEST(MduDataTest, SetValue_ExistingFloatListKey_UpdatesValue)
    {
        MduData data;
        data.data_entries["somekey"] = std::vector<double>{0.0};

        const std::vector<double> newValue{0.5, 0.6};
        data.setValue<std::vector<double>>("somekey", newValue);

        const auto& result = data.getValueAs<std::vector<double>>("somekey");
        ASSERT_EQ(result.size(), 2u);
        EXPECT_DOUBLE_EQ(result[0], 0.5);
        EXPECT_DOUBLE_EQ(result[1], 0.6);
    }

    TEST(MduDataTest, SetValue_CaseInsensitiveKey_UpdatesValue)
    {
        MduData data;
        data.data_entries["somekey"] = 1;

        data.setValue<int>("SomeKey", 42);

        EXPECT_EQ(data.getValueAs<int>("somekey"), 42);
    }

    TEST(MduDataTest, SetValue_AbsentKey_ThrowsRuntimeError)
    {
        MduData data;

        EXPECT_THROW(data.setValue<int>("missing", 1), std::runtime_error);
    }

    // -------------------------------------------------------------------------
    // data_entries
    // -------------------------------------------------------------------------

    TEST(MduDataTest, DataEntries_DefaultConstructed_IsEmpty)
    {
        const MduData data;

        EXPECT_TRUE(data.data_entries.empty());
    }

    TEST(MduDataTest, DataEntries_AfterAddingEntry_HasCorrectSize)
    {
        MduData data;
        data.data_entries["somekey"] = 1;

        EXPECT_EQ(data.data_entries.size(), 1);
    }

} // namespace dflowfm_io::test