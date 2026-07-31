#include <gtest/gtest.h>

#include <map>

#include <dflowfm_io/MduData.h>
#include <dflowfm_io/StringUtils.h>

#include "MduTestData.h"

namespace dflowfm_io::test
{

    // -------------------------------------------------------------------------
    // Helpers
    // -------------------------------------------------------------------------

    namespace
    {
        MduData MakeMduData(std::unordered_map<std::string, Value> entries)
        {
            return MduData(std::move(entries));
        }
    } // namespace

    // -------------------------------------------------------------------------
    // size
    // -------------------------------------------------------------------------

    TEST(MduDataTest, Size_NoData_ReturnsZero)
    {
        MduData data = MakeMduData({});

        EXPECT_EQ(data.size(), 0);
    }
    
    TEST(MduDataTest, Size_AfterAddingEntries_ReturnsCorrectSize)
    {
        MduData data = MakeMduData({
            {"keya", 1},
            {"keyb", 2},
            {"keyc", 3}
        });

        EXPECT_EQ(data.size(), 3);
    }

    // -------------------------------------------------------------------------
    // empty
    // -------------------------------------------------------------------------

    TEST(MduDataTest, Empty_NonEmptyData_ReturnsFalse)
    {
        MduData data = MakeMduData({
            {"somekey", 1}
        });

        EXPECT_FALSE(data.empty());
    }

    TEST(MduDataTest, Empty_NoData_ReturnsTrue)
    {
        MduData data = MakeMduData({});

        EXPECT_TRUE(data.empty());
    }

    // -------------------------------------------------------------------------
    // hasValue
    // -------------------------------------------------------------------------

    TEST(MduDataTest, HasValue_ExistingKey_ReturnsTrue)
    {
        MduData data = MakeMduData({
            {"somekey", 1}
        });

        EXPECT_TRUE(data.hasValue("somekey"));
    }

    TEST(MduDataTest, HasValue_AbsentKey_ReturnsFalse)
    {
        MduData data = MakeMduData({
            {"somekey", 1}
        });

        EXPECT_FALSE(data.hasValue("otherkey"));
    }

    TEST(MduDataTest, HasValue_KeyStoredLowercase_LookupCaseInsensitive)
    {
        MduData data = MakeMduData({
            {"somekey", 1}
        });

        EXPECT_TRUE(data.hasValue("SomeKey"));
        EXPECT_TRUE(data.hasValue("SOMEKEY"));
        EXPECT_TRUE(data.hasValue("somekey"));
    }

    TEST(MduDataTest, HasValue_KeyStoredUppercase_LookupCaseInsensitive)
    {
        MduData data = MakeMduData({
            {"somekey", 1}
        });

        EXPECT_TRUE(data.hasValue("SOMEKEY"));
    }

    // -------------------------------------------------------------------------
    // getValue
    // -------------------------------------------------------------------------

    TEST(MduDataTest, GetValue_ExistingKey_ReturnsStoredValue)
    {
        MduData data = MakeMduData({
            {"somekey", 42}
        });

        EXPECT_EQ(std::get<int>(data.getValue("somekey")), 42);
    }

    TEST(MduDataTest, GetValue_CaseInsensitiveLookup_ReturnsStoredValue)
    {
        MduData data = MakeMduData({
            {"somekey", 42}
        });

        EXPECT_EQ(std::get<int>(data.getValue("SomeKey")), 42);
    }

    TEST(MduDataTest, GetValue_AbsentKey_ThrowsRuntimeError)
    {
        MduData data = MakeMduData({
            {"somekey", 1}
        });

        EXPECT_THROW(data.getValue("missing"), std::runtime_error);
    }

    // -------------------------------------------------------------------------
    // getValueAs (const)
    // -------------------------------------------------------------------------

    TEST(MduDataTest, GetValueAs_ExistingIntKey_ReturnsCorrectValue)
    {
        MduData data = MakeMduData({
            {"somekey", 42}
        });

        EXPECT_EQ(data.getValueAs<int>("somekey"), 42);
    }

    TEST(MduDataTest, GetValueAs_ExistingFloatKey_ReturnsCorrectValue)
    {
        MduData data = MakeMduData({
            {"somekey", 3.14}
        });

        EXPECT_DOUBLE_EQ(data.getValueAs<double>("somekey"), 3.14);
    }

    TEST(MduDataTest, GetValueAs_ExistingStringKey_ReturnsCorrectValue)
    {
        MduData data = MakeMduData({
            {"somekey", std::string{"hello"}}
        });

        EXPECT_EQ(data.getValueAs<std::string>("somekey"), "hello");
    }

    TEST(MduDataTest, GetValueAs_ExistingBoolKey_ReturnsCorrectValue)
    {
        MduData data = MakeMduData({
            {"somekey", true}
        });

        EXPECT_TRUE(data.getValueAs<bool>("somekey"));
    }

    TEST(MduDataTest, GetValueAs_ExistingPathKey_ReturnsCorrectValue)
    {
        MduData data = MakeMduData({
            {"somekey", std::filesystem::path{"some/file.txt"}}
        });

        EXPECT_EQ(data.getValueAs<std::filesystem::path>("somekey"), std::filesystem::path{"some/file.txt"});
    }

    TEST(MduDataTest, GetValueAs_ExistingStringEnumKey_ReturnsCorrectValue)
    {
        MduData data = MakeMduData({
            {"somekey", StringEnumValue{"somevalue"}}
        });

        EXPECT_EQ(data.getValueAs<StringEnumValue>("somekey").value, "somevalue");
    }

    TEST(MduDataTest, GetValueAs_ExistingIntEnumKey_ReturnsCorrectValue)
    {
        MduData data = MakeMduData({
            {"somekey", IntEnumValue{3}}
        });

        EXPECT_EQ(data.getValueAs<IntEnumValue>("somekey").value, 3);
    }

    TEST(MduDataTest, GetValueAs_ExistingDateTimeKey_ReturnsCorrectValue)
    {
        const auto now = std::chrono::system_clock::now();
        MduData data = MakeMduData({
            {"somekey", now}
        });

        const auto& actual = data.getValueAs<std::optional<std::chrono::system_clock::time_point>>("somekey");
        ASSERT_TRUE(actual.has_value());
        EXPECT_EQ(actual.value(), now);
    }

    TEST(MduDataTest, GetValueAs_NulloptDateTimeKey_ReturnsNullopt)
    {
        MduData data = MakeMduData({
            {"somekey", std::optional<std::chrono::system_clock::time_point>{}}
        });

        const auto& actual = data.getValueAs<std::optional<std::chrono::system_clock::time_point>>("somekey");
        EXPECT_FALSE(actual.has_value());
    }

    TEST(MduDataTest, GetValueAs_ExistingStringListKey_ReturnsCorrectValue)
    {
        const std::vector<std::string> value{"a", "b", "c"};
        MduData data = MakeMduData({
            {"somekey", value}
        });

        EXPECT_EQ(data.getValueAs<std::vector<std::string>>("somekey"), value);
    }

    TEST(MduDataTest, GetValueAs_ExistingPathListKey_ReturnsCorrectValue)
    {
        const std::vector<std::filesystem::path> value{"a.txt", "b.txt"};
        MduData data = MakeMduData({
            {"somekey", value}
        });

        EXPECT_EQ(data.getValueAs<std::vector<std::filesystem::path>>("somekey"), value);
    }

    TEST(MduDataTest, GetValueAs_ExistingFloatListKey_ReturnsCorrectValue)
    {
        const std::vector<double> value{0.1, 0.2, 0.3};
        MduData data = MakeMduData({
            {"somekey", value}
        });

        const auto& result = data.getValueAs<std::vector<double>>("somekey");
        ASSERT_EQ(result.size(), 3u);
        EXPECT_DOUBLE_EQ(result[0], 0.1);
        EXPECT_DOUBLE_EQ(result[1], 0.2);
        EXPECT_DOUBLE_EQ(result[2], 0.3);
    }

    TEST(MduDataTest, GetValueAs_CaseInsensitiveLookup_ReturnsCorrectValue)
    {
        MduData data = MakeMduData({
            {"somekey", 7}
        });

        EXPECT_EQ(data.getValueAs<int>("SomeKey"), 7);
        EXPECT_EQ(data.getValueAs<int>("SOMEKEY"), 7);
    }

    TEST(MduDataTest, GetValueAs_AbsentKey_ThrowsRuntimeError)
    {
        MduData data = MakeMduData({
            {"somekey", 1}
        });

        EXPECT_THROW(data.getValueAs<int>("missing"), std::runtime_error);
    }

    TEST(MduDataTest, GetValueAs_WrongType_ThrowsBadVariantAccess)
    {
        MduData data = MakeMduData({
            {"somekey", 1}
        });

        EXPECT_THROW(data.getValueAs<std::string>("somekey"), std::bad_variant_access);
    }

    // -------------------------------------------------------------------------
    // getValueAs (mutable)
    // -------------------------------------------------------------------------

    TEST(MduDataTest, GetValueAsMutable_ExistingKey_CanModifyValue)
    {
        MduData data = MakeMduData({
            {"somekey", 1}
        });

        data.getValueAs<int>("somekey") = 99;

        EXPECT_EQ(data.getValueAs<int>("somekey"), 99);
    }

    TEST(MduDataTest, GetValueAsMutable_AbsentKey_ThrowsRuntimeError)
    {
        MduData data = MakeMduData({
            {"somekey", 1}
        });

        EXPECT_THROW(data.getValueAs<int>("missing"), std::runtime_error);
    }

    // -------------------------------------------------------------------------
    // setValue
    // -------------------------------------------------------------------------

    TEST(MduDataTest, SetValue_ExistingIntKey_UpdatesValue)
    {
        MduData data = MakeMduData({
            {"somekey", 1}
        });

        data.setValue<int>("somekey", 42);

        EXPECT_EQ(data.getValueAs<int>("somekey"), 42);
    }

    TEST(MduDataTest, SetValue_ExistingFloatKey_UpdatesValue)
    {
        MduData data = MakeMduData({
            {"somekey", 1.0}
        });

        data.setValue<double>("somekey", 2.5);

        EXPECT_DOUBLE_EQ(data.getValueAs<double>("somekey"), 2.5);
    }

    TEST(MduDataTest, SetValue_ExistingStringKey_UpdatesValue)
    {
        MduData data = MakeMduData({
            {"somekey", std::string{"old"}}
        });

        data.setValue<std::string>("somekey", "new");

        EXPECT_EQ(data.getValueAs<std::string>("somekey"), "new");
    }

    TEST(MduDataTest, SetValue_ExistingBoolKey_UpdatesValue)
    {
        MduData data = MakeMduData({
            {"somekey", false}
        });

        data.setValue<bool>("somekey", true);

        EXPECT_TRUE(data.getValueAs<bool>("somekey"));
    }

    TEST(MduDataTest, SetValue_ExistingPathKey_UpdatesValue)
    {
        MduData data = MakeMduData({
            {"somekey", std::filesystem::path{"old.txt"}}
        });

        data.setValue<std::filesystem::path>("somekey", std::filesystem::path{"new.txt"});

        EXPECT_EQ(data.getValueAs<std::filesystem::path>("somekey"), std::filesystem::path{"new.txt"});
    }

    TEST(MduDataTest, SetValue_ExistingStringEnumKey_UpdatesValue)
    {
        MduData data = MakeMduData({
            {"somekey", StringEnumValue{"somevalue"}}
        });

        data.setValue<StringEnumValue>("somekey", StringEnumValue{"newvalue"});

        EXPECT_EQ(data.getValueAs<StringEnumValue>("somekey").value, "newvalue");
    }

    TEST(MduDataTest, SetValue_ExistingIntEnumKey_UpdatesValue)
    {
        MduData data = MakeMduData({
            {"somekey", IntEnumValue{1}}
        });

        data.setValue<IntEnumValue>("somekey", IntEnumValue{2});

        EXPECT_EQ(data.getValueAs<IntEnumValue>("somekey").value, 2);
    }

    TEST(MduDataTest, SetValue_ExistingDateTimeKey_UpdatesValue)
    {
        MduData data = MakeMduData({
            {"somekey", std::optional<std::chrono::system_clock::time_point>{}}
        });

        const auto newValue = std::chrono::system_clock::now();
        data.setValue<std::optional<std::chrono::system_clock::time_point>>("somekey", newValue);

        const auto& actual = data.getValueAs<std::optional<std::chrono::system_clock::time_point>>("somekey");
        ASSERT_TRUE(actual.has_value());
        EXPECT_EQ(actual.value(), newValue);
    }

    TEST(MduDataTest, SetValue_NulloptDateTimeKey_UpdatesValueToNullopt)
    {
        MduData data = MakeMduData({
            {"somekey", std::chrono::system_clock::now()}
        });

        data.setValue<std::optional<std::chrono::system_clock::time_point>>("somekey", std::nullopt);

        const auto& actual = data.getValueAs<std::optional<std::chrono::system_clock::time_point>>("somekey");
        EXPECT_FALSE(actual.has_value());
    }

    TEST(MduDataTest, SetValue_ExistingStringListKey_UpdatesValue)
    {
        MduData data = MakeMduData({
            {"somekey", std::vector<std::string>{"old"}}
        });

        const std::vector<std::string> newValue{"a", "b"};
        data.setValue<std::vector<std::string>>("somekey", newValue);

        EXPECT_EQ(data.getValueAs<std::vector<std::string>>("somekey"), newValue);
    }

    TEST(MduDataTest, SetValue_ExistingPathListKey_UpdatesValue)
    {
        MduData data = MakeMduData({
            {"somekey", std::vector<std::filesystem::path>{"old.txt"}}
        });

        const std::vector<std::filesystem::path> newValue{"a.txt", "b.txt"};
        data.setValue<std::vector<std::filesystem::path>>("somekey", newValue);

        EXPECT_EQ(data.getValueAs<std::vector<std::filesystem::path>>("somekey"), newValue);
    }

    TEST(MduDataTest, SetValue_ExistingFloatListKey_UpdatesValue)
    {
        MduData data = MakeMduData({
            {"somekey", std::vector<double>{0.0}}
        });

        const std::vector<double> newValue{0.5, 0.6};
        data.setValue<std::vector<double>>("somekey", newValue);

        const auto& result = data.getValueAs<std::vector<double>>("somekey");
        ASSERT_EQ(result.size(), 2u);
        EXPECT_DOUBLE_EQ(result[0], 0.5);
        EXPECT_DOUBLE_EQ(result[1], 0.6);
    }

    TEST(MduDataTest, SetValue_CaseInsensitiveKey_UpdatesValue)
    {
        MduData data = MakeMduData({
            {"somekey", 1}
        });

        data.setValue<int>("SomeKey", 42);

        EXPECT_EQ(data.getValueAs<int>("somekey"), 42);
    }

    TEST(MduDataTest, SetValue_AbsentKey_ThrowsRuntimeError)
    {
        MduData data = MakeMduData({
            {"somekey", 1}
        });

        EXPECT_THROW(data.setValue<int>("missing", 1), std::runtime_error);
    }

    TEST(MduDataTest, SetValue_WrongType_ThrowsRuntimeErrorAndPreservesValue)
    {
        MduData data = MakeMduData({
            {"somekey", 1}
        });

        data.setValue<int>("somekey", 22);
        EXPECT_EQ(data.getValueAs<int>("somekey"), 22);

        EXPECT_THROW(data.setValue<std::string>("somekey", "you are a string now"), std::runtime_error);

        EXPECT_EQ(data.getValueAs<int>("somekey"), 22);
    }

    TEST(MduDataTest, SetValue_ValueOverload_ExistingKey_UpdatesValue)
    {
        MduData data = MakeMduData({
            {"somekey", 1}
        });

        data.setValue("somekey", Value(42));

        EXPECT_EQ(data.getValueAs<int>("somekey"), 42);
    }

    // -------------------------------------------------------------------------
    // visitKeyValuePairs
    // -------------------------------------------------------------------------

    TEST(MduDataTest, VisitKeyValuePairs_VisitsEveryStoredEntry)
    {
        MduData data = MakeMduData({
            {"keya", 1},
            {"keyb", 2},
            {"keyc", 3}
        });

        std::map<std::string, int> visited;
        data.visitKeyValuePairs([&](std::string_view key, const Value& value) {
            visited.emplace(std::string(key), std::get<int>(value));
        });

        EXPECT_EQ(visited.size(), 3u);
        EXPECT_EQ(visited.at("keya"), 1);
        EXPECT_EQ(visited.at("keyb"), 2);
        EXPECT_EQ(visited.at("keyc"), 3);
    }

} // namespace dflowfm_io::test