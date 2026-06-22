#include <gtest/gtest.h>

#include <dflowfm_io/MduData.h>

namespace dflowfm_io::test
{

    // -------------------------------------------------------------------------
    // Helpers
    // -------------------------------------------------------------------------

    namespace
    {
        MduData MduDataWithInt(std::string key, int value)
        {
            MduData data;
            data.data_entries[key] = value;
            return data;
        }

        MduData MduDataWithString(std::string key, std::string value)
        {
            MduData data;
            data.data_entries[key] = std::move(value);
            return data;
        }
    } // namespace

    // -------------------------------------------------------------------------
    // hasValue
    // -------------------------------------------------------------------------

    TEST(MduDataTest, HasValue_ExistingKey_ReturnsTrue)
    {
        const MduData data = MduDataWithInt("somekey", 1);

        EXPECT_TRUE(data.hasValue("somekey"));
    }

    TEST(MduDataTest, HasValue_AbsentKey_ReturnsFalse)
    {
        const MduData data;

        EXPECT_FALSE(data.hasValue("somekey"));
    }

    TEST(MduDataTest, HasValue_KeyStoredLowercase_LookupCaseInsensitive)
    {
        const MduData data = MduDataWithInt("somekey", 1);

        EXPECT_TRUE(data.hasValue("SomeKey"));
        EXPECT_TRUE(data.hasValue("SOMEKEY"));
        EXPECT_TRUE(data.hasValue("somekey"));
    }

    TEST(MduDataTest, HasValue_KeyStoredUppercase_LookupCaseInsensitive)
    {
        MduData data;
        data.data_entries["somekey"] = 1; // storage is always lowercase

        EXPECT_TRUE(data.hasValue("SOMEKEY"));
    }

    // -------------------------------------------------------------------------
    // getValueAs (const)
    // -------------------------------------------------------------------------

    TEST(MduDataTest, GetValueAs_ExistingIntKey_ReturnsCorrectValue)
    {
        const MduData data = MduDataWithInt("somekey", 42);

        EXPECT_EQ(data.getValueAs<int>("somekey"), 42);
    }

    TEST(MduDataTest, GetValueAs_ExistingStringKey_ReturnsCorrectValue)
    {
        const MduData data = MduDataWithString("somekey", "hello");

        EXPECT_EQ(data.getValueAs<std::string>("somekey"), "hello");
    }

    TEST(MduDataTest, GetValueAs_CaseInsensitiveLookup_ReturnsCorrectValue)
    {
        const MduData data = MduDataWithInt("somekey", 7);

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
        const MduData data = MduDataWithInt("somekey", 1);

        EXPECT_THROW(data.getValueAs<std::string>("somekey"), std::bad_variant_access);
    }

    // -------------------------------------------------------------------------
    // getValueAs (mutable)
    // -------------------------------------------------------------------------

    TEST(MduDataTest, GetValueAsMutable_ExistingKey_CanModifyValue)
    {
        MduData data = MduDataWithInt("somekey", 1);

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

    TEST(MduDataTest, SetValue_ExistingKey_UpdatesValue)
    {
        MduData data = MduDataWithInt("somekey", 1);

        data.setValue<int>("somekey", 42);

        EXPECT_EQ(data.getValueAs<int>("somekey"), 42);
    }

    TEST(MduDataTest, SetValue_CaseInsensitiveKey_UpdatesValue)
    {
        MduData data = MduDataWithInt("somekey", 1);

        data.setValue<int>("SomeKey", 42);

        EXPECT_EQ(data.getValueAs<int>("somekey"), 42);
    }

    TEST(MduDataTest, SetValue_AbsentKey_ThrowsRuntimeError)
    {
        MduData data;

        EXPECT_THROW(data.setValue<int>("missing", 1), std::runtime_error);
    }

    TEST(MduDataTest, SetValue_StringValue_UpdatesCorrectly)
    {
        MduData data = MduDataWithString("somekey", "old");

        data.setValue<std::string>("somekey", "new");

        EXPECT_EQ(data.getValueAs<std::string>("somekey"), "new");
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
        MduData data = MduDataWithInt("somekey", 1);

        EXPECT_EQ(data.data_entries.size(), 1);
    }

} // namespace dflowfm_io::test