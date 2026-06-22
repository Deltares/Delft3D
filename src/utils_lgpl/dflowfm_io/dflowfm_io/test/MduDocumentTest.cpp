#include <gtest/gtest.h>

#include <dflowfm_io/MduDocument.h>

#include "MduTestData.h"

#include <filesystem>
#include <limits>
#include <sstream>

namespace dflowfm_io::test
{

    class MduDocumentTest : public ::testing::Test
    {
    protected:
        static void SetUpTestSuite() { compliantMduString = std::make_unique<std::string>(MakeCompliantMduString()); }

        static void TearDownTestSuite() { compliantMduString.reset(); }

        static std::istringstream CompliantStream() { return std::istringstream(*compliantMduString); }

        static inline std::unique_ptr<std::string> compliantMduString;
    };

    // -------------------------------------------------------------------------
    // Load(stream)
    // -------------------------------------------------------------------------

    TEST_F(MduDocumentTest, Load_ValidStream_NoErrors)
    {
        auto stream = CompliantStream();
        MduDocument doc;
        doc.Load(stream);

        EXPECT_FALSE(doc.GetReport().HasErrors());
    }

    TEST_F(MduDocumentTest, Load_ValidStream_PopulatesMduData)
    {
        auto stream = CompliantStream();
        MduDocument doc;
        doc.Load(stream);

        EXPECT_FALSE(doc.GetData().data_entries.empty());
    }

    TEST(MduDocumentTest_Stream, Load_FailedStream_ThrowsIosBaseFailure)
    {
        std::istringstream stream;
        stream.setstate(std::ios::failbit);
        MduDocument doc;

        EXPECT_THROW(doc.Load(stream), std::ios_base::failure);
    }

    // -------------------------------------------------------------------------
    // Load(path)
    // -------------------------------------------------------------------------

    TEST(MduDocumentTest_Path, Load_EmptyPath_ThrowsInvalidArgument)
    {
        MduDocument doc;
        EXPECT_THROW(doc.Load(std::filesystem::path{}), std::invalid_argument);
    }

    TEST(MduDocumentTest_Path, Load_NonExistingPath_ThrowsIosBaseFailure)
    {
        MduDocument doc;
        EXPECT_THROW(doc.Load(std::filesystem::path{"nonexistent_file_xyz.mdu"}), std::ios_base::failure);
    }

    // -------------------------------------------------------------------------
    // Save(stream)
    // -------------------------------------------------------------------------

    TEST_F(MduDocumentTest, Save_ValidStream_WritesNonEmptyContent)
    {
        auto stream = CompliantStream();
        MduDocument doc;
        doc.Load(stream);

        std::ostringstream out;
        doc.Save(out);

        EXPECT_FALSE(out.str().empty());
    }

    TEST_F(MduDocumentTest, Save_FailedStream_ThrowsIosBaseFailure)
    {
        auto stream = CompliantStream();
        MduDocument doc;
        doc.Load(stream);

        std::ostringstream out;
        out.setstate(std::ios::failbit);

        EXPECT_THROW(doc.Save(out), std::ios_base::failure);
    }

    // -------------------------------------------------------------------------
    // Save(path)
    // -------------------------------------------------------------------------

    TEST(MduDocumentTest_Path, Save_EmptyPath_ThrowsInvalidArgument)
    {
        MduDocument doc;
        EXPECT_THROW(doc.Save(std::filesystem::path{}), std::invalid_argument);
    }

    // -------------------------------------------------------------------------
    // GetValue / SetValue
    // -------------------------------------------------------------------------

    TEST_F(MduDocumentTest, GetValue_UnknownKey_ThrowsInvalidArgument)
    {
        auto stream = CompliantStream();
        MduDocument doc;
        doc.Load(stream);

        EXPECT_THROW(doc.GetValue<int>("general.nonexistent_xyz"), std::invalid_argument);
    }

    TEST_F(MduDocumentTest, SetValue_UnknownKey_ThrowsInvalidArgument)
    {
        auto stream = CompliantStream();
        MduDocument doc;
        doc.Load(stream);

        EXPECT_THROW(doc.SetValue("general.nonexistent_xyz", 42), std::invalid_argument);
    }

    TEST_F(MduDocumentTest, SetValue_EnumOutOfRange_ThrowsOutOfRange)
    {
        auto stream = CompliantStream();
        MduDocument doc;
        doc.Load(stream);

        const auto [targetSection, targetProperty] = FirstPropertyOfType(ValueType::Enum);
        const std::string key = FormatKey(targetSection->name, targetProperty->key);

        EXPECT_THROW(doc.SetValue(key, EnumValue{std::numeric_limits<int>::max()}), std::out_of_range);
    }

    TEST_F(MduDocumentTest, GetValue_ExistingIntProperty_ReturnsCorrectValue)
    {
        auto stream = CompliantStream();
        MduDocument doc;
        doc.Load(stream);

        const auto [targetSection, targetProperty] = FirstOptionalPropertyWithDefault(ValueType::Int);
        const std::string key = FormatKey(targetSection->name, targetProperty->key);

        const int expected = std::get<int>(ConvertToValue(*targetProperty));

        EXPECT_EQ(doc.GetValue<int>(key), expected);
    }

    TEST_F(MduDocumentTest, SetValue_ValidIntValue_UpdatesData)
    {
        auto stream = CompliantStream();
        MduDocument doc;
        doc.Load(stream);

        const auto [targetSection, targetProperty] = FirstPropertyOfType(ValueType::Int);
        const std::string key = FormatKey(targetSection->name, targetProperty->key);

        doc.SetValue(key, 99);

        EXPECT_EQ(doc.GetValue<int>(key), 99);
    }

} // namespace dflowfm_io::test