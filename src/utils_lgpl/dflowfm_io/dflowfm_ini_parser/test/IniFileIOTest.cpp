#include <gtest/gtest.h>

#include <filesystem>
#include <ios>
#include <stdexcept>

#include "ini/IniFileIO.h"

namespace ini::tests
{

    // ============================================================
    // LoadString
    // ============================================================

    TEST(IniFileIOTest, LoadString_EmptyString_ReturnsEmptyData)
    {
        const IniData data = file_io::LoadString("");

        EXPECT_TRUE(data.GetSections().empty());
    }

    TEST(IniFileIOTest, LoadString_ValidString_PopulatesSection)
    {
        const IniData data = file_io::LoadString("[general]\n");

        EXPECT_TRUE(data.HasSection("general"));
    }

    TEST(IniFileIOTest, LoadString_ValidString_PopulatesProperty)
    {
        const IniData data = file_io::LoadString("[general]\nkey=value\n");

        const IniSection& section = data.GetSection("general");

        EXPECT_TRUE(section.HasProperty("key"));
        EXPECT_EQ(section.GetProperty("key").GetValue(), "value");
    }

    TEST(IniFileIOTest, LoadString_MultipleSections_PopulatesAllSections)
    {
        const IniData data = file_io::LoadString("[general]\n[physics]\n[numerics]\n");

        EXPECT_TRUE(data.HasSection("general"));
        EXPECT_TRUE(data.HasSection("physics"));
        EXPECT_TRUE(data.HasSection("numerics"));
    }

    TEST(IniFileIOTest, LoadString_CalledTwice_ReturnsIndependentData)
    {
        const IniData first = file_io::LoadString("[general]\n");
        const IniData second = file_io::LoadString("[physics]\n");

        EXPECT_TRUE(first.HasSection("general"));
        EXPECT_FALSE(first.HasSection("physics"));
        EXPECT_FALSE(second.HasSection("general"));
        EXPECT_TRUE(second.HasSection("physics"));
    }

    TEST(IniFileIOTest, LoadString_DuplicateSectionsNotAllowed_ThrowsFailure)
    {
        IniFileIOOptions options;
        options.parserOptions.allowDuplicateSections = false;

        EXPECT_THROW(file_io::LoadString("[general]\n[general]\n", options), std::runtime_error);
    }

    // ============================================================
    // Load (path)
    // ============================================================

    TEST(IniFileIOTest, Load_EmptyPath_ThrowsInvalidArgument)
    {
        EXPECT_THROW(file_io::Load(""), std::invalid_argument);
    }

    TEST(IniFileIOTest, Load_NonExistentFile_ThrowsFailure)
    {
        EXPECT_THROW(file_io::Load("/nonexistent/path/test.ini"), std::ios_base::failure);
    }

    // ============================================================
    // ToString
    // ============================================================

    TEST(IniFileIOTest, ToString_EmptyData_ReturnsEmptyString)
    {
        EXPECT_TRUE(file_io::ToString(IniData{}).empty());
    }

    TEST(IniFileIOTest, ToString_WithSection_WritesSection)
    {
        IniData data;
        data.AddSection("general");

        EXPECT_NE(file_io::ToString(data).find("[general]"), std::string::npos);
    }

    TEST(IniFileIOTest, ToString_WithSectionAndProperty_WritesSectionAndProperty)
    {
        IniData data;
        auto& section = data.AddSection("general");
        section.AddProperty("key", "value");

        const std::string content = file_io::ToString(data);

        EXPECT_NE(content.find("[general]"), std::string::npos);
        EXPECT_NE(content.find("key"), std::string::npos);
        EXPECT_NE(content.find("value"), std::string::npos);
    }

    // ============================================================
    // Save (path)
    // ============================================================

    TEST(IniFileIOTest, Save_EmptyPath_ThrowsInvalidArgument)
    {
        EXPECT_THROW(file_io::Save("", IniData{}), std::invalid_argument);
    }

    TEST(IniFileIOTest, Save_NonExistentDirectory_ThrowsFailure)
    {
        EXPECT_THROW(file_io::Save("/nonexistent/path/test.ini", IniData{}), std::ios_base::failure);
    }

    // ============================================================
    // Round-trip
    // ============================================================

    TEST(IniFileIOTest, ToStringThenLoadString_EmptyData_RoundTrips)
    {
        const IniData data = file_io::LoadString(file_io::ToString(IniData{}));

        EXPECT_TRUE(data.GetSections().empty());
    }

    TEST(IniFileIOTest, ToStringThenLoadString_SingleSection_RoundTrips)
    {
        IniData original;
        original.AddSection("general");

        const IniData data = file_io::LoadString(file_io::ToString(original));

        EXPECT_TRUE(data.HasSection("general"));
    }

    TEST(IniFileIOTest, ToStringThenLoadString_SectionWithProperty_RoundTrips)
    {
        IniData original;
        auto& section = original.AddSection("general");
        section.AddProperty("key", "value");

        const IniData data = file_io::LoadString(file_io::ToString(original));

        EXPECT_TRUE(data.HasSection("general"));
        EXPECT_TRUE(data.GetSection("general").HasProperty("key"));
        EXPECT_EQ(data.GetSection("general").GetProperty("key").GetValue(), "value");
    }

    TEST(IniFileIOTest, ToStringThenLoadString_CustomOptions_RoundTrips)
    {
        IniFileIOOptions options;
        options.scheme.propertyAssignmentDelimiter = ':';
        options.formatterOptions = IniFormatterOptions::EmptySpace();

        IniData original;
        original.AddSection("general").AddProperty("key", "value");

        const std::string content = file_io::ToString(original, options);
        EXPECT_NE(content.find("key:value"), std::string::npos);

        const IniData data = file_io::LoadString(content, options);
        EXPECT_EQ(data.GetSection("general").GetProperty("key").GetValue(), "value");
    }

} // namespace ini::tests