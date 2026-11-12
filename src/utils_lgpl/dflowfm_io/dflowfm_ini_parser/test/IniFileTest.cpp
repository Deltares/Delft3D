#include <gtest/gtest.h>

#include <chrono>
#include <filesystem>
#include <ios>
#include <stdexcept>

#include "ini/IniFile.h"

namespace ini::tests
{

    // ============================================================
    // LoadString
    // ============================================================

    TEST(IniFileTest, LoadString_EmptyString_ReturnsEmptyData)
    {
        IniFile file;

        file.LoadString("");

        EXPECT_TRUE(file.GetData().GetSections().empty());
    }

    TEST(IniFileTest, LoadString_ValidString_PopulatesSection)
    {
        IniFile file;

        file.LoadString("[general]\n");

        EXPECT_TRUE(file.GetData().HasSection("general"));
    }

    TEST(IniFileTest, LoadString_ValidString_PopulatesProperty)
    {
        IniFile file;

        file.LoadString("[general]\nkey=value\n");

        const IniSection& section = file.GetData().GetSection("general");

        EXPECT_TRUE(section.HasProperty("key"));
        EXPECT_EQ(section.GetProperty("key").GetValue(), "value");
    }

    TEST(IniFileTest, LoadString_MultipleSections_PopulatesAllSections)
    {
        IniFile file;

        file.LoadString("[general]\n[physics]\n[numerics]\n");

        const IniData& data = file.GetData();

        EXPECT_TRUE(data.HasSection("general"));
        EXPECT_TRUE(data.HasSection("physics"));
        EXPECT_TRUE(data.HasSection("numerics"));
    }

    TEST(IniFileTest, LoadString_CalledTwice_ReplacesData)
    {
        IniFile file;
        file.LoadString("[general]\n");
        file.LoadString("[physics]\n");

        const IniData& data = file.GetData();

        EXPECT_FALSE(data.HasSection("general"));
        EXPECT_TRUE(data.HasSection("physics"));
    }

    TEST(IniFileTest, LoadString_DuplicateSectionsNotAllowed_ThrowsFailure)
    {
        IniFileOptions options;
        options.parserOptions.allowDuplicateSections = false;

        IniFile file(options);

        EXPECT_THROW(file.LoadString("[general]\n[general]\n"), std::runtime_error);
    }

    // ============================================================
    // Load (path)
    // ============================================================

    TEST(IniFileTest, Load_EmptyPath_ThrowsInvalidArgument)
    {
        IniFile file;

        EXPECT_THROW(file.Load(""), std::invalid_argument);
    }

    TEST(IniFileTest, Load_NonExistentFile_ThrowsFailure)
    {
        IniFile file;

        EXPECT_THROW(file.Load("/nonexistent/path/test.ini"), std::ios_base::failure);
    }

    // ============================================================
    // ToString
    // ============================================================

    TEST(IniFileTest, ToString_EmptyData_ReturnsEmptyString)
    {
        IniFile file;

        EXPECT_TRUE(file.ToString().empty());
    }

    TEST(IniFileTest, ToString_WithSection_WritesSection)
    {
        IniFile file;
        file.GetData().AddSection("general");

        EXPECT_NE(file.ToString().find("[general]"), std::string::npos);
    }

    TEST(IniFileTest, ToString_WithSectionAndProperty_WritesSectionAndProperty)
    {
        IniFile file;
        auto& section = file.GetData().AddSection("general");
        section.AddProperty("key", "value");

        const std::string content = file.ToString();

        EXPECT_NE(content.find("[general]"), std::string::npos);
        EXPECT_NE(content.find("key"), std::string::npos);
        EXPECT_NE(content.find("value"), std::string::npos);
    }

    // ============================================================
    // Save (path)
    // ============================================================

    TEST(IniFileTest, Save_EmptyPath_ThrowsInvalidArgument)
    {
        IniFile file;

        EXPECT_THROW(file.Save(""), std::invalid_argument);
    }

    TEST(IniFileTest, Save_NonExistentDirectory_ThrowsFailure)
    {
        IniFile file;

        EXPECT_THROW(file.Save("/nonexistent/path/test.ini"), std::ios_base::failure);
    }

    // ============================================================
    // Round-trip
    // ============================================================

    TEST(IniFileTest, ToStringThenLoadString_EmptyData_RoundTrips)
    {
        IniFile writer;

        IniFile reader;
        reader.LoadString(writer.ToString());

        EXPECT_TRUE(reader.GetData().GetSections().empty());
    }

    TEST(IniFileTest, ToStringThenLoadString_SingleSection_RoundTrips)
    {
        IniFile writer;
        writer.GetData().AddSection("general");

        IniFile reader;
        reader.LoadString(writer.ToString());

        EXPECT_TRUE(reader.GetData().HasSection("general"));
    }

    TEST(IniFileTest, ToStringThenLoadString_SectionWithProperty_RoundTrips)
    {
        IniFile writer;
        auto& section = writer.GetData().AddSection("general");
        section.AddProperty("key", "value");

        IniFile reader;
        reader.LoadString(writer.ToString());

        const IniData& data = reader.GetData();

        EXPECT_TRUE(data.HasSection("general"));
        EXPECT_TRUE(data.GetSection("general").HasProperty("key"));
        EXPECT_EQ(data.GetSection("general").GetProperty("key").GetValue(), "value");
    }

    // ============================================================
    // SetData
    // ============================================================

    TEST(IniFileTest, SetData_WithEmptyData_ClearsExistingData)
    {
        IniFile file;
        file.GetData().AddSection("general");

        file.SetData(IniData{});

        EXPECT_TRUE(file.GetData().GetSections().empty());
    }

} // namespace ini::tests