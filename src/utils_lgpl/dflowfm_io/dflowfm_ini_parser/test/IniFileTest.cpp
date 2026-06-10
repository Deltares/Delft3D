#include <gtest/gtest.h>

#include <filesystem>
#include <sstream>
#include <stdexcept>

#include "ini/IniFile.h"

namespace ini::tests
{

    // ============================================================
    // Load (stream)
    // ============================================================

    TEST(IniFileTest, Load_FailedStream_ThrowsFailure)
    {
        std::istringstream stream;
        stream.setstate(std::ios::failbit);
        IniFile file;

        EXPECT_THROW(file.Load(stream), std::ios_base::failure);
    }

    TEST(IniFileTest, Load_EmptyStream_ReturnsEmptyData)
    {
        std::istringstream stream("");
        IniFile file;

        file.Load(stream);

        EXPECT_TRUE(file.GetData().empty());
    }

    TEST(IniFileTest, Load_ValidStream_PopulatesSection)
    {
        std::istringstream stream("[general]\n");
        IniFile file;

        file.Load(stream);

        EXPECT_TRUE(file.GetData().HasSection("general"));
    }

    TEST(IniFileTest, Load_ValidStream_PopulatesProperty)
    {
        std::istringstream stream("[general]\nkey=value\n");
        IniFile file;

        file.Load(stream);

        const IniSection& section = file.GetData().GetSection("general");

        EXPECT_TRUE(section.HasProperty("key"));
        EXPECT_EQ(section.GetProperty("key").GetValue(), "value");
    }

    TEST(IniFileTest, Load_MultipleSections_PopulatesAllSections)
    {
        std::istringstream stream("[general]\n[physics]\n[numerics]\n");
        IniFile file;

        file.Load(stream);

        const IniData& data = file.GetData();

        EXPECT_TRUE(data.HasSection("general"));
        EXPECT_TRUE(data.HasSection("physics"));
        EXPECT_TRUE(data.HasSection("numerics"));
    }

    TEST(IniFileTest, Load_CalledTwice_ReplacesData)
    {
        IniFile file;
        std::istringstream first("[general]\n");
        file.Load(first);

        std::istringstream second("[physics]\n");
        file.Load(second);

        const IniData& data = file.GetData();

        EXPECT_FALSE(data.HasSection("general"));
        EXPECT_TRUE(data.HasSection("physics"));
    }

    TEST(IniFileTest, Load_DuplicateSectionsNotAllowed_ThrowsFailure)
    {
        std::istringstream stream("[general]\n[general]\n");

        IniFileOptions options;
        options.parserOptions.allowDuplicateSections = false;

        IniFile file(options);

        EXPECT_THROW(file.Load(stream), std::runtime_error);
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
    // Save (stream)
    // ============================================================

    TEST(IniFileTest, Save_FailedStream_ThrowsFailure)
    {
        std::ostringstream stream;
        stream.setstate(std::ios::failbit);
        IniFile file;

        EXPECT_THROW(file.Save(stream), std::ios_base::failure);
    }

    TEST(IniFileTest, Save_WithSection_WritesSection)
    {
        std::ostringstream stream;
        IniFile file;
        file.GetData().AddSection("general");

        file.Save(stream);

        EXPECT_NE(stream.str().find("[general]"), std::string::npos);
    }

    TEST(IniFileTest, Save_WithSectionAndProperty_WritesSectionAndProperty)
    {
        std::ostringstream stream;
        IniFile file;
        auto& section = file.GetData().AddSection("general");
        section.AddProperty("key", "value");

        file.Save(stream);

        const std::string content = stream.str();

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

    TEST(IniFileTest, SaveThenLoad_EmptyData_RoundTrips)
    {
        std::stringstream stream;
        IniFile writer;
        writer.Save(stream);

        IniFile reader;
        reader.Load(stream);

        EXPECT_TRUE(reader.GetData().empty());
    }

    TEST(IniFileTest, SaveThenLoad_SingleSection_RoundTrips)
    {
        std::stringstream stream;
        IniFile writer;
        writer.GetData().AddSection("general");
        writer.Save(stream);

        IniFile reader;
        reader.Load(stream);

        EXPECT_TRUE(reader.GetData().HasSection("general"));
    }

    TEST(IniFileTest, SaveThenLoad_SectionWithProperty_RoundTrips)
    {
        std::stringstream stream;
        IniFile writer;
        auto& section = writer.GetData().AddSection("general");
        section.AddProperty("key", "value");
        writer.Save(stream);

        IniFile reader;
        reader.Load(stream);

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

        EXPECT_TRUE(file.GetData().empty());
    }

} // namespace ini::tests