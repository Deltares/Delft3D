#include <gtest/gtest.h>

#include <filesystem>
#include <sstream>
#include <stdexcept>

#include "ini/IniFile.h"

namespace ini::tests
{

    // ============================================================
    // Load (path)
    // ============================================================

    TEST(IniFileTest, Load_EmptyPath_ThrowsInvalidArgument)
    {
        EXPECT_THROW(Load(""), std::invalid_argument);
    }

    TEST(IniFileTest, Load_NonExistentFile_ThrowsFailure)
    {
        EXPECT_THROW(Load("/nonexistent/path/test.ini"), std::ios_base::failure);
    }

    // ============================================================
    // Load (stream)
    // ============================================================

    TEST(IniFileTest, Load_FailedStream_ThrowsFailure)
    {
        std::istringstream stream;
        stream.setstate(std::ios::failbit);
        EXPECT_THROW(Load(stream), std::ios_base::failure);
    }

    TEST(IniFileTest, Load_EmptyStream_ReturnsEmptyData)
    {
        std::istringstream stream("");
        const IniData data = Load(stream);

        EXPECT_TRUE(data.GetSections().empty());
    }

    TEST(IniFileTest, Load_ValidStream_PopulatesSection)
    {
        std::istringstream stream("[general]\n");
        const IniData data = Load(stream);

        EXPECT_TRUE(data.HasSection("general"));
    }

    TEST(IniFileTest, Load_ValidStream_PopulatesProperty)
    {
        std::istringstream stream("[general]\nkey=value\n");
        const IniData data = Load(stream);

        const IniSection& section = data.GetSection("general");

        EXPECT_TRUE(section.HasProperty("key"));
        EXPECT_EQ(section.GetProperty("key").GetValue(), "value");
    }

    TEST(IniFileTest, Load_MultipleSections_PopulatesAllSections)
    {
        std::istringstream stream("[general]\n[physics]\n[numerics]\n");
        const IniData data = Load(stream);

        EXPECT_TRUE(data.HasSection("general"));
        EXPECT_TRUE(data.HasSection("physics"));
        EXPECT_TRUE(data.HasSection("numerics"));
    }

    TEST(IniFileTest, Load_CalledTwice_ReturnsIndependentData)
    {
        std::istringstream first("[general]\n");
        std::istringstream second("[physics]\n");
        const IniData firstData = Load(first);
        const IniData secondData = Load(second);

        EXPECT_TRUE(firstData.HasSection("general"));
        EXPECT_FALSE(firstData.HasSection("physics"));
        EXPECT_FALSE(secondData.HasSection("general"));
        EXPECT_TRUE(secondData.HasSection("physics"));
    }

    TEST(IniFileTest, Load_DuplicateSectionsNotAllowed_ThrowsFailure)
    {
        std::istringstream stream("[general]\n[general]\n");

        IniFileOptions options;
        options.parserOptions.allowDuplicateSections = false;

        EXPECT_THROW(Load(stream, options), std::runtime_error);
    }

    // ============================================================
    // Save (path)
    // ============================================================

    TEST(IniFileTest, Save_EmptyPath_ThrowsInvalidArgument)
    {
        EXPECT_THROW(Save("", IniData{}), std::invalid_argument);
    }

    TEST(IniFileTest, Save_NonExistentDirectory_ThrowsFailure)
    {
        EXPECT_THROW(Save("/nonexistent/path/test.ini", IniData{}), std::ios_base::failure);
    }

    // ============================================================
    // Save (stream)
    // ============================================================

    TEST(IniFileTest, Save_FailedStream_ThrowsFailure)
    {
        std::ostringstream stream;
        stream.setstate(std::ios::failbit);
        EXPECT_THROW(Save(stream, IniData{}), std::ios_base::failure);
    }

    TEST(IniFileTest, Save_WithSection_WritesSection)
    {
        std::ostringstream stream;
        IniData data;
        data.AddSection("general");

        Save(stream, data);

        EXPECT_NE(stream.str().find("[general]"), std::string::npos);
    }

    TEST(IniFileTest, Save_WithSectionAndProperty_WritesSectionAndProperty)
    {
        std::ostringstream stream;
        IniData data;
        auto& section = data.AddSection("general");
        section.AddProperty("key", "value");

        Save(stream, data);

        const std::string content = stream.str();

        EXPECT_NE(content.find("[general]"), std::string::npos);
        EXPECT_NE(content.find("key"), std::string::npos);
        EXPECT_NE(content.find("value"), std::string::npos);
    }

    // ============================================================
    // Round-trip
    // ============================================================

    TEST(IniFileTest, SaveThenLoad_EmptyData_RoundTrips)
    {
        std::stringstream stream;
        Save(stream, IniData{});
        const IniData data = Load(stream);

        EXPECT_TRUE(data.GetSections().empty());
    }

    TEST(IniFileTest, SaveThenLoad_SingleSection_RoundTrips)
    {
        std::stringstream stream;
        IniData original;
        original.AddSection("general");
        Save(stream, original);

        const IniData data = Load(stream);

        EXPECT_TRUE(data.HasSection("general"));
    }

    TEST(IniFileTest, SaveThenLoad_SectionWithProperty_RoundTrips)
    {
        std::stringstream stream;
        IniData original;
        auto& section = original.AddSection("general");
        section.AddProperty("key", "value");
        Save(stream, original);

        const IniData data = Load(stream);

        EXPECT_TRUE(data.HasSection("general"));
        EXPECT_TRUE(data.GetSection("general").HasProperty("key"));
        EXPECT_EQ(data.GetSection("general").GetProperty("key").GetValue(), "value");
    }

} // namespace ini::tests
