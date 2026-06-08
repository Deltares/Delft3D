#include <gtest/gtest.h>

#include <filesystem>
#include <fstream>
#include <stdexcept>

#include "ini/IniFile.h"

namespace ini::tests
{

    class IniFileTest : public ::testing::Test
    {
    protected:
        void SetUp() override
        {
            tempDir = std::filesystem::temp_directory_path() / "IniFileTest";
            std::filesystem::create_directories(tempDir);
        }

        void TearDown() override { std::filesystem::remove_all(tempDir); }

        std::filesystem::path CreateFile(const std::string& filename, const std::string& content) const
        {
            const auto path = tempDir / filename;
            std::ofstream stream(path);
            stream << content;
            return path;
        }

        std::filesystem::path tempDir;
    };

    // ============================================================
    // Constructor
    // ============================================================

    TEST_F(IniFileTest, Constructor_EmptyPath_ThrowsInvalidArgument)
    {
        EXPECT_THROW(IniFile(""), std::invalid_argument);
    }

    TEST_F(IniFileTest, Constructor_ValidPath_SetsPath)
    {
        const auto path = tempDir / "test.ini";

        const IniFile file(path);

        EXPECT_EQ(file.GetPath(), path);
    }

    TEST_F(IniFileTest, Constructor_ValidPath_DataIsEmpty)
    {
        const IniFile file(tempDir / "test.ini");

        EXPECT_TRUE(file.GetData().empty());
    }

    // ============================================================
    // LoadFrom
    // ============================================================

    TEST_F(IniFileTest, LoadFrom_EmptyPath_ThrowsInvalidArgument)
    {
        EXPECT_THROW(IniFile::LoadFrom(""), std::invalid_argument);
    }

    TEST_F(IniFileTest, LoadFrom_NonExistentFile_ThrowsFailure)
    {
        EXPECT_THROW(IniFile::LoadFrom(tempDir / "nonexistent.ini"), std::ios_base::failure);
    }

    TEST_F(IniFileTest, LoadFrom_Directory_ThrowsFailure)
    {
        EXPECT_THROW(IniFile::LoadFrom(tempDir), std::ios_base::failure);
    }

    TEST_F(IniFileTest, LoadFrom_EmptyFile_ReturnsEmptyData)
    {
        const auto path = CreateFile("empty.ini", "");

        const IniFile file = IniFile::LoadFrom(path);

        EXPECT_TRUE(file.GetData().empty());
    }

    TEST_F(IniFileTest, LoadFrom_ValidFile_SetsPath)
    {
        const auto path = CreateFile("test.ini", "[general]\n");

        const IniFile file = IniFile::LoadFrom(path);

        EXPECT_EQ(file.GetPath(), path);
    }

    TEST_F(IniFileTest, LoadFrom_ValidFile_PopulatesSection)
    {
        const auto path = CreateFile("test.ini", "[general]\n");

        const IniFile file = IniFile::LoadFrom(path);

        EXPECT_TRUE(file.GetData().HasSection("general"));
    }

    TEST_F(IniFileTest, LoadFrom_ValidFile_PopulatesProperty)
    {
        const auto path = CreateFile("test.ini", "[general]\nkey=value\n");

        const IniFile file = IniFile::LoadFrom(path);

        const IniSection& section = file.GetData().GetSection("general");

        EXPECT_TRUE(section.HasProperty("key"));
        EXPECT_EQ(section.GetProperty("key").GetValue(), "value");
    }

    TEST_F(IniFileTest, LoadFrom_MultipleSections_PopulatesAllSections)
    {
        const auto path = CreateFile("test.ini", "[general]\n[physics]\n[numerics]\n");

        const IniFile file = IniFile::LoadFrom(path);

        const IniData& data = file.GetData();

        EXPECT_TRUE(data.HasSection("general"));
        EXPECT_TRUE(data.HasSection("physics"));
        EXPECT_TRUE(data.HasSection("numerics"));
    }

    // ============================================================
    // Load
    // ============================================================

    TEST_F(IniFileTest, Load_NonExistentFile_ThrowsFailure)
    {
        IniFile file(tempDir / "nonexistent.ini");

        EXPECT_THROW(file.Load(), std::ios_base::failure);
    }

    TEST_F(IniFileTest, Load_ValidFile_PopulatesData)
    {
        const auto path = CreateFile("test.ini", "[general]\nkey=value\n");
        IniFile file(path);

        file.Load();

        EXPECT_TRUE(file.GetData().HasSection("general"));
    }

    TEST_F(IniFileTest, Load_CalledTwice_ReplacesData)
    {
        const auto path = CreateFile("test.ini", "[general]\n");
        IniFile file(path);
        file.Load();

        CreateFile("test.ini", "[physics]\n");
        file.Load();

        const IniData& data = file.GetData();

        EXPECT_FALSE(data.HasSection("general"));
        EXPECT_TRUE(data.HasSection("physics"));
    }

    // ============================================================
    // Save
    // ============================================================

    TEST_F(IniFileTest, Save_NonExistentDirectory_ThrowsFailure)
    {
        IniFile file(tempDir / "nonexistent" / "test.ini");

        EXPECT_THROW(file.Save(), std::ios_base::failure);
    }

    TEST_F(IniFileTest, Save_ValidPath_CreatesFile)
    {
        const auto path = tempDir / "output.ini";
        IniFile file(path);

        file.Save();

        EXPECT_TRUE(std::filesystem::exists(path));
    }

    TEST_F(IniFileTest, Save_WithSection_WritesSection)
    {
        const auto path = tempDir / "output.ini";
        IniFile file(path);
        file.GetData().AddSection("general");

        file.Save();

        std::ifstream stream(path);
        const std::string content((std::istreambuf_iterator<char>(stream)), std::istreambuf_iterator<char>());

        EXPECT_NE(content.find("[general]"), std::string::npos);
    }

    TEST_F(IniFileTest, Save_WithSectionAndProperty_WritesSectionAndProperty)
    {
        const auto path = tempDir / "output.ini";
        IniFile file(path);
        auto& section = file.GetData().AddSection("general");
        section.AddProperty("key", "value");

        file.Save();

        std::ifstream stream(path);
        const std::string content((std::istreambuf_iterator<char>(stream)), std::istreambuf_iterator<char>());

        EXPECT_NE(content.find("[general]"), std::string::npos);
        EXPECT_NE(content.find("key"), std::string::npos);
        EXPECT_NE(content.find("value"), std::string::npos);
    }

    // ============================================================
    // Round-trip
    // ============================================================

    TEST_F(IniFileTest, SaveThenLoad_EmptyData_RoundTrips)
    {
        const auto path = tempDir / "roundtrip.ini";

        IniFile writer(path);
        writer.Save();

        const IniFile reader = IniFile::LoadFrom(path);

        EXPECT_TRUE(reader.GetData().empty());
    }

    TEST_F(IniFileTest, SaveThenLoad_SingleSection_RoundTrips)
    {
        const auto path = tempDir / "roundtrip.ini";

        IniFile writer(path);
        writer.GetData().AddSection("general");
        writer.Save();

        const IniFile reader = IniFile::LoadFrom(path);

        EXPECT_TRUE(reader.GetData().HasSection("general"));
    }

    TEST_F(IniFileTest, SaveThenLoad_SectionWithProperty_RoundTrips)
    {
        const auto path = tempDir / "roundtrip.ini";

        IniFile writer(path);
        auto& section = writer.GetData().AddSection("general");
        section.AddProperty("key", "value");
        writer.Save();

        const IniFile reader = IniFile::LoadFrom(path);
        const IniData& data = reader.GetData();

        EXPECT_TRUE(data.HasSection("general"));
        EXPECT_TRUE(data.GetSection("general").HasProperty("key"));
        EXPECT_EQ(data.GetSection("general").GetProperty("key").GetValue(), "value");
    }

    // ============================================================
    // SetData
    // ============================================================

    TEST_F(IniFileTest, SetData_WithEmptyData_ClearsExistingData)
    {
        IniFile file(tempDir / "test.ini");
        file.GetData().AddSection("general");

        file.SetData(IniData{});

        EXPECT_TRUE(file.GetData().empty());
    }

} // namespace ini::tests