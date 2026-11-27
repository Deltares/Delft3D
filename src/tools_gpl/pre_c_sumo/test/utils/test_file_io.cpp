#include <gtest/gtest.h>

#include "cosumo_test_data.hpp"
#include "utils/file_io.hpp"

#include <filesystem>

TEST(FileIO, ReadsExistingFile)
{
    const auto contents = pre_c_sumo::utils::file_io::readFile(pre_c_sumo::test::full_settings_path);
    ASSERT_TRUE(contents.has_value()) << contents.error().message;
    EXPECT_TRUE(contents->contains("<COSUMO>"));
    EXPECT_TRUE(contents->contains("</COSUMO>"));
}

TEST(FileIO, ReportsMissingFile)
{
    const auto path = std::filesystem::path(pre_c_sumo::test::full_settings_path) / "missing.xml";
    const auto missing = pre_c_sumo::utils::file_io::readFile(path);
    ASSERT_FALSE(missing.has_value());
    EXPECT_TRUE(missing.error().message.contains(path.string()));
    EXPECT_TRUE(missing.error().message.starts_with("Cannot open file: "));
}

TEST(FileIO, RejectsEmptyWritePath)
{
    const auto result = pre_c_sumo::utils::file_io::writeFile({}, "content");
    ASSERT_FALSE(result.has_value());
    EXPECT_TRUE(result.error().message.starts_with("Failed to open file for writing "));
}
