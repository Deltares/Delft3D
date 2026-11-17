#include <gtest/gtest.h>

#include "exchange_file_io.hpp"

#include <vector>

namespace
{
    class ExchangeFileTest : public ::testing::Test
    {
    protected:
        ExchangeFileTest()
        {
            operations.create_directories = [this](const std::filesystem::path& path) {
                calls.emplace_back("create_directories");
                directory = path;
                return directory_result;
            };
            operations.write_file = [this](const std::filesystem::path& path, std::string_view content) {
                calls.emplace_back("write_file");
                filename = path;
                written_content = content;
                return write_result;
            };
        }

        pre_c_sumo::ExchangeFileOperations operations;
        std::vector<std::string> calls;
        std::filesystem::path directory;
        std::filesystem::path filename;
        std::string written_content;
        std::expected<void, pre_c_sumo::WriteError> directory_result;
        std::expected<void, pre_c_sumo::WriteError> write_result;
    };
}

TEST_F(ExchangeFileTest, WritesSuppliedContentUnchangedAfterCreatingParentDirectories)
{
    const auto path = std::filesystem::path("output") / "nested" / "FF2NF.xml";
    constexpr std::string_view content = "already serialized content; not necessarily XML";
    const auto result = pre_c_sumo::writeExchangeFile(path, content, operations);

    ASSERT_TRUE(result.has_value()) << result.error().message;
    EXPECT_EQ(calls, (std::vector<std::string>{"create_directories", "write_file"}));
    EXPECT_EQ(directory, path.parent_path());
    EXPECT_EQ(filename, path);
    EXPECT_EQ(written_content, content);
}

TEST_F(ExchangeFileTest, BareFilenameSkipsDirectoryCreation)
{
    const auto result = pre_c_sumo::writeExchangeFile("FF2NF.xml", "<COSUMO/>", operations);
    ASSERT_TRUE(result.has_value()) << result.error().message;
    EXPECT_EQ(calls, (std::vector<std::string>{"write_file"}));
    EXPECT_EQ(filename, std::filesystem::path("FF2NF.xml"));
}

TEST_F(ExchangeFileTest, DirectoryFailureIsPropagatedWithoutWriting)
{
    directory_result = std::unexpected(pre_c_sumo::WriteError{"directory creation failed"});
    const auto result = pre_c_sumo::writeExchangeFile(
        std::filesystem::path("output") / "FF2NF.xml", "<COSUMO/>", operations);
    ASSERT_FALSE(result.has_value());
    EXPECT_EQ(result.error().message, "directory creation failed");
    EXPECT_EQ(calls, (std::vector<std::string>{"create_directories"}));
}

TEST_F(ExchangeFileTest, FileFailureIsPropagated)
{
    write_result = std::unexpected(pre_c_sumo::WriteError{"file writing failed"});
    const auto result = pre_c_sumo::writeExchangeFile(
        std::filesystem::path("output") / "FF2NF.xml", "<COSUMO/>", operations);
    ASSERT_FALSE(result.has_value());
    EXPECT_EQ(result.error().message, "file writing failed");
    EXPECT_EQ(calls, (std::vector<std::string>{"create_directories", "write_file"}));
}

TEST_F(ExchangeFileTest, FailedCallDoesNotAffectSubsequentCall)
{
    auto failing_operations = operations;
    failing_operations.write_file = [](const std::filesystem::path&,
                                       std::string_view) -> std::expected<void, pre_c_sumo::WriteError> {
        return std::unexpected(pre_c_sumo::WriteError{"file writing failed"});
    };
    const auto failed_result = pre_c_sumo::writeExchangeFile("FF2NF.xml", "<COSUMO/>", failing_operations);
    ASSERT_FALSE(failed_result.has_value());
    const auto result = pre_c_sumo::writeExchangeFile("FF2NF.xml", "<COSUMO/>", operations);
    ASSERT_TRUE(result.has_value()) << result.error().message;
    EXPECT_EQ(calls, (std::vector<std::string>{"write_file"}));
}
