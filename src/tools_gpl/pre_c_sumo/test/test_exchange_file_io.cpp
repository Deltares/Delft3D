#include <gtest/gtest.h>

#include "csumo_exchange.hpp"
#include "coupling_test_data.hpp"
#include "csumo_file_paths.hpp"
#include "exchange_file_io.hpp"

#include <filesystem>
#include <random>
#include <string>
#include <system_error>
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

    class ExchangeFileFilesystemTest : public ::testing::Test
    {
    protected:
        void SetUp() override
        {
            std::random_device random;
            for (int attempt = 0; attempt < 10; ++attempt)
            {
                const auto candidate = std::filesystem::temp_directory_path() /
                                       ("pre_c_sumo_" + std::to_string(random()) + "_" + std::to_string(random()));
                std::error_code error;
                if (std::filesystem::create_directory(candidate, error))
                {
                    directory = candidate;
                    return;
                }
                ASSERT_FALSE(error) << error.message();
            }
            FAIL() << "Unable to create a unique temporary directory";
        }

        void TearDown() override
        {
            if (!directory.empty())
            {
                std::error_code error;
                std::filesystem::remove_all(directory, error);
                EXPECT_FALSE(error) << error.message();
            }
        }

        std::filesystem::path directory;
    };
} // namespace

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
    const auto result =
        pre_c_sumo::writeExchangeFile(std::filesystem::path("output") / "FF2NF.xml", "<COSUMO/>", operations);
    ASSERT_FALSE(result.has_value());
    EXPECT_EQ(result.error().message, "directory creation failed");
    EXPECT_EQ(calls, (std::vector<std::string>{"create_directories"}));
}

TEST_F(ExchangeFileTest, FileFailureIsPropagated)
{
    write_result = std::unexpected(pre_c_sumo::WriteError{"file writing failed"});
    const auto result =
        pre_c_sumo::writeExchangeFile(std::filesystem::path("output") / "FF2NF.xml", "<COSUMO/>", operations);
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

TEST_F(ExchangeFileFilesystemTest, WritesReadsAndRefusesToOverwriteExistingFile)
{
    const auto path = directory / "nested" / "FF2NF.xml";
    const auto written = pre_c_sumo::writeExchangeFile(path, "first content");
    ASSERT_TRUE(written.has_value()) << written.error().message;

    const auto overwritten = pre_c_sumo::writeExchangeFile(path, "replacement");
    EXPECT_FALSE(overwritten.has_value());

    const auto contents = pre_c_sumo::readExchangeFile(path);
    ASSERT_TRUE(contents.has_value()) << contents.error().message;
    EXPECT_EQ(*contents, "first content");
}

TEST_F(ExchangeFileFilesystemTest, ReadsNF2FFInDiffuserOrderAndReportsLaterFailures)
{
    pre_c_sumo::CSumoSettings settings;
    settings.diffusers.resize(2);
    settings.diffusers[0].ff2nf_dir = directory / "FF2NF";
    settings.diffusers[1].ff2nf_dir = directory / "FF2NF";
    const auto paths = pre_c_sumo::nf2ffFilepaths(settings, 90.0);
    ASSERT_EQ(paths.size(), 2u);

    const auto first = pre_c_sumo::writeExchangeFile(
        paths[0], coupling_test_data::build_nf2ff_xml(0, 1, false, 0, 0, 10.0));
    ASSERT_TRUE(first.has_value()) << first.error().message;
    const auto second = pre_c_sumo::writeExchangeFile(
        paths[1], coupling_test_data::build_nf2ff_xml(0, 1, false, 0, 0, 20.0));
    ASSERT_TRUE(second.has_value()) << second.error().message;

    const auto results = pre_c_sumo::readNF2FFFiles(settings, 90.0);
    ASSERT_TRUE(results.has_value()) << results.error().message;
    ASSERT_EQ(results->size(), 2u);
    EXPECT_DOUBLE_EQ((*results)[0].source_flow_rate, 10.0);
    EXPECT_DOUBLE_EQ((*results)[1].source_flow_rate, 20.0);

    ASSERT_TRUE(std::filesystem::remove(paths[1]));
    const auto missing = pre_c_sumo::readNF2FFFiles(settings, 90.0);
    ASSERT_FALSE(missing.has_value());
    EXPECT_TRUE(missing.error().message.contains(paths[1].string()));

    const auto malformed_file = pre_c_sumo::writeExchangeFile(paths[1], "<NF2FF>");
    ASSERT_TRUE(malformed_file.has_value()) << malformed_file.error().message;
    const auto malformed = pre_c_sumo::readNF2FFFiles(settings, 90.0);
    ASSERT_FALSE(malformed.has_value());
    EXPECT_TRUE(malformed.error().message.contains(paths[1].string()));
    EXPECT_TRUE(malformed.error().message.contains("Error parsing NF2FF file"));
}
