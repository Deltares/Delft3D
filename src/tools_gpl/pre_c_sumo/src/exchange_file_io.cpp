#include "exchange_file_io.hpp"
#include "monadic_utils.hpp"

#include <cerrno>
#include <format>
#include <fstream>
#include <sstream>
#include <system_error>

namespace pre_c_sumo
{
    ExchangeFileOperations::ExchangeFileOperations()
        : create_directories([](const std::filesystem::path& directory) -> std::expected<void, WriteError> {
              std::error_code error;
              std::filesystem::create_directories(directory, error);
              if (error)
              {
                  return std::unexpected(WriteError{
                      std::format("Failed to create directory '{}': {}", directory.string(), error.message())});
              }
              return {};
          }),
          write_file([](const std::filesystem::path& file_path,
                        std::string_view xml) -> std::expected<void, WriteError> {
              errno = 0;
              std::ofstream output_file(file_path, std::ios::out | std::ios::noreplace);
              if (!output_file)
              {
                  const auto error = std::error_code(errno == 0 ? EIO : errno, std::generic_category());
                  return std::unexpected(WriteError{
                      std::format("Failed to open file for writing '{}': {}", file_path.string(), error.message())});
              }
              errno = 0;
              output_file << xml;
              output_file.close();
              if (!output_file)
              {
                  const auto error = std::error_code(errno == 0 ? EIO : errno, std::generic_category());
                  return std::unexpected(
                      WriteError{std::format("Failed to write to file '{}': {}", file_path.string(), error.message())});
              }
              return {};
          })
    {
    }

    std::expected<std::string, parsing_utils::ParseError> readExchangeFile(
        const std::filesystem::path& file_path)
    {
        std::ifstream file(file_path);
        if (!file)
        {
            return std::unexpected(parsing_utils::ParseError{std::format("Cannot open file: {}", file_path.string())});
        }
        std::ostringstream buffer;
        buffer << file.rdbuf();
        if (file.bad() || buffer.bad())
        {
            return std::unexpected(parsing_utils::ParseError{std::format("Failed to read file: {}", file_path.string())});
        }
        return buffer.str();
    }

    std::expected<void, WriteError> writeExchangeFile(const std::filesystem::path& file_path,
                                                        std::string_view xml, const ExchangeFileOperations& file_operations)
    {
        const auto directory = file_path.parent_path();
        if (!directory.empty())
        {
            RETURN_IF_ERROR(file_operations.create_directories(directory));
        }
        return file_operations.write_file(file_path, xml);
    }

} // namespace pre_c_sumo
