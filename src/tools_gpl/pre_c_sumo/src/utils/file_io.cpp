#include "utils/file_io.hpp"

#include <cerrno>
#include <format>
#include <fstream>
#include <sstream>
#include <system_error>

namespace pre_c_sumo::utils::file_io
{
    std::expected<std::string, io_errors::ReadError> readFile(const std::filesystem::path& file_path)
    {
        std::ifstream file(file_path);
        if (!file)
        {
            return std::unexpected(io_errors::ReadError{std::format("Cannot open file: {}", file_path.string())});
        }
        std::ostringstream buffer;
        buffer << file.rdbuf();
        if (file.bad() || buffer.bad())
        {
            return std::unexpected(io_errors::ReadError{std::format("Failed to read file: {}", file_path.string())});
        }
        return buffer.str();
    }

    std::expected<void, io_errors::WriteError> writeFile(const std::filesystem::path& file_path,
                                                         std::string_view content)
    {
        const auto directory = file_path.parent_path();
        if (!directory.empty())
        {
            std::error_code error;
            std::filesystem::create_directories(directory, error);
            if (error)
            {
                return std::unexpected(io_errors::WriteError{
                    std::format("Failed to create directory '{}': {}", directory.string(), error.message())});
            }
        }

        errno = 0;
        std::ofstream output_file(file_path, std::ios::out | std::ios::noreplace);
        if (!output_file)
        {
            const auto error = std::error_code(errno == 0 ? EIO : errno, std::generic_category());
            return std::unexpected(io_errors::WriteError{
                std::format("Failed to open file for writing '{}': {}", file_path.string(), error.message())});
        }
        errno = 0;
        output_file << content;
        output_file.close();
        if (!output_file)
        {
            const auto error = std::error_code(errno == 0 ? EIO : errno, std::generic_category());
            return std::unexpected(io_errors::WriteError{
                std::format("Failed to write to file '{}': {}", file_path.string(), error.message())});
        }
        return {};
    }

} // namespace pre_c_sumo::utils::file_io
