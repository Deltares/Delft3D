#include "utils/file_io.hpp"

#include <cerrno>
#include <expected>
#include <filesystem>
#include <format>
#include <fstream>
#include <ios>
#include <sstream>
#include <string>
#include <string_view>
#include <system_error>

#include "utils/io_errors.hpp"

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

        // The current Linux image uses GCC Toolset 14 headers but an older system libstdc++
        // that ignores std::ios::noreplace. This check is not atomic with the subsequent open.
        // Once the OS runtime supports noreplace, replace this check with std::ios::noreplace
        // in the open flags to restore atomic exclusive creation. (std::ofstream(file_path, std::ios::out |
        // std::ios::noreplace))
        std::error_code status_error;
        const bool file_exists = std::filesystem::exists(file_path, status_error);
        if (status_error)
        {
            return std::unexpected(io_errors::WriteError{
                std::format("Failed to check file '{}': {}", file_path.string(), status_error.message())});
        }
        if (file_exists)
        {
            return std::unexpected(io_errors::WriteError{std::format("File already exists: {}", file_path.string())});
        }

        errno = 0;
        std::ofstream output_file(file_path, std::ios::out);
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
