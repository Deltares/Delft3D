#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_FILE_IO_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_FILE_IO_HPP

#include <expected>
#include <filesystem>
#include <string>
#include <string_view>

#include "utils/io_errors.hpp"

namespace pre_c_sumo::utils::file_io
{
    /// Read text without parsing or interpreting its contents.
    [[nodiscard]] std::expected<std::string, io_errors::ReadError> readFile(const std::filesystem::path& file_path);

    /// Write already-serialized content, creating parent directories and rejecting files that already exist.
    /// The existence check and file creation are not atomic; concurrent writers are not supported.
    [[nodiscard]] std::expected<void, io_errors::WriteError> writeFile(const std::filesystem::path& file_path,
                                                                       std::string_view content);
} // namespace pre_c_sumo::utils::file_io

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_FILE_IO_HPP
