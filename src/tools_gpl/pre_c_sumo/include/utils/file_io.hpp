#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_FILE_IO_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_FILE_IO_HPP

#include <expected>
#include <filesystem>
#include <string>
#include <string_view>

#include "utils/io_errors.hpp"

namespace pre_c_sumo::utils::file_io
{
    /**
     * @brief Reads a file as text without parsing or interpreting its contents.
     * @param file_path Path to the file to read.
     * @return File contents, or a ReadError if the file cannot be read.
     */
    [[nodiscard]] std::expected<std::string, io_errors::ReadError> readFile(const std::filesystem::path& file_path);

    /**
     * @brief Writes already-serialized text, creating parent directories if needed.
     *
     * Existing files are not overwritten. The existence check and file creation are not atomic,
     * so concurrent writers to the same path are not supported.
     * @param file_path Path to the destination file.
     * @param content Serialized text to write.
     * @return Success, or a WriteError if the content cannot be written.
     */
    [[nodiscard]] std::expected<void, io_errors::WriteError> writeFile(const std::filesystem::path& file_path,
                                                                       std::string_view content);
} // namespace pre_c_sumo::utils::file_io

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_FILE_IO_HPP
