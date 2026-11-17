#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_EXCHANGE_FILE_IO_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_EXCHANGE_FILE_IO_HPP

#include "exchange_file_types.hpp"
#include "parsing_types.hpp"

namespace pre_c_sumo
{
    /// Read text without parsing or interpreting its contents.
    [[nodiscard]] std::expected<std::string, parsing_utils::ParseError> readExchangeFile(
        const std::filesystem::path& file_path);

    /// Write already-serialized content, creating parent directories and never overwriting an existing file.
    [[nodiscard]] std::expected<void, WriteError> writeExchangeFile(
        const std::filesystem::path& file_path, std::string_view content,
        const ExchangeFileOperations& file_operations = {});
} // namespace pre_c_sumo

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_EXCHANGE_FILE_IO_HPP
