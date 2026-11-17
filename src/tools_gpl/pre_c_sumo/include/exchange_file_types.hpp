#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_EXCHANGE_FILE_TYPES_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_EXCHANGE_FILE_TYPES_HPP

#include <expected>
#include <filesystem>
#include <functional>
#include <string>
#include <string_view>

namespace pre_c_sumo
{
    struct WriteError
    {
        std::string message;
    };

    /**
     * @brief Synchronous filesystem operations; content views are valid only during write_file.
     */
    struct ExchangeFileOperations
    {
        ExchangeFileOperations();

        std::function<std::expected<void, WriteError>(const std::filesystem::path&)> create_directories;
        std::function<std::expected<void, WriteError>(const std::filesystem::path&, std::string_view)> write_file;
    };
} // namespace pre_c_sumo

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_EXCHANGE_FILE_TYPES_HPP
