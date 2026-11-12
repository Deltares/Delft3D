#pragma once

#include "ini/IniData.h"
#include "ini/IniOptions.h"

#include <filesystem>
#include <string>

/// @brief Reads and writes INI files without retaining data or options.
///
/// @details Pass an @ref ini::IniFileIOOptions instance to customize parsing and formatting.
namespace ini::file_io
{
    /// @brief Parses the specified INI-formatted string.
    /// @param text The INI-formatted text to parse.
    /// @param options The options controlling parsing behavior.
    /// @return The parsed INI data.
    IniData LoadString(const std::string& text, const IniFileIOOptions& options = {});

    /// @brief Loads and parses the file at the specified path.
    /// @param path The path of the INI file.
    /// @param options The options controlling parsing behavior.
    /// @return The parsed INI data.
    /// @throws std::invalid_argument When @p path is empty.
    /// @throws std::ios_base::failure When the file cannot be read.
    IniData Load(const std::filesystem::path& path, const IniFileIOOptions& options = {});

    /// @brief Formats the specified INI data as an INI-formatted string.
    /// @param data The INI data to format.
    /// @param options The options controlling formatting behavior.
    /// @return The formatted INI text.
    std::string ToString(const IniData& data, const IniFileIOOptions& options = {});

    /// @brief Formats and saves the specified INI data to a file.
    /// @param path The path of the INI file.
    /// @param data The INI data to save.
    /// @param options The options controlling formatting behavior.
    /// @throws std::invalid_argument When @p path is empty.
    /// @throws std::ios_base::failure When the file cannot be written.
    void Save(const std::filesystem::path& path, const IniData& data, const IniFileIOOptions& options = {});

} // namespace ini::file_io