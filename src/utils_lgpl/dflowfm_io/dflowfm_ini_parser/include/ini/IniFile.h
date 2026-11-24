#pragma once

#include "ini/IniData.h"
#include "ini/IniOptions.h"

#include <filesystem>
#include <istream>
#include <ostream>

namespace ini
{

    /// @brief Loads and parses INI data from a file.
    /// @param path The path of the file to load.
    /// @param options The options controlling parsing.
    /// @return The parsed INI data.
    /// @throws std::invalid_argument When @p path is empty.
    /// @throws std::ios_base::failure When the file cannot be read.
    /// @throws std::format_error When the file contents violate the INI parsing rules.
    IniData Load(const std::filesystem::path& path, const IniFileOptions& options = {});

    /// @brief Loads and parses INI data from a stream.
    /// @param in The stream to read from.
    /// @param options The options controlling parsing.
    /// @return The parsed INI data.
    /// @throws std::ios_base::failure When the stream cannot be read.
    /// @throws std::format_error When the stream contents violate the INI parsing rules.
    IniData Load(std::istream& in, const IniFileOptions& options = {});

    /// @brief Formats and saves INI data to a file.
    /// @param path The path of the file to write. The file is created or overwritten.
    /// @param data The INI data to save.
    /// @param options The options controlling formatting.
    /// @throws std::invalid_argument When @p path is empty.
    /// @throws std::ios_base::failure When the file cannot be written.
    void Save(const std::filesystem::path& path, const IniData& data, const IniFileOptions& options = {});

    /// @brief Formats and saves INI data to a stream.
    /// @param out The stream to write to.
    /// @param data The INI data to save.
    /// @param options The options controlling formatting.
    /// @throws std::ios_base::failure When the stream cannot be written.
    void Save(std::ostream& out, const IniData& data, const IniFileOptions& options = {});

} // namespace ini
