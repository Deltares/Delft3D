#pragma once

#include "ini/IniData.h"
#include "ini/IniOptions.h"

#include <istream>
#include <string_view>

namespace ini
{

    /// @brief Parses INI-formatted text from the specified string to an INI data object.
    /// @param ini The INI-formatted text to parse.
    /// @param scheme The delimiters defining sections, properties and comments.
    /// @param options The options controlling parsing.
    /// @return An IniData object containing the parsed INI data.
    /// @throws std::format_error When the INI text has an invalid format.
    IniData Parse(std::string_view ini, IniScheme scheme = {}, IniParserOptions options = {});

    /// @brief Parses INI-formatted text from the specified stream to an INI data object.
    /// @param stream The stream from which to read the INI-formatted text.
    /// @param scheme The delimiters defining sections, properties and comments.
    /// @param options The options controlling parsing.
    /// @return An IniData object containing the parsed INI data.
    /// @throws std::format_error When the INI text has an invalid format.
    IniData Parse(std::istream& stream, IniScheme scheme = {}, IniParserOptions options = {});

} // namespace ini