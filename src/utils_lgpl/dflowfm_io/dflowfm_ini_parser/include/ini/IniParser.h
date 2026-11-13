#pragma once

#include "ini/IniData.h"
#include "ini/IniOptions.h"
#include "ini/IniScheme.h"

#include <string>

namespace ini
{

    /// @brief Parses INI-formatted text from the specified string to an INI data object.
    /// @param ini The INI-formatted text to parse.
    /// @param scheme The delimiters defining sections, properties and comments.
    /// @param options Controls duplicate sections, duplicate properties, multi-line values and comments.
    /// @return An IniData object containing the parsed INI data.
    /// @throws std::format_error When the INI text has an invalid format.
    IniData Parse(const std::string& ini, IniScheme scheme = {}, IniParserOptions options = {});

} // namespace ini