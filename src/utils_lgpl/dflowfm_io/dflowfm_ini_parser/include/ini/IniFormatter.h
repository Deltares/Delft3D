#pragma once

#include "ini/IniData.h"
#include "ini/IniOptions.h"
#include "ini/IniScheme.h"

#include <string>

namespace ini
{

    /// @brief Formats the specified INI data to an INI-formatted string.
    /// @param iniData The @ref IniData to format.
    /// @param scheme The delimiters defining sections, properties and comments.
    /// @param options Controls column widths, padding, indentation and which sections, properties and comments are
    /// written.
    /// @return The formatted INI string.
    std::string Format(const IniData& iniData, IniScheme scheme = {}, IniFormatterOptions options = {});

} // namespace ini