#pragma once

#include "ini/IniFormatterOptions.h"
#include "ini/IniParserOptions.h"
#include "ini/IniScheme.h"

namespace ini
{

    /// @brief Represents the options for reading and writing INI files.
    ///
    /// @details This struct combines the scheme, parser options and formatter options
    ///          into a single configuration object that can be passed to @ref IniFile.
    struct IniFileOptions
    {
        /// @brief The scheme defining the format of the INI file.
        IniScheme scheme;

        /// @brief The options controlling parsing behavior.
        IniParserOptions parserOptions;

        /// @brief The options controlling formatting behavior.
        IniFormatterOptions formatterOptions;
    };

} // namespace ini