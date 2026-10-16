#pragma once

namespace ini
{

    /// @brief Represents the options for parsing INI data.
    struct IniParserOptions
    {
        /// @brief Indicates whether property keys with whitespace are allowed during parsing.
        bool allowPropertyKeysWithSpaces = false;

        /// @brief Indicates whether duplicate sections are allowed during parsing.
        bool allowDuplicateSections = true;

        /// @brief Indicates whether duplicate properties within a section are allowed during parsing.
        bool allowDuplicateProperties = true;

        /// @brief Indicates whether multi-line property values are allowed during parsing.
        bool allowMultiLineValues = false;

        /// @brief Indicates whether property and section comments are parsed.
        bool parseComments = true;
    };

} // namespace ini