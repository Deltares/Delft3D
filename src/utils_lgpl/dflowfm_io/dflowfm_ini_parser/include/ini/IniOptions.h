#pragma once

#include "ini/IniScheme.h"

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

    /// @brief Represents the options for formatting INI data.
    struct IniFormatterOptions
    {
        /// @brief Returns options configured for formatting without any whitespace padding.
        static IniFormatterOptions EmptySpace()
        {
            IniFormatterOptions options;
            options.propertyIndentationLevel = 0;
            options.propertyKeyWidth = 0;
            options.propertyValueWidth = 0;
            options.propertyAssignmentPadding = 0;
            options.propertyCommentPadding = 0;
            return options;
        }

        /// @brief The indentation level (number of spaces) before each property key.
        int propertyIndentationLevel = 0;

        /// @brief The minimum width reserved for the property key column.
        int propertyKeyWidth = 21;

        /// @brief The minimum width reserved for the property value column.
        int propertyValueWidth = 20;

        /// @brief The number of spaces padding around the property assignment delimiter.
        int propertyAssignmentPadding = 1;

        /// @brief The number of spaces between the property value and the comment.
        int propertyCommentPadding = 1;

        /// @brief Indicates whether comments are written during formatting.
        bool writeComments = true;

        /// @brief Indicates whether sections without properties are written.
        bool writeEmptySections = true;

        /// @brief Indicates whether properties without a value are written.
        bool writePropertyWithoutValue = false;
    };

    /// @brief Represents the options for reading and writing INI files.
    ///
    /// @details This struct combines the scheme, parser options and formatter options
    ///          into a single configuration object that can be passed to @ref ini::file_io functions.
    struct IniFileIOOptions
    {
        /// @brief The scheme defining the format of the INI file.
        IniScheme scheme;

        /// @brief The options controlling parsing behavior.
        IniParserOptions parserOptions;

        /// @brief The options controlling formatting behavior.
        IniFormatterOptions formatterOptions;
    };

} // namespace ini