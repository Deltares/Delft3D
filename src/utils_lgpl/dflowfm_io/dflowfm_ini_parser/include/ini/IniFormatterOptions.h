#pragma once

namespace ini
{

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

} // namespace ini