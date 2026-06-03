#pragma once

namespace ini
{

    /// @brief Defines the format of an INI file through customization of the characters
    ///        that define sections, property value assignment and comments.
    ///
    /// @details By default, the various delimiters for the INI file are set to:
    ///
    /// - `#` for one-line comments and inline comments
    /// - `#` for delimiting a special value
    /// - `[` `]` for delimiting a section
    /// - `=` for property key / value pairs
    /// - `\` for multi-line property values
    ///
    /// @par Example
    /// An example of well-formed data with the default values:
    /// @code{.ini}
    /// # section comment line 1
    /// # section comment line 2
    /// [section]
    /// key1 = value1 # inline property comment
    /// key2 = value2 \
    /// value3 \
    /// value4 # inline property comment
    /// key3 = #value5# # inline property comment
    /// @endcode
    struct IniScheme
    {
        /// @brief The delimiter used to indicate comments in INI data.
        char commentDelimiter = '#';

        /// @brief The delimiter used to indicate the start of sections in INI data.
        char sectionStartDelimiter = '[';

        /// @brief The delimiter used to indicate the end of sections in INI data.
        char sectionEndDelimiter = ']';

        /// @brief The delimiter used to separate property keys and values in INI data.
        char propertyAssignmentDelimiter = '=';

        /// @brief The delimiter used to indicate continuation of multi-line property values.
        char multiLineValueDelimiter = '\\';

        /// @brief The character used to quote values in INI data.
        char valueQuoteDelimiter = '#';
    };

} // namespace ini