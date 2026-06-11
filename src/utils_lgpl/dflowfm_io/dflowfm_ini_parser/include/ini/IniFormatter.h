#pragma once

#include "ini/IniData.h"
#include "ini/IniFormatterOptions.h"
#include "ini/IniScheme.h"

#include <ostream>
#include <string>

namespace ini
{

    /// @brief Formats INI data to an INI-formatted string.
    ///
    /// @details The formatting behavior can be customized through @ref GetOptions(),
    ///          which specifies formatting options like the property key/value width,
    ///          indentation and whether properties without a value should be written.
    ///
    ///          The INI file format can be customized through @ref GetScheme(),
    ///          which specifies the characters that define sections, properties and comments.
    class IniFormatter
    {
    public:
        /// @brief Gets the scheme that defines the format of the INI file.
        IniScheme& GetScheme() { return scheme; }

        /// @brief Sets the scheme that defines the format of the INI file.
        /// @param value The new INI scheme.
        void SetScheme(IniScheme value) { this->scheme = std::move(value); }

        /// @brief Gets the options that control the INI formatting behavior.
        IniFormatterOptions& GetOptions() { return options; }

        /// @brief Sets the options that control the INI formatting behavior.
        /// @param value The new formatting options.
        void SetOptions(IniFormatterOptions options) { this->options = std::move(options); }

        /// @brief Formats the specified INI data to an INI-formatted string.
        /// @param iniData The @ref IniData to format.
        /// @return The formatted INI string.
        std::string Format(const IniData& iniData) const;

        /// @brief Formats the specified INI data and writes it to the specified stream.
        /// @param iniData The @ref IniData to format.
        /// @param stream The stream to write the formatted INI data to.
        void Format(const IniData& iniData, std::ostream& stream) const;

    private:
        IniScheme scheme;
        IniFormatterOptions options;

        void WriteIniData(const IniData& iniData, std::ostream& stream) const;
        void WriteSections(const IniData& iniData, std::ostream& stream) const;
        void WriteSection(const IniSection& section, std::ostream& stream) const;
        void WriteProperties(const IniSection& section, std::ostream& stream) const;
        void WriteProperty(const IniProperty& property, std::ostream& stream) const;
        void WriteComments(const std::vector<std::string>& comments, std::ostream& stream) const;
        void WriteComment(const std::string& comment, std::ostream& stream) const;
        void WriteNewLine(std::ostream& stream) const;
        bool CanWriteSection(const IniSection& section) const;
        bool CanWriteProperty(const IniProperty& property) const;
    };

} // namespace ini