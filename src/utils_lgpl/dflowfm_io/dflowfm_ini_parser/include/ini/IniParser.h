#pragma once

#include "ini/IniData.h"
#include "ini/IniParserOptions.h"
#include "ini/IniProperty.h"
#include "ini/IniScheme.h"
#include "ini/IniSection.h"

#include <istream>
#include <optional>
#include <string>
#include <unordered_set>
#include <vector>

namespace ini
{

    /// @brief Parses INI-formatted text to an INI data object.
    ///
    /// @details The parsing behavior can be customized through constructor options,
    /// which specifies parsing options like whether duplicate section names,
    /// duplicate property keys and multi-line values are allowed.
    ///
    /// The INI file format can be customized through the constructor scheme,
    /// which specifies the characters that define sections, properties and comments.
    class IniParser
    {
    public:
        /// @brief Creates a parser with the specified scheme and parsing options.
        /// @param scheme The INI scheme to use.
        /// @param options The parsing options to use.
        explicit IniParser(IniScheme scheme = {}, IniParserOptions options = {});

        /// @brief Parses INI-formatted text from the specified string to an INI data object.
        /// @param ini The INI-formatted text to parse.
        /// @return An IniData object containing the parsed INI data.
        /// @throws std::format_error When the INI text has an invalid format.
        IniData Parse(const std::string& ini);

        /// @brief Parses INI-formatted text from the specified stream to an INI data object.
        /// @param stream The stream from which to read the INI-formatted text.
        /// @return An IniData object containing the parsed INI data.
        /// @throws std::format_error When the INI text has an invalid format.
        IniData Parse(std::istream& stream);

    private:
        IniScheme scheme;
        IniParserOptions options;

        IniData iniData;
        IniSection* currentSection;
        IniProperty* currentProperty;

        std::vector<std::string> values;
        std::vector<std::string> blockComments;
        std::vector<std::string> inlineComments;

        std::unordered_set<std::string> foundSections;
        std::unordered_set<std::string> foundProperties;
        std::unordered_set<char> invalidChars;

        std::string currentLine;
        int lineNumber{0};
        bool multiLineContinuation{false};

        void InitializeParsingContext();
        void SetInvalidChars();

        void CleanCurrentLine();
        void StripByteOrderMark();
        void ParseCurrentLine();

        bool IsEmptyLine() const;
        bool IsValidLine() const;
        bool IsCommentLine() const;
        bool IsSectionLine() const;
        bool IsPropertyLine() const;
        bool IsMultiLineContinuation() const;

        void HandleInvalidLineFormat() const;

        void ParseCommentLine();
        void ParseSectionLine();
        void ParsePropertyLine();
        void ParseMultiLineValueLine();

        void ValidateSectionName(const std::string& name);
        void ValidatePropertyKey(const std::string& key);
        void ValidatePropertyValue(const std::string& value);

        void AddNewSection(const std::string& name);
        void AddNewProperty(const std::string& key, const std::string& value, const std::string& comment = {});
        void AddValueAndComment(const std::string& value, const std::string& comment);
        void FinalizeCurrentProperty();

        bool HasMultiLineContinuation(const std::string& value) const;
        std::string CleanupMultiLineValue(std::string value) const;
    };

} // namespace ini