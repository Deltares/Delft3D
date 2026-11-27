#include "ini/IniParser.h"
#include "ini/IniData.h"
#include "ini/IniSection.h"
#include "ini/IniProperty.h"
#include "StringUtils.h"

#include <algorithm>
#include <format>
#include <sstream>
#include <string_view>
#include <unordered_set>
#include <utility>
#include <vector>

namespace ini
{
    namespace
    {
        class ParsingContext
        {
        public:
            ParsingContext(const IniScheme& scheme, const IniParserOptions& options) : scheme(scheme), options(options)
            {
            }

            IniData Parse(std::istream& stream);

        private:
            const IniScheme& scheme;
            const IniParserOptions& options;

            IniData iniData;
            IniSection* currentSection{nullptr};
            IniProperty* currentProperty{nullptr};

            std::vector<std::string> values;
            std::vector<std::string> blockComments;
            std::vector<std::string> inlineComments;

            std::unordered_set<std::string> foundSections;
            std::unordered_set<std::string> foundProperties;
            std::unordered_set<char> invalidStartChars;

            std::string currentLine;
            int lineNumber{0};
            bool multiLineContinuation{false};

            void SetInvalidChars();
            void CleanCurrentLine();
            void StripByteOrderMark();
            void ParseCurrentLine();

            bool IsEmptyLine() const;
            bool IsValidLineStart() const;
            bool IsCommentLine() const;
            bool IsSectionLine() const;
            bool IsPropertyLine() const;
            bool IsMultiLineContinuation() const;

            void HandleInvalidLineFormat() const;

            void ParseCommentLine();
            void ParseSectionLine();
            void ParsePropertyLine();
            void ParseMultiLineValueLine();

            void ValidateSectionName(std::string_view name);
            void ValidatePropertyKey(std::string_view key);
            void ValidatePropertyValue(std::string_view value);

            void AddNewSection(std::string name);
            void AddNewProperty(std::string key, std::string value, std::string comment = {});
            void AddValueAndComment(std::string value, std::string comment);
            void FinalizeCurrentProperty();

            bool HasMultiLineContinuation(std::string_view value) const;
            std::string CleanupMultiLineValue(std::string value) const;
        };

        IniData ParsingContext::Parse(std::istream& stream)
        {
            SetInvalidChars();

            while (std::getline(stream, currentLine))
            {
                lineNumber++;
                CleanCurrentLine();
                ParseCurrentLine();
            }

            FinalizeCurrentProperty();
            return iniData;
        }

        void ParsingContext::SetInvalidChars()
        {
            invalidStartChars = {'*', ':', ';', ']', '[', '\\', '?', '!', '@', '#', '$',
                                 '%', '^', '&', '(', ')', '<',  '>', '|', '~', '`'};

            invalidStartChars.erase(scheme.commentDelimiter);
            invalidStartChars.erase(scheme.sectionStartDelimiter);
        }

        void ParsingContext::CleanCurrentLine()
        {
            if (lineNumber == 1)
            {
                StripByteOrderMark();
            }

            currentLine.erase(std::remove(currentLine.begin(), currentLine.end(), '\0'), currentLine.end());
            std::replace(currentLine.begin(), currentLine.end(), '\t', ' ');
            currentLine = trim(currentLine);
        }

        void ParsingContext::StripByteOrderMark()
        {
            constexpr std::string_view utf8BOM = "\xEF\xBB\xBF";
            if (currentLine.starts_with(utf8BOM))
            {
                currentLine.erase(0, utf8BOM.size());
            }
        }

        void ParsingContext::ParseCurrentLine()
        {
            if (IsEmptyLine())
            {
                return;
            }

            if (!IsValidLineStart())
            {
                HandleInvalidLineFormat();
            }

            if (IsCommentLine())
            {
                ParseCommentLine();
            }
            else if (IsSectionLine())
            {
                FinalizeCurrentProperty();
                ParseSectionLine();
            }
            else if (IsPropertyLine())
            {
                FinalizeCurrentProperty();
                ParsePropertyLine();
            }
            else if (IsMultiLineContinuation())
            {
                ParseMultiLineValueLine();
            }
            else
            {
                HandleInvalidLineFormat();
            }
        }

        bool ParsingContext::IsEmptyLine() const
        {
            return currentLine.empty();
        }

        bool ParsingContext::IsValidLineStart() const
        {
            return invalidStartChars.find(currentLine[0]) == invalidStartChars.end();
        }

        bool ParsingContext::IsCommentLine() const
        {
            return currentLine[0] == scheme.commentDelimiter;
        }

        bool ParsingContext::IsSectionLine() const
        {
            return currentLine[0] == scheme.sectionStartDelimiter;
        }

        bool ParsingContext::IsPropertyLine() const
        {
            const std::size_t assignmentIndex = currentLine.find(scheme.propertyAssignmentDelimiter);
            const std::size_t commentIndex = currentLine.find(scheme.commentDelimiter);
            
            return assignmentIndex < commentIndex;
        }

        bool ParsingContext::IsMultiLineContinuation() const
        {
            return multiLineContinuation;
        }

        void ParsingContext::HandleInvalidLineFormat() const
        {
            throw std::format_error(std::format("Error on line {}: invalid INI-formatted text.", lineNumber));
        }

        void ParsingContext::ParseCommentLine()
        {
            if (!options.parseComments)
            {
                return;
            }

            const std::size_t commentIndex = currentLine.find(scheme.commentDelimiter);
            std::string comment = trim(currentLine.substr(commentIndex + 1));

            blockComments.push_back(std::move(comment));
        }

        void ParsingContext::ParseSectionLine()
        {
            const std::size_t commentIndex = currentLine.find(scheme.commentDelimiter);
            const std::string sectionPart =
                commentIndex != std::string::npos ? trim(currentLine.substr(0, commentIndex)) : currentLine;

            const std::size_t endIndex = sectionPart.rfind(scheme.sectionEndDelimiter);

            if (endIndex == std::string::npos ||
                sectionPart.back() != scheme.sectionEndDelimiter ||
                sectionPart.find(scheme.sectionStartDelimiter, 1) != std::string::npos ||
                sectionPart.find(scheme.sectionEndDelimiter) != endIndex)
            {
                HandleInvalidLineFormat();
            }

            std::string sectionName = trim(sectionPart.substr(1, endIndex - 1));

            ValidateSectionName(sectionName);
            AddNewSection(std::move(sectionName));
        }

        void ParsingContext::ValidateSectionName(std::string_view name)
        {
            if (name.empty())
            {
                throw std::format_error(std::format("Error on line {}: section name cannot be empty.", lineNumber));
            }

            if (!options.allowDuplicateSections && !foundSections.insert(tolower(name)).second)
            {
                throw std::format_error(
                    std::format("Error on line {}: duplicate section with name '{}'.", lineNumber, name));
            }

            if (!options.allowDuplicateProperties)
            {
                foundProperties.clear();
            }
        }

        void ParsingContext::AddNewSection(std::string name)
        {
            IniSection& section = iniData.AddSection(std::move(name));

            section.SetLineNumber(lineNumber);
            section.AddComments(blockComments);

            currentSection = &section;

            blockComments.clear();
        }

        void ParsingContext::ParsePropertyLine()
        {
            if (currentSection == nullptr)
            {
                throw std::format_error(
                    std::format("Error on line {}: properties must be defined within a section.", lineNumber));
            }

            const std::size_t assignmentIndex = currentLine.find(scheme.propertyAssignmentDelimiter);
            std::string key = trim(currentLine.substr(0, assignmentIndex));
            ValidatePropertyKey(key);

            const std::size_t valueStartIndex = assignmentIndex + 1;
            const std::size_t commentIndex = currentLine.find(scheme.commentDelimiter, valueStartIndex);

            std::string value = commentIndex != std::string::npos
                                    ? trim(currentLine.substr(valueStartIndex, commentIndex - valueStartIndex))
                                    : trim(currentLine.substr(valueStartIndex));
            ValidatePropertyValue(value);
            multiLineContinuation = HasMultiLineContinuation(value);
            value = CleanupMultiLineValue(std::move(value));

            std::string comment = (commentIndex != std::string::npos && options.parseComments)
                                      ? trim(currentLine.substr(commentIndex + 1))
                                      : std::string{};

            AddNewProperty(std::move(key), std::move(value), std::move(comment));
        }

        void ParsingContext::ValidatePropertyKey(std::string_view key)
        {
            if (key.empty())
            {
                throw std::format_error(std::format("Error on line {}: property key cannot be empty.", lineNumber));
            }

            if (!options.allowPropertyKeysWithSpaces && key.find(' ') != std::string_view::npos)
            {
                throw std::format_error(
                    std::format("Error on line {}: property key cannot contain spaces.", lineNumber));
            }

            if (!options.allowDuplicateProperties && !foundProperties.insert(tolower(key)).second)
            {
                throw std::format_error(
                    std::format("Error on line {}: duplicate property with key '{}'.", lineNumber, key));
            }
        }

        void ParsingContext::ValidatePropertyValue(std::string_view value)
        {
            if (!options.allowMultiLineValues && scheme.multiLineValueDelimiter.has_value() && !value.empty() &&
                value.back() == *scheme.multiLineValueDelimiter)
            {
                throw std::format_error(
                    std::format("Error on line {}: multi-line values are not allowed.", lineNumber));
            }
        }

        void ParsingContext::AddNewProperty(std::string key, std::string value, std::string comment)
        {
            IniProperty& property = currentSection->AddProperty(IniProperty{std::move(key)});
            property.SetLineNumber(lineNumber);
            currentProperty = &property;

            values.push_back(std::move(value));
            inlineComments.push_back(std::move(comment));
            blockComments.clear();
        }

        void ParsingContext::ParseMultiLineValueLine()
        {
            const std::size_t commentIndex = currentLine.find(scheme.commentDelimiter);

            std::string value =
                commentIndex != std::string::npos ? trim(currentLine.substr(0, commentIndex)) : trim(currentLine);
            multiLineContinuation = HasMultiLineContinuation(value);
            value = CleanupMultiLineValue(std::move(value));

            std::string comment = (commentIndex != std::string::npos && options.parseComments)
                                      ? trim(currentLine.substr(commentIndex + 1))
                                      : std::string{};

            AddValueAndComment(std::move(value), std::move(comment));
        }

        void ParsingContext::AddValueAndComment(std::string value, std::string comment)
        {
            values.push_back(std::move(value));
            inlineComments.push_back(std::move(comment));
            blockComments.clear();
        }

        void ParsingContext::FinalizeCurrentProperty()
        {
            multiLineContinuation = false;

            if (currentProperty == nullptr || values.empty())
            {
                return;
            }

            std::ostringstream valueStream;
            for (std::size_t i = 0; i < values.size(); ++i)
            {
                if (i > 0)
                {
                    valueStream << '\n';
                }
                valueStream << values[i];
            }
            currentProperty->SetValue(trim(valueStream.str()));

            std::ostringstream commentStream;
            for (std::size_t i = 0; i < inlineComments.size(); ++i)
            {
                if (i > 0)
                {
                    commentStream << '\n';
                }
                commentStream << inlineComments[i];
            }
            currentProperty->SetComment(trim(commentStream.str()));

            values.clear();
            inlineComments.clear();
            currentProperty = nullptr;
        }

        bool ParsingContext::HasMultiLineContinuation(std::string_view value) const
        {
            return options.allowMultiLineValues &&
                   (!scheme.multiLineValueDelimiter.has_value() ||
                    (!value.empty() && value.back() == *scheme.multiLineValueDelimiter));
        }

        std::string ParsingContext::CleanupMultiLineValue(std::string value) const
        {
            if (scheme.multiLineValueDelimiter.has_value())
                value = trim_end(std::move(value), *scheme.multiLineValueDelimiter);
            value = trim_end(std::move(value), ' ');
            return value;
        }

    } // namespace

    IniData Parse(std::string_view ini, IniScheme scheme, IniParserOptions options)
    {
        std::istringstream stream{std::string(ini)};
        return Parse(stream, std::move(scheme), std::move(options));
    }

    IniData Parse(std::istream& stream, IniScheme scheme, IniParserOptions options)
    {
        return ParsingContext(scheme, options).Parse(stream);
    }

} // namespace ini