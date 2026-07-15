#include "ini/IniParser.h"
#include "ini/IniData.h"
#include "ini/IniSection.h"
#include "ini/IniProperty.h"
#include "ini/StringUtils.h"

#include <algorithm>
#include <format>
#include <sstream>

namespace ini
{

    IniData IniParser::Parse(const std::string& ini)
    {
        std::istringstream stream(ini);
        return Parse(stream);
    }

    IniData IniParser::Parse(std::istream& stream)
    {
        InitializeParsingContext();
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

    void IniParser::InitializeParsingContext()
    {
        iniData = IniData{};
        values.clear();
        blockComments.clear();
        inlineComments.clear();
        foundSections.clear();
        foundProperties.clear();
        currentSection = nullptr;
        currentProperty = nullptr;
        currentLine.clear();
        lineNumber = 0;
    }

    void IniParser::SetInvalidChars()
    {
        invalidChars = {'*', ':', ';', ']', '[', '\\', '?', '!', '@', '#', '$',
                        '%', '^', '&', '(', ')', '<',  '>', '|', '~', '`'};

        invalidChars.erase(scheme.commentDelimiter);
        invalidChars.erase(scheme.sectionStartDelimiter);
    }

    void IniParser::CleanCurrentLine()
    {
        if (lineNumber == 1)
        {
            StripByteOrderMark();
        }

        currentLine.erase(std::remove(currentLine.begin(), currentLine.end(), '\0'), currentLine.end());
        std::replace(currentLine.begin(), currentLine.end(), '\t', ' ');
        currentLine = trim(currentLine);
    }

    void IniParser::StripByteOrderMark()
    {
        constexpr std::string_view utf8BOM = "\xEF\xBB\xBF";
        if (currentLine.starts_with(utf8BOM))
        {
            currentLine.erase(0, utf8BOM.size());
        }
    }

    void IniParser::ParseCurrentLine()
    {
        if (IsEmptyLine())
        {
            return;
        }

        if (!IsValidLine())
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
        else if (IsMultiLineValueLine())
        {
            ParseMultiLineValueLine();
        }
        else
        {
            HandleInvalidLineFormat();
        }
    }

    bool IniParser::IsEmptyLine() const { return currentLine.empty(); }

    bool IniParser::IsValidLine() const
    {
        return currentLine.empty() || invalidChars.find(currentLine[0]) == invalidChars.end();
    }

    bool IniParser::IsCommentLine() const { return !currentLine.empty() && currentLine[0] == scheme.commentDelimiter; }

    bool IniParser::IsSectionLine() const
    {
        return !currentLine.empty() && currentLine[0] == scheme.sectionStartDelimiter &&
               currentLine.find(scheme.sectionEndDelimiter) != std::string::npos;
    }

    bool IniParser::IsPropertyLine() const
    {
        return currentLine.find(scheme.propertyAssignmentDelimiter) != std::string::npos;
    }

    bool IniParser::IsMultiLineValueLine() const
    {
        return currentProperty != nullptr && currentLine.find(scheme.propertyAssignmentDelimiter) == std::string::npos;
    }

    void IniParser::HandleInvalidLineFormat() const
    {
        throw std::format_error(std::format("Error on line {}: invalid INI-formatted text.", lineNumber));
    }

    void IniParser::ParseCommentLine()
    {
        if (!options.parseComments)
        {
            return;
        }

        const std::size_t commentIndex = currentLine.find(scheme.commentDelimiter);
        const std::string comment = trim(currentLine.substr(commentIndex + 1));

        blockComments.push_back(comment);
    }

    void IniParser::ParseSectionLine()
    {
        const std::size_t startIndex = currentLine.find(scheme.sectionStartDelimiter);
        const std::size_t endIndex = currentLine.rfind(scheme.sectionEndDelimiter);
        const std::string sectionName = trim(currentLine.substr(startIndex + 1, endIndex - startIndex - 1));

        ValidateSectionName(sectionName);
        AddNewSection(sectionName);
    }

    void IniParser::ValidateSectionName(const std::string& name)
    {
        if (name.empty())
        {
            throw std::format_error(std::format("Error on line {}: section name cannot be empty.", lineNumber));
        }

        if (!options.allowDuplicateSections && !foundSections.insert(name).second)
        {
            throw std::format_error(
                std::format("Error on line {}: duplicate section with name '{}'.", lineNumber, name));
        }

        if (!options.allowDuplicateProperties)
        {
            foundProperties.clear();
        }
    }

    void IniParser::AddNewSection(const std::string& name)
    {
        IniSection& section = iniData.AddSection(name);

        section.SetLineNumber(lineNumber);
        section.AddComments(blockComments);

        currentSection = &section;

        blockComments.clear();
    }

    void IniParser::ParsePropertyLine()
    {
        if (currentSection == nullptr)
        {
            throw std::format_error(
                std::format("Error on line {}: properties must be defined within a section.", lineNumber));
        }

        const std::size_t assignmentIndex = currentLine.find(scheme.propertyAssignmentDelimiter);
        const std::string key = trim(currentLine.substr(0, assignmentIndex));
        ValidatePropertyKey(key);

        const std::size_t valueStartIndex = assignmentIndex + 1;
        const std::size_t commentIndex = currentLine.find(scheme.commentDelimiter, valueStartIndex);

        std::string value = commentIndex != std::string::npos
                                ? trim(currentLine.substr(valueStartIndex, commentIndex - valueStartIndex))
                                : trim(currentLine.substr(valueStartIndex));
        ValidatePropertyValue(value);
        value = CleanupMultiLineValue(std::move(value));

        const std::string comment = (commentIndex != std::string::npos && options.parseComments)
                                        ? trim(currentLine.substr(commentIndex + 1))
                                        : std::string{};

        AddNewProperty(key, value, comment);
    }

    void IniParser::ValidatePropertyKey(const std::string& key)
    {
        if (key.empty())
        {
            throw std::format_error(std::format("Error on line {}: property key cannot be empty.", lineNumber));
        }

        if (!options.allowPropertyKeysWithSpaces && key.find(' ') != std::string::npos)
        {
            throw std::format_error(std::format("Error on line {}: property key cannot contain spaces.", lineNumber));
        }

        if (!options.allowDuplicateProperties && !foundProperties.insert(key).second)
        {
            throw std::format_error(
                std::format("Error on line {}: duplicate property with key '{}'.", lineNumber, key));
        }
    }

    void IniParser::ValidatePropertyValue(const std::string& value)
    {
        if (!options.allowMultiLineValues && !value.empty() && value.back() == scheme.multiLineValueDelimiter)
        {
            throw std::format_error(std::format("Error on line {}: multi-line values are not allowed.", lineNumber));
        }
    }

    void IniParser::AddNewProperty(const std::string& key, const std::string& value, const std::string& comment)
    {
        IniProperty& property = currentSection->AddProperty(IniProperty{key});
        property.SetLineNumber(lineNumber);
        currentProperty = &property;

        values.push_back(value);
        inlineComments.push_back(comment);
        blockComments.clear();
    }

    void IniParser::ParseMultiLineValueLine()
    {
        const std::size_t commentIndex = currentLine.find(scheme.commentDelimiter);

        std::string value =
            commentIndex != std::string::npos ? trim(currentLine.substr(0, commentIndex)) : trim(currentLine);
        value = CleanupMultiLineValue(std::move(value));

        const std::string comment = (commentIndex != std::string::npos && options.parseComments)
                                        ? trim(currentLine.substr(commentIndex + 1))
                                        : std::string{};

        AddValueAndComment(value, comment);
    }

    void IniParser::AddValueAndComment(const std::string& value, const std::string& comment)
    {
        values.push_back(value);
        inlineComments.push_back(comment);
        blockComments.clear();
    }

    void IniParser::FinalizeCurrentProperty()
    {
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

    std::string IniParser::CleanupMultiLineValue(std::string value) const
    {
        value = trim_end(std::move(value), scheme.multiLineValueDelimiter);
        value = trim_end(std::move(value), ' ');
        return value;
    }

} // namespace ini