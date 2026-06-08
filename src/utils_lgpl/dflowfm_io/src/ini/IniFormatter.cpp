#include "ini/IniFormatter.h"
#include "ini/IniProperty.h"
#include "ini/IniSection.h"

#include <iomanip>
#include <sstream>

namespace ini
{

    std::string IniFormatter::Format(const IniData& iniData) const
    {
        std::ostringstream oss;
        Format(iniData, oss);
        return oss.str();
    }

    void IniFormatter::Format(const IniData& iniData, std::ostream& stream) const { WriteIniData(iniData, stream); }

    void IniFormatter::WriteIniData(const IniData& iniData, std::ostream& stream) const
    {
        WriteSections(iniData, stream);
    }

    void IniFormatter::WriteSections(const IniData& iniData, std::ostream& stream) const
    {
        for (const IniSection& section : iniData)
        {
            WriteSection(section, stream);
            WriteNewLine(stream);
        }
    }

    void IniFormatter::WriteSection(const IniSection& section, std::ostream& stream) const
    {
        const std::vector<std::string>& comments = section.GetComments();

        if (options.writeComments && !comments.empty())
        {
            WriteComments(comments, stream);
            WriteNewLine(stream);
        }

        stream << scheme.sectionStartDelimiter << section.GetName() << scheme.sectionEndDelimiter;

        if (!section.empty())
        {
            WriteNewLine(stream);
            WriteProperties(section, stream);
        }
    }

    void IniFormatter::WriteProperties(const IniSection& section, std::ostream& stream) const
    {
        for (const IniProperty& property : section)
        {
            if (CanWriteProperty(property))
            {
                WriteProperty(property, stream);
                WriteNewLine(stream);
            }
        }
    }

    bool IniFormatter::CanWriteProperty(const IniProperty& property) const
    {
        return property.HasValue() || options.writePropertyWithoutValue;
    }

    void IniFormatter::WriteProperty(const IniProperty& property, std::ostream& stream) const
    {
        if (options.propertyIndentationLevel > 0) stream << std::setw(options.propertyIndentationLevel) << "";

        if (options.propertyKeyWidth > 0)
        {
            stream << std::left << std::setw(options.propertyKeyWidth) << property.GetKey();
        }
        else
        {
            stream << property.GetKey();
        }

        if (options.propertyAssignmentPadding > 0)
        {
            stream << std::setw(options.propertyAssignmentPadding) << "";
        }

        stream << scheme.propertyAssignmentDelimiter;

        if (options.propertyAssignmentPadding > 0)
        {
            stream << std::setw(options.propertyAssignmentPadding) << "";
        }

        if (options.propertyValueWidth > 0)
        {
            stream << std::left << std::setw(options.propertyValueWidth) << property.GetValue();
        }
        else
        {
            stream << property.GetValue();
        }

        if (options.writeComments && property.HasComment())
        {
            WriteComment(property.GetComment(), stream);
        }
    }

    void IniFormatter::WriteComments(const std::vector<std::string>& comments, std::ostream& stream) const
    {
        for (const std::string& comment : comments)
        {
            WriteComment(comment, stream);
            WriteNewLine(stream);
        }
    }

    void IniFormatter::WriteComment(const std::string& comment, std::ostream& stream) const
    {
        stream << scheme.commentDelimiter << ' ' << comment;
    }

    void IniFormatter::WriteNewLine(std::ostream& stream) const { stream << '\n'; }

} // namespace ini