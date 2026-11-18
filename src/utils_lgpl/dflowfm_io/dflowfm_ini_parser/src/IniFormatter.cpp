#include "ini/IniFormatter.h"
#include "ini/IniProperty.h"
#include "ini/IniSection.h"

#include <algorithm>
#include <iomanip>
#include <sstream>
#include <vector>

namespace ini
{

    namespace
    {
        void WriteNewLine(std::ostream& stream) { stream << '\n'; }

        void WriteComment(const std::string& comment, std::ostream& stream, const IniScheme& scheme,
                          const IniFormatterOptions& options)
        {
            if (comment.empty()) return;

            stream << scheme.commentDelimiter;

            if (options.propertyCommentPadding > 0)
            {
                stream << std::setw(options.propertyCommentPadding) << "";
            }

            stream << comment;
        }

        void WriteComments(const std::vector<std::string>& comments, std::ostream& stream, const IniScheme& scheme,
                           const IniFormatterOptions& options)
        {
            for (const std::string& comment : comments)
            {
                WriteComment(comment, stream, scheme, options);
                WriteNewLine(stream);
            }
        }

        bool CanWriteProperty(const IniProperty& property, const IniFormatterOptions& options)
        {
            return property.HasValue() || options.writePropertyWithoutValue;
        }

        void WriteProperty(const IniProperty& property, std::ostream& stream, const IniScheme& scheme,
                           const IniFormatterOptions& options)
        {
            if (options.propertyIndentationLevel > 0) stream << std::setw(options.propertyIndentationLevel) << "";

            const std::string& key = property.GetKey();
            if (options.propertyKeyWidth > 0)
            {
                stream << std::left << std::setw(options.propertyKeyWidth) << key;
            }
            else
            {
                stream << key;
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
                // If the key exceeded the configurable key width, reduce the value width to compensate
                const int keyOverflow = static_cast<int>(key.size()) - options.propertyKeyWidth;
                const int adjustedValueWidth = options.propertyValueWidth - std::max(0, keyOverflow);

                if (adjustedValueWidth > 0)
                {
                    stream << std::left << std::setw(adjustedValueWidth) << property.GetValue();
                }
                else
                {
                    stream << property.GetValue();
                }
            }
            else
            {
                stream << property.GetValue();
            }

            if (options.writeComments && property.HasComment())
            {
                if (options.propertyCommentPadding > 0)
                {
                    stream << std::setw(options.propertyCommentPadding) << "";
                }

                WriteComment(property.GetComment(), stream, scheme, options);
            }
        }

        void WriteProperties(const IniSection& section, std::ostream& stream, const IniScheme& scheme,
                             const IniFormatterOptions& options)
        {
            for (const IniProperty& property : section.GetProperties())
            {
                if (CanWriteProperty(property, options))
                {
                    WriteProperty(property, stream, scheme, options);
                    WriteNewLine(stream);
                }
            }
        }

        bool CanWriteSection(const IniSection& section, const IniFormatterOptions& options)
        {
            return !section.GetProperties().empty() || options.writeEmptySections;
        }

        void WriteSection(const IniSection& section, std::ostream& stream, const IniScheme& scheme,
                          const IniFormatterOptions& options)
        {
            const std::vector<std::string>& comments = section.GetComments();

            if (options.writeComments && !comments.empty())
            {
                WriteComments(comments, stream, scheme, options);
            }

            stream << scheme.sectionStartDelimiter << section.GetName() << scheme.sectionEndDelimiter;

            if (!section.GetProperties().empty())
            {
                WriteNewLine(stream);
                WriteProperties(section, stream, scheme, options);
            }
        }

        void WriteSections(const IniData& iniData, std::ostream& stream, const IniScheme& scheme,
                           const IniFormatterOptions& options)
        {
            for (const IniSection& section : iniData.GetSections())
            {
                if (CanWriteSection(section, options))
                {
                    WriteSection(section, stream, scheme, options);
                    WriteNewLine(stream);
                }
            }
        }

    } // namespace

    std::string Format(const IniData& iniData, IniScheme scheme, IniFormatterOptions options)
    {
        std::ostringstream oss;
        WriteSections(iniData, oss, scheme, options);
        return oss.str();
    }

} // namespace ini