#include <dflowfm_io/MduValidator.h>
#include <dflowfm_io/MduSchema.h>
#include <dflowfm_io/IssueReport.h>

#include <ini/IniData.h>
#include <ini/IniSection.h>
#include <ini/IniProperty.h>

namespace dflowfm_io
{
    IssueReport MduValidator::Validate(const ini::IniData& iniData, const MduSchema& schema)
    {
        IssueReport report;
        ValidateRequired(iniData, schema, report);
        ValidateUnsupported(iniData, schema, report);
        ValidateDeprecated(iniData, schema, report);
        return report;
    }

    void MduValidator::ValidateRequired(const ini::IniData& iniData, const MduSchema& schema, IssueReport& report)
    {
        for (const auto& sectionSchema : schema.Sections())
        {
            if (!iniData.HasSection(sectionSchema.name))
            {
                if (sectionSchema.required) report.AddError("Required section [{}] is missing.", sectionSchema.name);

                for (const auto& propertySchema : sectionSchema.properties)
                    if (propertySchema.required)
                        report.AddError("Required property [{}].{} is missing.", sectionSchema.name, propertySchema.key);

                continue;
            }

            const ini::IniSection& section = iniData.GetSection(sectionSchema.name);
            for (const auto& propertySchema : sectionSchema.properties)
            {
                if (!section.HasProperty(propertySchema.key))
                {
                    if (propertySchema.required)
                        report.AddError("Required property [{}].{} is missing.", sectionSchema.name, propertySchema.key);
                    else if (!propertySchema.default_value.empty())
                        report.AddInfo("Property [{}].{} is not provided. Default is used: \"{}\".", sectionSchema.name,
                                       propertySchema.key, propertySchema.default_value);
                    continue;
                }

                const ini::IniProperty& property = section.GetProperty(propertySchema.key);
                if (!property.HasValue())
                {
                    if (propertySchema.required)
                        report.AddError(property.GetLineNumber(), "Required property [{}].{} is specified without a value.",
                                        sectionSchema.name, propertySchema.key);
                    else if (!propertySchema.default_value.empty())
                        report.AddInfo(property.GetLineNumber(), "Property [{}].{} is specified without a value. Default is used: \"{}\".",
                                       sectionSchema.name, propertySchema.key, propertySchema.default_value);
                }
            }
        }
    }

    void MduValidator::ValidateUnsupported(const ini::IniData& iniData, const MduSchema& schema, IssueReport& report)
    {
        for (const auto& section : iniData)
        {
            const auto* sectionSchema = schema.FindSection(section.GetName());
            if (!sectionSchema)
            {
                report.AddWarning(section.GetLineNumber(), "Section [{}] is not a supported section.",
                                  section.GetName());
                continue;
            }

            for (const auto& property : section)
            {
                const auto* propertySchema = schema.FindProperty(section.GetName(), property.GetKey());
                if (!propertySchema)
                    report.AddWarning(property.GetLineNumber(), "Property [{}].{} is not a supported property.",
                                      section.GetName(), property.GetKey());
            }
        }
    }

    void MduValidator::ValidateDeprecated(const ini::IniData& iniData, const MduSchema& schema, IssueReport& report)
    {
        for (const auto& section : iniData)
        {
            for (const auto& property : section)
            {
                const auto* propertySchema = schema.FindProperty(section.GetName(), property.GetKey());
                if (!propertySchema)
                    continue;

                if (propertySchema->status.type == StatusType::Deprecated)
                {
                    report.AddWarning(property.GetLineNumber(), "Property [{}].{} is deprecated. {}",
                                      section.GetName(), property.GetKey(), propertySchema->status.comment);
                    continue;
                }

                if (propertySchema->value_type != ValueType::Enum && propertySchema->value_type != ValueType::IntEnum)
                    continue;

                if (!property.HasValue())
                    continue;

                for (const auto& enumValueSchema : propertySchema->enum_values)
                {
                    if (enumValueSchema.status.type != StatusType::Deprecated)
                        continue;

                    const std::string deprecatedValue = propertySchema->value_type == ValueType::IntEnum
                                                            ? std::to_string(enumValueSchema.value)
                                                            : enumValueSchema.label;

                    if (property.GetValue() == deprecatedValue)
                    {
                        report.AddWarning(property.GetLineNumber(), "Property [{}].{}={} is deprecated. {}",
                                          section.GetName(), property.GetKey(), deprecatedValue, enumValueSchema.status.comment);
                        break;
                    }
                }
            }
        }
    }

} // namespace dflowfm_io