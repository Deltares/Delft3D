#include <dflowfm_io/MduValidator.h>
#include <dflowfm_io/MduSchema.h>
#include <dflowfm_io/IssueReport.h>

#include <ini/IniData.h>
#include <ini/IniSection.h>
#include <ini/IniProperty.h>

namespace dflowfm_io
{

    IssueReport MduValidator::Validate(const ini::IniData& iniData)
    {
        IssueReport report;
        ValidateRequired(iniData, report);
        ValidateUnsupported(iniData, report);
        return report;
    }

    void MduValidator::ValidateRequired(const ini::IniData& iniData, IssueReport& report)
    {
        for (const auto& sectionSchema : MDU_SCHEMA.sections)
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

    void MduValidator::ValidateUnsupported(const ini::IniData& iniData, IssueReport& report)
    {
        for (const auto& section : iniData)
        {
            const auto* sectionSchema = MDU_SCHEMA.FindSection(section.GetName());
            if (!sectionSchema)
            {
                report.AddWarning(section.GetLineNumber(), "Section [{}] is not a supported section.",
                                  section.GetName());
                continue;
            }

            for (const auto& property : section)
            {
                const auto* propertySchema = sectionSchema->FindProperty(property.GetKey());
                if (!propertySchema)
                    report.AddWarning(property.GetLineNumber(), "Property [{}].{} is not a supported property.",
                                      section.GetName(), property.GetKey());
            }
        }
    }

} // namespace dflowfm_io