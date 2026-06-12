#include <dflowfm_io/MduValidator.h>
#include <dflowfm_io/MduSchema.h>
#include <dflowfm_io/MduSchemaData.h>
#include <dflowfm_io/IssueReport.h>
#include <ini/IniData.h>
#include <ini/IniSection.h>
#include <ini/IniProperty.h>

using namespace ini;

namespace dflowfm_io
{

    MduValidator::MduValidator() {}

    IssueReport MduValidator::Validate(const IniData& iniData) const
    {
        IssueReport report;
        ValidateRequired(iniData, report);
        ValidateUnsupported(iniData, report);
        return report;
    }

    void MduValidator::ValidateRequired(const IniData& iniData, IssueReport& report) const
    {
        for (const auto& sectionSchema : MDU_SCHEMA.sections)
        {
            if (!iniData.HasSection(sectionSchema.name))
            {
                if (sectionSchema.required) report.AddError("Section [{}] is missing.", sectionSchema.name);

                for (const auto& propertySchema : sectionSchema.properties)
                    if (propertySchema.required)
                        report.AddError("Property [{}].{} is missing.", sectionSchema.name, propertySchema.key);

                continue;
            }

            const IniSection& section = iniData.GetSection(sectionSchema.name);
            for (const auto& propertySchema : sectionSchema.properties)
            {
                if (!section.HasProperty(propertySchema.key))
                {
                    if (propertySchema.required)
                        report.AddError("Required property [{}].{} is missing.", sectionSchema.name, propertySchema.key);
                    else if (propertySchema.HasDefault())
                        report.AddInfo("Property [{}].{} is not provided. Default is used: \"{}\".", sectionSchema.name,
                                       propertySchema.key, propertySchema.default_value);
                    continue;
                }

                const IniProperty& property = section.GetProperty(propertySchema.key);
                if (!property.HasValue())
                {
                    if (propertySchema.required)
                        report.AddError(property.GetLineNumber(), "Required property [{}].{} is specified without a value.", sectionSchema.name,
                                        propertySchema.key);
                    else if (propertySchema.HasDefault())
                        report.AddInfo(property.GetLineNumber(), "Property [{}].{} is specified without a value. Default is used: \"{}\".",
                                       sectionSchema.name, propertySchema.key, propertySchema.default_value);
                }
            }
        }
    }

    void MduValidator::ValidateUnsupported(const IniData& iniData, IssueReport& report) const
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