#include <dflowfm_io/ConversionResult.h>
#include <dflowfm_io/MduConverter.h>

using namespace ini;

namespace dflowfm_io
{
    MduConverter::MduConverter() : mduSchema(BuildMduSchema()) {}

    ConversionResult<MduModel> MduConverter::ToModel(const IniData& iniData)
    {
        ValidateStructure(iniData);
        ConvertValues(iniData);

        return {std::move(mduModel), std::move(report)};
    }

    void MduConverter::ValidateStructure(const IniData& iniData)
    {
        ValidateSchemaAgainstData(iniData);
        ValidateDataAgainstSchema(iniData);
    }

    void MduConverter::ValidateSchemaAgainstData(const IniData& iniData)
    {
        for (const auto& ss : mduSchema.sections)
        {
            if (!iniData.HasSection(ss.name))
            {
                if (ss.required)
                {
                    report.AddError("Section [{}] is missing.", ss.name);
                }

                for (const auto& ps : ss.properties)
                {
                    if (ps.required)
                    {
                        report.AddError("Property [{}].{} is missing.", ss.name, ps.key);
                    }
                }

                continue;
            }

            const IniSection& section = iniData.GetSection(ss.name);
            for (const auto& ps : ss.properties)
            {
                if (!section.HasProperty(ps.key))
                {
                    if (ps.required)
                    {
                        report.AddError("Property [{}].{} is missing.", ss.name, ps.key);
                    }
                    else if (ps.HasDefault())
                    {
                        report.AddInfo("Property [{}].{} is not provided. Default is used: \"{}\".", ss.name, ps.key,
                                       ps.default_value);
                    }
                    continue;
                }

                const IniProperty& property = section.GetProperty(ps.key);
                if (!property.HasValue())
                {
                    if (ps.required)
                    {
                        report.AddError(property.GetLineNumber(), "Property [{}].{} is empty.", ss.name, ps.key);
                    }
                    else if (ps.HasDefault())
                    {
                        report.AddInfo(property.GetLineNumber(), "Property [{}].{} is empty. Default is used: \"{}\".",
                                       ss.name, ps.key, ps.default_value);
                    }
                }
            }
        }
    }

    void MduConverter::ValidateDataAgainstSchema(const IniData& iniData)
    {
        for (const auto& section : iniData)
        {
            const auto* ss = mduSchema.FindSection(section.GetName());
            if (!ss)
            {
                report.AddWarning(section.GetLineNumber(), "Section [{}] is not a supported section.",
                                  section.GetName());
                continue;
            }

            for (const auto& property : section)
            {
                const auto* ps = ss->FindProperty(property.GetKey());
                if (!ps)
                {
                    report.AddWarning(property.GetLineNumber(), "Property [{}].{} is not a supported property.",
                                      section.GetName(), property.GetKey());
                }
            }
        }
    }

    void MduConverter::ConvertValues(const IniData& iniData) {}

} // namespace dflowfm_io