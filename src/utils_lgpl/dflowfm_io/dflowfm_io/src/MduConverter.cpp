#include <dflowfm_io/ConversionResult.h>
#include <dflowfm_io/MduConverter.h>
#include <dflowfm_io/MduValidator.h>

using namespace ini;

namespace dflowfm_io
{
    static const IniSection* GetSection(const IniData& iniData, const std::string& name)
    {
        return iniData.HasSection(name) ? &iniData.GetSection(name) : nullptr;
    }

    static const IniProperty* GetProperty(const IniSection& section, const std::string& key)
    {
        return section.HasProperty(key) ? &section.GetProperty(key) : nullptr;
    }

    template <typename T>
    static void TryConvert(const IniProperty* property, IssueReport& report, T& field)
    {
        if (!property || !property->HasValue()) return;

        if (!property->TryGetConvertedValue(field))
            report.AddError(property->GetLineNumber(), "Property {} value \"{}\" could not be converted.",
                            property->GetKey(), property->GetValue());
    }

    ConversionResult<MduData> MduConverter::Convert(const IniData& iniData)
    {
        MduValidator validator;
        IssueReport report = validator.Validate(iniData);

        MduData mduData;

        if (const auto* generalSection = GetSection(iniData, "general"))
        {
            TryConvert(GetProperty(*generalSection, "program"), report, mduData.general.program);
            TryConvert(GetProperty(*generalSection, "fileVersion"), report, mduData.general.fileVersion);
        }

        if (const auto* geometrySection = GetSection(iniData, "geometry"))
        {
            TryConvert(GetProperty(*geometrySection, "netFile"), report, mduData.geometry.netFile);
            TryConvert(GetProperty(*geometrySection, "useCaching"), report, mduData.geometry.useCaching);
        }

        if (const auto* numericsSection = GetSection(iniData, "numerics"))
        {
            TryConvert(GetProperty(*numericsSection, "cflMax"), report, mduData.numerics.cflMax);
            TryConvert(GetProperty(*numericsSection, "kmx"), report, mduData.numerics.kmx);
        }

        return {std::move(mduData), std::move(report)};
    }

} // namespace dflowfm_io