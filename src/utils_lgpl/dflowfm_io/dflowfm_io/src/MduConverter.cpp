#include <dflowfm_io/ConversionResult.h>
#include <dflowfm_io/MduConverter.h>

#include <unordered_set>
#include <dflowfm_io/MduValidator.h>

using namespace ini;

static const std::unordered_set<std::string> STRING_KEYS = {"general.program", "general.fileversion" };
static const std::unordered_set<std::string> INTEGER_KEYS = {"geometry.kmx" };
static const std::unordered_set<std::string> FLOATING_POINT_KEYS = {"geometry.waterlevini" };

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

        // TODO : This is a temporary solution to allow retrieving values by key without having to know the section and
        // property names. Still very WIP / experimental.
        mduData.entries_string["general.program"] = "D-Flow FM"; // Default value
        for (const auto& section : iniData)
        {
            for (const auto& property : section)
            {
                const std::string key = to_lowercase(section.GetName() + "." + property.GetKey());
                const std::string& value = property.GetValue();

                if (STRING_KEYS.contains(key))
                {
                    mduData.entries_string[key] = value;
                }
                else if (INTEGER_KEYS.contains(key))
                {
                    int integer_value;
                    if (property.TryGetConvertedValue(integer_value))
                    {
                        mduData.entries_int[key] = integer_value;
                    }
                } 
                else if (FLOATING_POINT_KEYS.contains(key))
                {
                    double float_value;
                    if (property.TryGetConvertedValue(float_value))
                    {
                        mduData.entries_double[key] = float_value;
                    }
                }
            }
        }





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