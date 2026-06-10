#include <dflowfm_io/ConversionResult.h>
#include <dflowfm_io/MduConverter.h>

#include <unordered_set>
#include <dflowfm_io/MduValidator.h>

using namespace ini;

enum class ValueType
{
    String,
    StringList,
    Integer,
    FloatingPoint
};

static const std::unordered_map<std::string, ValueType> KEY_VALUE_TYPES = {
    {"general.program", ValueType::String},
    {"general.fileversion", ValueType::String},
    {"geometry.kmx", ValueType::Integer},
    {"geometry.waterlevini", ValueType::FloatingPoint}
};

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
        mduData.data_entries["general.program"] = "D-Flow FM"; // Default value
        for (const auto& section : iniData)
        {
            for (const auto& property : section)
            {
                const std::string key = to_lowercase(section.GetName() + "." + property.GetKey());

                auto it = KEY_VALUE_TYPES.find(key);
                if (it == KEY_VALUE_TYPES.end())
                {
                    continue; // Unrecognized key, skip
                }

                const ValueType value_type = it->second;
                std::optional<MduData::Value> converted_value = std::nullopt;
                if (value_type == ValueType::String)
                {
                    std::string value;
                    if (property.TryGetConvertedValue(value)) converted_value = value;
                }
                else if (value_type == ValueType::Integer)
                {
                    int intValue;
                    if (property.TryGetConvertedValue(intValue)) converted_value = intValue;
                }
                else if (value_type == ValueType::FloatingPoint)
                {
                    double doubleValue;
                    if (property.TryGetConvertedValue(doubleValue)) converted_value = doubleValue;
                }
                else if (value_type == ValueType::StringList)
                {
                    std::vector<std::string> values;
                    if (property.TryGetConvertedValueCollection(values)) converted_value = values;
                }
                else
                {
                    continue; // Unrecognized value type, skip
                }

                if (!converted_value.has_value())
                {
                    // TODO error
                }
                                
                mduData.data_entries[key] = std::move(*converted_value);
            }
        }

        return {std::move(mduData), std::move(report)};
    }

} // namespace dflowfm_io