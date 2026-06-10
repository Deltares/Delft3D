#include <unordered_set>
#include <filesystem>

#include <dflowfm_io/ConversionResult.h>
#include <dflowfm_io/MduConverter.h>
#include <dflowfm_io/MduSchema.h>

#include <cassert>
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

    ConversionResult<MduData> MduConverter::Convert(const IniData& iniData)
    {
        MduValidator validator;
        IssueReport report = validator.Validate(iniData);

        MduData mduData;

        mduData.data_entries["general.program"] = std::string("D-Flow FM"); // Default value

        for (const auto& section : iniData)
        {
            for (const auto& property : section)
            {
                // Often properties are present in the MDU file but no value is specified. Skip these.
                if (!property.HasValue()) continue;

                const std::string key = to_lowercase(section.GetName() + "." + property.GetKey());

                auto* mdu_schema_section = MDU_SCHEMA.FindSection(section.GetName());
                if (!mdu_schema_section)
                {
                    continue; // Unrecognized section, skip
                }

                auto* mdu_schema_property = mdu_schema_section->FindProperty(property.GetKey());
                if (!mdu_schema_property)
                {
                    continue; // Unrecognized property, skip
                }

                const ValueType value_type = mdu_schema_property->value_type;
                std::optional<MduData::Value> converted_value = std::nullopt;
                if (value_type == ValueType::Path)
                {
                    std::filesystem::path value;
                    if (property.TryGetConvertedValue(value)) converted_value = value;
                }
                else if (value_type == ValueType::String)
                {
                    std::string value;
                    if (property.TryGetConvertedValue(value)) converted_value = value;
                }
                else if (value_type == ValueType::Integer)
                {
                    int intValue;
                    if (property.TryGetConvertedValue(intValue)) converted_value = intValue;
                }
                else if (value_type == ValueType::IntBool)
                {
                    bool value;
                    if (property.TryGetConvertedValue(value)) converted_value = value;
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
                else if (value_type == ValueType::PathList)
                {
                    std::vector<std::filesystem::path> values;
                    if (property.TryGetConvertedValueCollection(values)) converted_value = values;
                }
                else
                {
                    throw std::logic_error("INTERNAL ERROR: Unhandled value type");
                }

                if (!converted_value.has_value())
                {
                    assert(false); // TODO decent error handling
                }
                mduData.data_entries[key] = std::move(*converted_value);
            }
        }

        return {std::move(mduData), std::move(report)};
    }

    ConversionResult<IniData> MduConverter::Convert(const MduData& mduData)
    {
        IniData iniData;
        IssueReport report;

        for (const auto& sectionSchema : MDU_SCHEMA.sections)
        {
            auto& iniSection = iniData.AddSection(sectionSchema.name);

            for (const auto& propertySchema : sectionSchema.properties)
            {
                const std::string key = to_lowercase(sectionSchema.name + "." + propertySchema.key);

                const ValueType value_type = propertySchema.value_type;
                if (value_type == ValueType::Path)
                {
                    const auto& value = mduData.getValueAs<std::filesystem::path>(key);
                    iniSection.AddProperty(propertySchema.key, value.string());
                }
                else if (value_type == ValueType::String)
                {
                    std::string value = mduData.getValueAs<std::string>(key);
                    iniSection.AddProperty(propertySchema.key, value);
                }
                else if (value_type == ValueType::Integer)
                {
                    int value = mduData.getValueAs<int>(key);
                    iniSection.AddProperty(propertySchema.key, value);
                }
                else if (value_type == ValueType::IntBool)
                {
                    bool value = mduData.getValueAs<bool>(key);
                    iniSection.AddProperty(propertySchema.key, value);
                }
                else if (value_type == ValueType::FloatingPoint)
                {
                    double value = mduData.getValueAs<double>(key);
                    iniSection.AddProperty(propertySchema.key, value);
                }
                else if (value_type == ValueType::StringList)
                {
                    const auto& values = mduData.getValueAs<std::vector<std::string>>(key);
                    iniSection.AddMultiValueProperty(propertySchema.key, values);
                }
                else if (value_type == ValueType::PathList)
                {
                    const auto& values = mduData.getValueAs<std::vector<std::filesystem::path>>(key);
                    iniSection.AddMultiValueProperty(propertySchema.key, values);
                }
                else
                {
                    throw std::logic_error("INTERNAL ERROR: Unhandled value type");
                }
            }
        }

        return {std::move(iniData), std::move(report)};
    }

} // namespace dflowfm_io