#include <cassert>
#include <chrono>
#include <filesystem>
#include <unordered_set>

#include <dflowfm_io/ConversionResult.h>
#include <dflowfm_io/MduConverter.h>
#include <dflowfm_io/MduSchema.h>
#include <dflowfm_io/MduSchemaData.h>
#include <dflowfm_io/MduValidator.h>

using namespace ini;

namespace dflowfm_io
{

    const IniProperty* FindProperty(const IniData& iniData, const std::string& name, const std::string& key)
    {
        if (!iniData.HasSection(name)) return nullptr;
        const auto& section = iniData.GetSection(name);

        if (!section.HasProperty(key)) return nullptr;
        return &section.GetProperty(key);
    }

    ConversionResult<MduData> MduConverter::Convert(const IniData& iniData)
    {
        MduValidator validator;
        IssueReport report = validator.Validate(iniData);

        MduData mduData;

        for (const auto& sectionSchema : MDU_SCHEMA.sections)
        {
            for (const auto& propertySchema : sectionSchema.properties)
            {
                const auto* iniProperty = FindProperty(iniData, sectionSchema.name, propertySchema.key);

                const std::string key = to_lowercase(sectionSchema.name + "." + propertySchema.key);

                // A property may be absent or have no value in the MDU file; 
                // fall back to the schema default if one is defined.
                if (!iniProperty || !iniProperty->HasValue())
                {
                    if (propertySchema.default_value.has_value())
                        mduData.data_entries[key] = *propertySchema.default_value;
                    continue;
                }

                const ValueType value_type = propertySchema.value_type;
                std::optional<Value> converted_value = std::nullopt;
                if (value_type == ValueType::Path)
                {
                    converted_value = iniProperty->TryGetConvertedValue<std::filesystem::path>();
                }
                else if (value_type == ValueType::String)
                {
                    converted_value = iniProperty->TryGetConvertedValue<std::string>();
                }
                else if (value_type == ValueType::Int)
                {
                    converted_value = iniProperty->TryGetConvertedValue<int>();
                }
                else if (value_type == ValueType::IntBool)
                {
                    converted_value = iniProperty->TryGetConvertedValue<bool>();
                }
                else if (value_type == ValueType::Float)
                {
                    converted_value = iniProperty->TryGetConvertedValue<double>();
                }
                else if (value_type == ValueType::DateTime)
                {
                    converted_value = iniProperty->TryGetConvertedValue<std::chrono::system_clock::time_point>();
                }
                else if (value_type == ValueType::StringList)
                {
                    converted_value = iniProperty->TryGetConvertedValueCollection<std::string>();
                }
                else if (value_type == ValueType::PathList)
                {
                    converted_value = iniProperty->TryGetConvertedValueCollection<std::filesystem::path>();
                }
                else if (value_type == ValueType::FloatList)
                {
                    converted_value = iniProperty->TryGetConvertedValueCollection<double>();
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

            if (iniData.size() == 1)
            {
                iniSection.AddComment(MDU_SCHEMA.description);
                iniSection.AddComment("");
            }

            iniSection.AddComment(sectionSchema.description);

            for (const auto& propertySchema : sectionSchema.properties)
            {
                const std::string key = to_lowercase(sectionSchema.name + "." + propertySchema.key);
                if (!mduData.hasValue(key)) continue;

                const ValueType value_type = propertySchema.value_type;
                IniProperty* addedProperty = nullptr;
                if (value_type == ValueType::Path)
                {
                    const auto& value = mduData.getValueAs<std::filesystem::path>(key);
                    addedProperty = &iniSection.AddProperty(propertySchema.key, value);
                }
                else if (value_type == ValueType::String)
                {
                    std::string value = mduData.getValueAs<std::string>(key);
                    addedProperty = &iniSection.AddProperty(propertySchema.key, value);
                }
                else if (value_type == ValueType::Int)
                {
                    int value = mduData.getValueAs<int>(key);
                    addedProperty = &iniSection.AddProperty(propertySchema.key, value);
                }
                else if (value_type == ValueType::IntBool)
                {
                    bool value = mduData.getValueAs<bool>(key);
                    addedProperty = &iniSection.AddProperty(propertySchema.key, value ? "1" : "0");
                }
                else if (value_type == ValueType::Float)
                {
                    double value = mduData.getValueAs<double>(key);
                    addedProperty = &iniSection.AddProperty(propertySchema.key, value);
                }
                else if (value_type == ValueType::DateTime)
                {
                    const auto& value = mduData.getValueAs<std::chrono::system_clock::time_point>(key);
                    addedProperty = &iniSection.AddProperty(propertySchema.key, value);
                }
                else if (value_type == ValueType::StringList)
                {
                    const auto& values = mduData.getValueAs<std::vector<std::string>>(key);
                    addedProperty = &iniSection.AddMultiValueProperty(propertySchema.key, values);
                }
                else if (value_type == ValueType::PathList)
                {
                    const auto& values = mduData.getValueAs<std::vector<std::filesystem::path>>(key);
                    addedProperty = &iniSection.AddMultiValueProperty(propertySchema.key, values);
                }
                else if (value_type == ValueType::FloatList)
                {
                    const auto& values = mduData.getValueAs<std::vector<double>>(key);
                    iniSection.AddMultiValueProperty(propertySchema.key, values);
                }
                else
                {
                    throw std::logic_error("INTERNAL ERROR: Unhandled value type");
                }

                if (addedProperty && !propertySchema.description.empty())
                {
                    addedProperty->SetComment(propertySchema.description);
                }
            }
        }

        return {std::move(iniData), std::move(report)};
    }

} // namespace dflowfm_io