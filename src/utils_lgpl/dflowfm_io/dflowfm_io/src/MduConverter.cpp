#include <chrono>
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
                if (!mdu_schema_section) continue; // Unrecognized section, skip

                auto* mdu_schema_property = mdu_schema_section->FindProperty(property.GetKey());
                if (!mdu_schema_property) continue; // Unrecognized property, skip

                const ValueType value_type = mdu_schema_property->value_type;
                std::optional<MduData::Value> converted_value = std::nullopt;
                if (value_type == ValueType::Path)
                {
                    converted_value = property.TryGetConvertedValue<std::filesystem::path>();
                }
                else if (value_type == ValueType::String)
                {
                    converted_value = property.TryGetConvertedValue<std::string>();
                }
                else if (value_type == ValueType::Integer)
                {
                    converted_value = property.TryGetConvertedValue<int>();
                }
                else if (value_type == ValueType::IntBool)
                {
                    converted_value = property.TryGetConvertedValue<bool>();
                }
                else if (value_type == ValueType::FloatingPoint)
                {
                    converted_value = property.TryGetConvertedValue<double>();
                }
                else if (value_type == ValueType::DateTime)
                {
                    converted_value = property.TryGetConvertedValue<std::chrono::system_clock::time_point>();
                }
                else if (value_type == ValueType::StringList)
                {
                    converted_value = property.TryGetConvertedValueCollection<std::string>();
                }
                else if (value_type == ValueType::PathList)
                {
                    converted_value = property.TryGetConvertedValueCollection<std::filesystem::path>();
                }
                else if (value_type == ValueType::FloatingPointList)
                {
                    converted_value = property.TryGetConvertedValueCollection<double>();
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
                else if (value_type == ValueType::Integer)
                {
                    int value = mduData.getValueAs<int>(key);
                    addedProperty = &iniSection.AddProperty(propertySchema.key, value);
                }
                else if (value_type == ValueType::IntBool)
                {
                    bool value = mduData.getValueAs<bool>(key);
                    addedProperty = &iniSection.AddProperty(propertySchema.key, value ? "1" : "0");
                }
                else if (value_type == ValueType::FloatingPoint)
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
                else if (value_type == ValueType::FloatingPointList)
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