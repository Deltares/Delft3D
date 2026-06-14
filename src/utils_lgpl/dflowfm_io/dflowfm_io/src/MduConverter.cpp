#include <dflowfm_io/ConversionResult.h>
#include <dflowfm_io/MduConverter.h>
#include <dflowfm_io/MduSchema.h>
#include <dflowfm_io/MduValidator.h>

#include <cassert>
#include <chrono>
#include <filesystem>
#include <format>
#include <unordered_set>
#include <variant>

using namespace ini;

namespace dflowfm_io
{

    static const IniProperty* FindProperty(
        const IniData& iniData,
        const std::string& sectionName,
        const std::string& propertyKey)
    {
        if (!iniData.HasSection(sectionName)) return nullptr;
        const auto& section = iniData.GetSection(sectionName);

        if (!section.HasProperty(propertyKey)) return nullptr;
        return &section.GetProperty(propertyKey);
    }

    static std::optional<int> ConvertEnum(
        const IniProperty& iniProperty,
        const PropertySchema& propertySchema)
    {
        auto str_value = iniProperty.GetValue();
        if (str_value.empty())
            return std::nullopt;

        int index = 0;
        for (const auto& [k, v] : propertySchema.enum_values)
        {
            if (iequals(k, str_value))
                return index;
            ++index;
        }

        return std::nullopt;
    }

    static std::optional<int> ConvertIntEnum(
        const IniProperty& iniProperty,
        const PropertySchema& propertySchema)
    {
        auto int_value = iniProperty.TryGetValue<int>();
        if (!int_value.has_value())
            return std::nullopt;

        int val = *int_value;
        if (propertySchema.enum_values.count(std::to_string(val)) > 0)
            return val;

        return std::nullopt;
    }

    static std::string ConvertEnumToString(
        int index,
        const PropertySchema& propertySchema)
    {
        int i = 0;
        for (const auto& [k, v] : propertySchema.enum_values)
        {
            if (i == index)
                return k;
            ++i;
        }

        throw std::out_of_range(std::format("enum index {} out of range for property '{}'.",
            index, propertySchema.key));
    }

    static std::optional<Value> GetPropertyValue(
        const IniProperty& iniProperty,
        const PropertySchema& propertySchema)
    {
        switch (propertySchema.value_type)
        {
            case ValueType::Path:
                return iniProperty.TryGetValue<std::filesystem::path>();
            case ValueType::String:
                return iniProperty.TryGetValue<std::string>();
            case ValueType::Int:
                return iniProperty.TryGetValue<int>();
            case ValueType::IntBool:
                return iniProperty.TryGetValue<bool>();
            case ValueType::Float:
                return iniProperty.TryGetValue<double>();
            case ValueType::Enum:
                return ConvertEnum(iniProperty, propertySchema);
            case ValueType::IntEnum:
                return ConvertIntEnum(iniProperty, propertySchema);
            case ValueType::DateTime:
                return iniProperty.TryGetValue<std::chrono::system_clock::time_point>();
            case ValueType::StringList:
                return iniProperty.TryGetValues<std::string>();
            case ValueType::PathList:
                return iniProperty.TryGetValues<std::filesystem::path>();
            case ValueType::FloatList:
                return iniProperty.TryGetValues<double>();
            default:
                throw std::logic_error("INTERNAL ERROR: Unhandled value type");
        }
    }

    static IniProperty CreateProperty(
        const MduData& mduData,
        const PropertySchema& propertySchema,
        const std::string& key)
    {
        switch (propertySchema.value_type)
        {
            case ValueType::Path:
                return IniProperty::Create(propertySchema.key, mduData.getValueAs<std::filesystem::path>(key));
            case ValueType::String:
                return IniProperty::Create(propertySchema.key, mduData.getValueAs<std::string>(key));
            case ValueType::Int:
                return IniProperty::Create(propertySchema.key, mduData.getValueAs<int>(key));
            case ValueType::IntBool:
                return IniProperty::Create(propertySchema.key, mduData.getValueAs<bool>(key) ? "1" : "0");
            case ValueType::Float:
                return IniProperty::Create(propertySchema.key, mduData.getValueAs<double>(key));
            case ValueType::Enum:
                return IniProperty::Create(propertySchema.key, ConvertEnumToString(mduData.getValueAs<int>(key), propertySchema));
            case ValueType::IntEnum:
                return IniProperty::Create(propertySchema.key, mduData.getValueAs<int>(key));
            case ValueType::DateTime:
                return IniProperty::Create(propertySchema.key, mduData.getValueAs<std::chrono::system_clock::time_point>(key));
            case ValueType::StringList:
                return IniProperty::Create(propertySchema.key, mduData.getValueAs<std::vector<std::string>>(key));
            case ValueType::PathList:
                return IniProperty::Create(propertySchema.key, mduData.getValueAs<std::vector<std::filesystem::path>>(key));
            case ValueType::FloatList:
                return IniProperty::Create(propertySchema.key, mduData.getValueAs<std::vector<double>>(key));
            default:
                throw std::logic_error("INTERNAL ERROR: Unhandled value type");
        }
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

                auto converted_value = GetPropertyValue(*iniProperty, propertySchema);
                if (!converted_value.has_value())
                {
                    report.AddError(iniProperty->GetLineNumber(), "Property [{}].{} contains invalid value: \"{}\".",
                                    sectionSchema.name, propertySchema.key, iniProperty->GetValue());
                    continue;
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
                if (!mduData.hasValue(key))
                {
                    if (propertySchema.required)
                        report.AddError("Required property [{}].{} is missing.", sectionSchema.name, propertySchema.key);
                    continue;
                }

                IniProperty property = CreateProperty(mduData, propertySchema, key);
                property.SetComment(propertySchema.description);

                iniSection.AddProperty(std::move(property));
            }
        }

        return {std::move(iniData), std::move(report)};
    }

} // namespace dflowfm_io