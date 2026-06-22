#include <dflowfm_io/MduConverter.h>
#include <dflowfm_io/MduSchema.h>
#include <dflowfm_io/MduValidator.h>
#include <dflowfm_io/StringUtils.h>

#include <ini/IniData.h>

#include <cassert>
#include <chrono>
#include <filesystem>
#include <format>
#include <unordered_set>
#include <variant>

using namespace ini;

namespace dflowfm_io
{

    static std::string GetCurrentLocalTimeString()
    {
        const auto now = std::chrono::system_clock::now();
        const auto nowSeconds = std::chrono::floor<std::chrono::seconds>(now);
        const auto time = std::chrono::zoned_time{std::chrono::current_zone(), nowSeconds};
        return std::format("{:%H:%M:%S, %d-%m-%Y}", time);
    }

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

    static std::optional<EnumValue> TryConvertEnum(
        const IniProperty& iniProperty,
        const PropertySchema& propertySchema)
    {
        const auto str_value = iniProperty.GetValue();
        if (str_value.empty()) return std::nullopt;

        for (const auto& [number, name] : propertySchema.enum_values)
        {
            if (iequals(name, str_value)) return EnumValue{number};
        }
        return std::nullopt;
    }

    static std::optional<EnumValue> TryConvertIntEnum(
        const IniProperty& iniProperty,
        const PropertySchema& propertySchema)
    {
        const auto int_value = iniProperty.TryGetValue<int>();
        if (!int_value.has_value()) return std::nullopt;

        auto it = propertySchema.enum_values.find(*int_value);
        if (it == propertySchema.enum_values.end()) return std::nullopt;

        return EnumValue{it->first};
    }

    static const std::pair<const int, std::string>& FindEnumEntry(
        EnumValue enum_value,
        const PropertySchema& propertySchema)
    {
        auto it = propertySchema.enum_values.find(enum_value.value);
        if (it == propertySchema.enum_values.end())
        {
            throw std::out_of_range(std::format("enum numerical value {} out of range for property '{}'.",
                                                enum_value.value, propertySchema.key));
        }
        return *it;
    }

    static std::string ConvertEnumToString(EnumValue enum_value, const PropertySchema& propertySchema)
    {
        return FindEnumEntry(enum_value, propertySchema).second;
    }

    static int ConvertEnumToInt(EnumValue enum_value, const PropertySchema& propertySchema)
    {
        return FindEnumEntry(enum_value, propertySchema).first;
    }

    static std::optional<Value> TryGetPropertyValue(
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
                return TryConvertEnum(iniProperty, propertySchema);
            case ValueType::IntEnum:
                return TryConvertIntEnum(iniProperty, propertySchema);
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

    static Value GetDefaultPropertyValue(const PropertySchema& propertySchema)
    {
        const auto tempProperty = IniProperty::Create(propertySchema.key, propertySchema.default_value);

        auto value = TryGetPropertyValue(tempProperty, propertySchema);
        if (!value.has_value())
        {
            throw std::logic_error(
                std::format("INTERNAL ERROR: Default value \"{}\" for property '{}' could not be converted.",
                            propertySchema.default_value, propertySchema.key));
        }
        return std::move(*value);
    }

    static IniProperty CreateIniProperty(
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
                return IniProperty::Create(propertySchema.key, ConvertEnumToString(mduData.getValueAs<EnumValue>(key), propertySchema));
            case ValueType::IntEnum:
                return IniProperty::Create(propertySchema.key, ConvertEnumToInt(mduData.getValueAs<EnumValue>(key), propertySchema));
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

    std::pair<MduData, IssueReport> MduConverter::Convert(const IniData& iniData)
    {
        MduValidator validator;
        IssueReport report = validator.Validate(iniData);

        MduData mduData;

        for (const auto& sectionSchema : MDU_SCHEMA.sections)
        {
            for (const auto& propertySchema : sectionSchema.properties)
            {
                const auto* iniProperty = FindProperty(iniData, sectionSchema.name, propertySchema.key);
                const std::string key = FormatKey(sectionSchema.name, propertySchema.key);

                // A property may be absent or have no value in the MDU file;
                // fall back to the schema default if one is defined.
                if (!iniProperty || !iniProperty->HasValue())
                {
                    if (!propertySchema.default_value.empty())
                        mduData.data_entries[key] = GetDefaultPropertyValue(propertySchema);
                    continue;
                }

                auto converted_value = TryGetPropertyValue(*iniProperty, propertySchema);
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

    IniData MduConverter::Convert(const MduData& mduData)
    {
        IniData iniData;

        for (const auto& sectionSchema : MDU_SCHEMA.sections)
        {
            auto& iniSection = iniData.AddSection(sectionSchema.name);

            if (iniData.size() == 1)
            {
                iniSection.AddComment(std::format("Generated on {}\n", GetCurrentLocalTimeString()));
            }

            for (const auto& propertySchema : sectionSchema.properties)
            {
                const std::string key = FormatKey(sectionSchema.name, propertySchema.key);
                if (!mduData.hasValue(key))
                {
                    if (propertySchema.required)
                        throw std::logic_error(std::format("Required property [{}].{} is missing.",
                                               sectionSchema.name, propertySchema.key));
                    continue;
                }

                IniProperty property = CreateIniProperty(mduData, propertySchema, key);
                property.SetComment(propertySchema.description);

                iniSection.AddProperty(std::move(property));
            }
        }

        return std::move(iniData);
    }

} // namespace dflowfm_io