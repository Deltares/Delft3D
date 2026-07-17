#include <dflowfm_io/MduDataConverter.h>
#include <dflowfm_io/MduValueConverter.h>
#include <dflowfm_io/MduSchema.h>
#include <dflowfm_io/MduValidator.h>
#include <dflowfm_io/StringUtils.h>

#include <ini/IniData.h>

#include <format>

namespace dflowfm_io
{

    static const ini::IniProperty* FindProperty(
        const ini::IniData& iniData,
        const std::string& sectionName,
        const std::string& propertyKey)
    {
        if (!iniData.HasSection(sectionName)) return nullptr;
        const auto& section = iniData.GetSection(sectionName);

        if (!section.HasProperty(propertyKey)) return nullptr;
        return &section.GetProperty(propertyKey);
    }

    static std::string GetExpectedValueDescription(const PropertySchema& propertySchema)
    {
        static constexpr std::string_view prefix = "Expected value type: ";
        switch (propertySchema.value_type)
        {
            case ValueType::Enum:
            case ValueType::IntEnum:
            {
                std::string values;
                for (const auto& [value, description] : propertySchema.enum_values)
                {
                    if (!values.empty()) values += ", ";
                    values += propertySchema.value_type == ValueType::IntEnum
                        ? std::format("\"{}\"", value)
                        : std::format("\"{}\"", description);
                }
                return std::format("Supported values: {}", values);
            }
            case ValueType::String: return std::format("{}\"string\"", prefix);
            case ValueType::Int: return std::format("{}\"integer\"", prefix);
            case ValueType::IntBool: return std::format("{}\"integer (0 or 1)\"", prefix);
            case ValueType::Float: return std::format("{}\"float\"", prefix);
            case ValueType::Path: return std::format("{}\"path\"", prefix);
            case ValueType::PathList: return std::format("{}\"list of paths (separated by whitespace)\"", prefix);
            case ValueType::FloatList: return std::format("{}\"list of floats (separated by whitespace)\"", prefix);
            case ValueType::StringList: return std::format("{}\"list of strings (separated by whitespace)\"", prefix);
            case ValueType::DateTime: return std::format("{}\"datetime with format {}\"", prefix, propertySchema.format);
            default: throw std::invalid_argument(std::format("Unhandled ValueType: {}", static_cast<int>(propertySchema.value_type)));
        }
    }

    static std::string GetCurrentTimeString()
    {
        const auto now = std::chrono::system_clock::now();
        const auto nowSeconds = std::chrono::floor<std::chrono::seconds>(now);
        const auto time = std::chrono::zoned_time{std::chrono::current_zone(), nowSeconds};

        return std::format("{:%H:%M:%S, %d-%m-%Y}", time);
    }

    std::pair<MduData, IssueReport> MduDataConverter::Convert(const ini::IniData& iniData)
    {
        MduValidator validator;
        IssueReport report = validator.Validate(iniData);

        MduData mduData = MduData::CreateFromSchema();

        for (const auto& sectionSchema : MDU_SCHEMA.Sections())
        {
            for (const auto& propertySchema : sectionSchema.properties)
            {
                const auto* iniProperty = FindProperty(iniData, sectionSchema.name, propertySchema.key);
                const std::string key = FormatKey(sectionSchema.name, propertySchema.key);

                if (!iniProperty || !iniProperty->HasValue()) continue;

                auto converted_value = MduValueConverter::FromString(propertySchema, iniProperty->GetValue());
                if (!converted_value.has_value())
                {
                    const std::string expected = GetExpectedValueDescription(propertySchema);
                    report.AddError(iniProperty->GetLineNumber(), "Property [{}].{} contains invalid value: \"{}\". {}.",
                                    sectionSchema.name, propertySchema.key, iniProperty->GetValue(), expected);
                    continue;
                }

                mduData.data_entries[key] = std::move(*converted_value);
            }
        }

        return {std::move(mduData), std::move(report)};
    }

    ini::IniData MduDataConverter::Convert(const MduData& mduData)
    {
        ini::IniData iniData;

        for (const auto& sectionSchema : MDU_SCHEMA.Sections())
        {
            auto& iniSection = iniData.AddSection(sectionSchema.name);

            if (iniData.size() == 1)
            {
                iniSection.AddComment(std::format("Generated on {}\n", GetCurrentTimeString()));
            }

            for (const auto& propertySchema : sectionSchema.properties)
            {
                const std::string key = FormatKey(sectionSchema.name, propertySchema.key);
                if (!mduData.hasValue(key))
                {
                    continue;
                }

                auto value = MduValueConverter::ToString(propertySchema, mduData.data_entries.at(key));

                ini::IniProperty property(propertySchema.key, std::move(value), propertySchema.description);
                iniSection.AddProperty(std::move(property));
            }
        }

        return std::move(iniData);
    }

} // namespace dflowfm_io