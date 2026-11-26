#include "MduConversion.h"
#include "MduValidation.h"

#include <dflowfm_io/MduSchema.h>
#include "StringUtils.h"

#include <ini/IniData.h>
#include <ini/IniValueConversion.h>

#include <chrono>
#include <filesystem>
#include <format>
#include <variant>

namespace dflowfm_io::conversion
{

    namespace
    {
        std::string GetExpectedValueDescription(const PropertySchema& propertySchema)
        {
            static constexpr std::string_view prefix = "Expected value type: ";
            switch (propertySchema.value_type)
            {
                case ValueType::String: return std::format("{}\"string\"", prefix);
                case ValueType::Int: return std::format("{}\"integer\"", prefix);
                case ValueType::IntBool: return std::format("{}\"integer (0 or 1)\"", prefix);
                case ValueType::Float: return std::format("{}\"float\"", prefix);
                case ValueType::Path: return std::format("{}\"path\"", prefix);
                case ValueType::PathList: return std::format("{}\"list of paths (separated by whitespace)\"", prefix);
                case ValueType::FloatList: return std::format("{}\"list of floats (separated by whitespace)\"", prefix);
                case ValueType::StringList: return std::format("{}\"list of strings (separated by whitespace)\"", prefix);
                case ValueType::StringEnum:
                case ValueType::IntEnum:
                {
                    std::string values;
                    for (const auto& ev : propertySchema.enum_values)
                    {
                        if (!values.empty()) values += ", ";
                        values += std::format("\"{}\"", ev.value);
                    }
                    return std::format("Supported values: {}", values);
                }
                case ValueType::DateTime:
                {
                    const std::string dateTimeFormat =
                        propertySchema.format.has_value() && *propertySchema.format == FormatType::Date
                        ? "yyyymmdd"
                        : "yyyymmddhhmmss";
                    return std::format("{}\"datetime with format {}\"", prefix, dateTimeFormat);
                }
                default: throw std::invalid_argument(
                    std::format("Unhandled ValueType: {}", static_cast<int>(propertySchema.value_type)));
            }
        }

        ini::FloatFormat GetFloatFormat(const PropertySchema& schema)
        {
            if (schema.format == FormatType::Fixed)
                return ini::FloatFormat::Fixed;
            if (schema.format == FormatType::Scientific)
                return ini::FloatFormat::Scientific;
            return ini::FloatFormat::General;
        }

        ini::TimePointFormat GetTimePointFormat(const PropertySchema& schema)
        {
            return (schema.format.has_value() && schema.format.value() == FormatType::Date)
                       ? ini::TimePointFormat::CompactDateOnly
                       : ini::TimePointFormat::CompactDateTime;
        }

        Value DateTimeFromString(const PropertySchema& schema, const std::string& raw)
        {
            if (raw.empty())
                return std::optional<std::chrono::system_clock::time_point>{};
            
            return std::optional<std::chrono::system_clock::time_point>{
                ini::conversion::FromString(raw, GetTimePointFormat(schema))};
        }

        [[noreturn]] void ThrowInvalidEnumValue(const PropertySchema& schema, const std::string& raw)
        {
            throw std::invalid_argument(
                std::format("Enum value '{}' is not a valid value for property '{}'.", raw, schema.key));
        }

        Value StringEnumFromString(const PropertySchema& schema, const std::string& raw)
        {
            for (const auto& ev : schema.enum_values)
                if (iequals(ev.value, raw)) return StringEnumValue{ev.value};
            ThrowInvalidEnumValue(schema, raw);
        }

        Value IntEnumFromString(const PropertySchema& schema, const std::string& raw)
        {
            for (const auto& ev : schema.enum_values)
                if (iequals(ev.value, raw))
                    return IntEnumValue{ini::conversion::FromString<int>(ev.value)};
            ThrowInvalidEnumValue(schema, raw);
        }

        std::string DateTimeToString(const PropertySchema& schema, const Value& value)
        {
            auto dateTimeValue = std::get<std::optional<std::chrono::system_clock::time_point>>(value);
            if (!dateTimeValue.has_value())
                return std::string{};

            return ini::conversion::ToString(*dateTimeValue, GetTimePointFormat(schema));
        }

        std::string StringEnumToString(const PropertySchema& schema, const Value& value)
        {
            auto enumValue = std::get<StringEnumValue>(value);
            for (const auto& ev : schema.enum_values)
                if (iequals(ev.value, enumValue.value)) return ev.value;
            ThrowInvalidEnumValue(schema, enumValue.value);
        }

        std::string IntEnumToString(const PropertySchema& schema, const Value& value)
        {
            auto enumValue = std::get<IntEnumValue>(value);
            auto valueStr = ini::conversion::ToString(enumValue.value);
            for (const auto& ev : schema.enum_values)
                if (ev.value == valueStr) return valueStr;
            ThrowInvalidEnumValue(schema, valueStr);
        }

    } // namespace

    std::pair<MduData, IssueReport> ToMduData(const ini::IniData& iniData, const MduSchema& schema)
    {
        IssueReport report = validation::Validate(iniData, schema);
        MduData mduData(schema.CreateDefaultValues());

        for (const auto& sectionSchema : schema.Sections())
        {
            if (sectionSchema.status.type == StatusType::Obsolete)
                continue;

            for (const auto& propertySchema : sectionSchema.properties)
            {
                const auto* iniProperty = iniData.FindProperty(sectionSchema.name, propertySchema.key);
                const std::string key = FormatKey(sectionSchema.name, propertySchema.key);

                if (!iniProperty || !iniProperty->HasValue())
                    continue;

                if (schema.IsObsolete(propertySchema, iniProperty->GetValue()))
                    continue;

                try
                {
                    auto converted_value = ValueFromString(propertySchema, iniProperty->GetValue());
                    mduData.setValue(key, converted_value);
                }
                catch (const std::exception&)
                {
                    const std::string expected = GetExpectedValueDescription(propertySchema);
                    report.Add<Severity::Error>(iniProperty->GetLineNumber(), "Property [{}].{} contains invalid value: \"{}\". {}.",
                                                sectionSchema.name, propertySchema.key, iniProperty->GetValue(), expected);
                }
            }
        }

        return {std::move(mduData), std::move(report)};
    }

    ini::IniData ToIniData(const MduData& mduData, const MduSchema& schema)
    {
        ini::IniData iniData;

        for (const auto& sectionSchema : schema.Sections())
        {
            auto& iniSection = iniData.AddSection(sectionSchema.name);

            for (const auto& propertySchema : sectionSchema.properties)
            {
                const std::string key = FormatKey(sectionSchema.name, propertySchema.key);
                if (!mduData.hasValue(key))
                {
                    continue;
                }

                auto value = ValueToString(propertySchema, mduData.getValue(key));

                ini::IniProperty property(propertySchema.key, std::move(value), propertySchema.description);
                iniSection.AddProperty(std::move(property));
            }
        }

        return std::move(iniData);
    }

    Value ValueFromString(const PropertySchema& schema, const std::string& raw)
    {
        switch (schema.value_type)
        {
            case ValueType::String:
                return ini::conversion::FromString<std::string>(raw);
            case ValueType::Int:
                return ini::conversion::FromString<int>(raw);
            case ValueType::Float:
                return ini::conversion::FromString<double>(raw);
            case ValueType::IntBool:
                return ini::conversion::FromString<bool>(raw);
            case ValueType::Path:
                return ini::conversion::FromString<std::filesystem::path>(raw);
            case ValueType::DateTime:
                return DateTimeFromString(schema, raw);
            case ValueType::StringList:
                return ini::conversion::FromMultiValueString<std::string>(raw);
            case ValueType::PathList:
                return ini::conversion::FromMultiValueString<std::filesystem::path>(raw);
            case ValueType::FloatList:
                return ini::conversion::FromMultiValueString<double>(raw);
            case ValueType::StringEnum:
                return StringEnumFromString(schema, raw);
            case ValueType::IntEnum:
                return IntEnumFromString(schema, raw);
            default:
                throw std::logic_error(
                    std::format("Unhandled ValueType for property '{}'.", schema.key));
        }
    }

    std::string ValueToString(const PropertySchema& schema, const Value& value)
    {
        switch (schema.value_type)
        {
            case ValueType::String:
                return ini::conversion::ToString(std::get<std::string>(value));
            case ValueType::Int:
                return ini::conversion::ToString(std::get<int>(value));
            case ValueType::Float:
                return ini::conversion::ToString(std::get<double>(value), GetFloatFormat(schema));
            case ValueType::IntBool:
                return ini::conversion::ToString(std::get<bool>(value), ini::BoolFormat::ZeroOne);
            case ValueType::Path:
                return ini::conversion::ToString(std::get<std::filesystem::path>(value));
            case ValueType::DateTime:
                return DateTimeToString(schema, value);
            case ValueType::StringList:
                return ini::conversion::ToMultiValueString(std::get<std::vector<std::string>>(value));
            case ValueType::PathList:
                return ini::conversion::ToMultiValueString(std::get<std::vector<std::filesystem::path>>(value));
            case ValueType::FloatList:
                return ini::conversion::ToMultiValueString(std::get<std::vector<double>>(value), GetFloatFormat(schema));
            case ValueType::StringEnum:
                return StringEnumToString(schema, value);
            case ValueType::IntEnum:
                return IntEnumToString(schema, value);
            default:
                throw std::logic_error(
                    std::format("Unhandled ValueType for property '{}'.", schema.key));
        }
    }

} // namespace dflowfm_io::conversion