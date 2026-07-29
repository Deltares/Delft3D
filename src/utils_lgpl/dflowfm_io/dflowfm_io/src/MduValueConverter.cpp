#include <dflowfm_io/MduValueConverter.h>

#include <dflowfm_io/StringUtils.h>

#include <ini/IniValueConverter.h>

#include <chrono>
#include <filesystem>
#include <format>
#include <stdexcept>
#include <variant>

namespace dflowfm_io
{

    namespace
    {
        ini::FloatFormat GetFloatFormat(const PropertySchema& schema)
        {
            if (schema.format == FormatType::Fixed)
            {
                return ini::FloatFormat::Fixed;
            }
            if (schema.format == FormatType::Scientific)
            {
                return ini::FloatFormat::Scientific;
            }
            return ini::FloatFormat::General;
        }

        ini::TimePointFormat GetTimePointFormat(const PropertySchema& schema)
        {
            return (schema.format.has_value() && schema.format.value() == FormatType::Date)
                       ? ini::TimePointFormat::CompactDateOnly
                       : ini::TimePointFormat::CompactDateTime;
        }

        Value EnumFromString(const PropertySchema& schema, const std::string& raw)
        {
            for (const auto& ev : schema.enum_values)
                if (iequals(ev.label, raw)) return EnumValue{ev.value};
            throw std::invalid_argument(
                std::format("'{}' is not a valid value for property '{}'.", raw, schema.key));
        }

        Value IntEnumFromString(const PropertySchema& schema, const std::string& raw)
        {
            const int number = ini::IniValueConverter::FromString<int>(raw);
            for (const auto& ev : schema.enum_values)
                if (ev.value == number) return EnumValue{ev.value};
            throw std::invalid_argument(
                std::format("'{}' is not a valid value for property '{}'.", raw, schema.key));
        }

        std::string EnumToString(const PropertySchema& schema, const Value& value)
        {
            auto enumValue = std::get<EnumValue>(value);
            for (const auto& ev : schema.enum_values)
                if (ev.value == enumValue.value) return ev.label;
            throw std::out_of_range(
                std::format("Enum value {} is out of range for property '{}'.", enumValue.value, schema.key));
        }

    } // namespace

    Value MduValueConverter::FromString(const PropertySchema& schema, const std::string& raw)
    {
        switch (schema.value_type)
        {
            case ValueType::String:
                return ini::IniValueConverter::FromString<std::string>(raw);
            case ValueType::Int:
                return ini::IniValueConverter::FromString<int>(raw);
            case ValueType::Float:
                return ini::IniValueConverter::FromString<double>(raw);
            case ValueType::IntBool:
                return ini::IniValueConverter::FromString<bool>(raw);
            case ValueType::Path:
                return ini::IniValueConverter::FromString<std::filesystem::path>(raw);
            case ValueType::DateTime:
                return ini::IniValueConverter::FromString(raw, GetTimePointFormat(schema));
            case ValueType::StringList:
                return ini::IniValueConverter::FromMultiValueString<std::string>(raw);
            case ValueType::PathList:
                return ini::IniValueConverter::FromMultiValueString<std::filesystem::path>(raw);
            case ValueType::FloatList:
                return ini::IniValueConverter::FromMultiValueString<double>(raw);
            case ValueType::Enum:
                return EnumFromString(schema, raw);
            case ValueType::IntEnum:
                return IntEnumFromString(schema, raw);
            default:
                throw std::logic_error(
                    std::format("Unhandled ValueType for property '{}'.", schema.key));
        }
    }

    std::string MduValueConverter::ToString(const PropertySchema& schema, const Value& value)
    {
        switch (schema.value_type)
        {
            case ValueType::String:
                return ini::IniValueConverter::ToString(std::get<std::string>(value));
            case ValueType::Int:
                return ini::IniValueConverter::ToString(std::get<int>(value));
            case ValueType::Float:
                return ini::IniValueConverter::ToString(std::get<double>(value), GetFloatFormat(schema));
            case ValueType::IntBool:
                return ini::IniValueConverter::ToString(std::get<bool>(value), ini::BoolFormat::ZeroOne);
            case ValueType::Path:
                return ini::IniValueConverter::ToString(std::get<std::filesystem::path>(value));
            case ValueType::DateTime:
                return ini::IniValueConverter::ToString(
                    std::get<std::chrono::system_clock::time_point>(value), GetTimePointFormat(schema));
            case ValueType::StringList:
                return ini::IniValueConverter::ToMultiValueString(std::get<std::vector<std::string>>(value));
            case ValueType::PathList:
                return ini::IniValueConverter::ToMultiValueString(std::get<std::vector<std::filesystem::path>>(value));
            case ValueType::FloatList:
                return ini::IniValueConverter::ToMultiValueString(std::get<std::vector<double>>(value), GetFloatFormat(schema));
            case ValueType::Enum:
                return EnumToString(schema, value);
            case ValueType::IntEnum:
                return ini::IniValueConverter::ToString(std::get<EnumValue>(value).value);
            default:
                throw std::logic_error(
                    std::format("Unhandled ValueType for property '{}'.", schema.key));
        }
    }

} // namespace dflowfm_io