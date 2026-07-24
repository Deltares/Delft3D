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
        template <typename T>
        std::optional<Value> TryFromString(const std::string& raw)
        {
            try { return ini::IniValueConverter::FromString<T>(raw); }
            catch (const std::exception&) { return std::nullopt; }
        }

        template <typename T>
        std::optional<Value> TryFromMultiValueString(const std::string& raw)
        {
            try { return ini::IniValueConverter::FromMultiValueString<T>(raw); }
            catch (const std::exception&) { return std::nullopt; }
        }

        std::optional<Value> TryEnumFromString(const PropertySchema& schema, const std::string& raw)
        {
            for (const auto& ev : schema.enum_values)
                if (iequals(ev.label, raw)) return EnumValue{ev.value};
            return std::nullopt;
        }

        std::optional<Value> TryIntEnumFromString(const PropertySchema& schema, const std::string& raw)
        {
            int number{};
            try { number = ini::IniValueConverter::FromString<int>(raw); }
            catch (const std::exception&) { return std::nullopt; }

            for (const auto& ev : schema.enum_values)
                if (ev.value == number) return EnumValue{ev.value};
            return std::nullopt;
        }

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
                       ? ini::TimePointFormat::DateOnly
                       : ini::TimePointFormat::DateTime;
        }

        template <typename T>
        std::string ValueToString(const Value& value)
        {
            return ini::IniValueConverter::ToString(std::get<T>(value));
        }

        template <>
        std::string ValueToString<bool>(const Value& value)
        {
            return ini::IniValueConverter::ToString(std::get<bool>(value), ini::BoolFormat::ZeroOne);
        }

        std::string FloatToString(const PropertySchema& schema, const Value& value)
        {
            return ini::IniValueConverter::ToString(std::get<double>(value), GetFloatFormat(schema));
        }

        std::string DateTimeToString(const PropertySchema& schema, const Value& value)
        {
            const auto& timePoint = std::get<std::chrono::system_clock::time_point>(value);
            const auto format = GetTimePointFormat(schema);
            return ini::IniValueConverter::ToString(timePoint, format);
        }

        template <typename T>
        std::string MultiValueToString(const Value& value)
        {
            return ini::IniValueConverter::ToMultiValueString(std::get<std::vector<T>>(value));
        }

        std::string FloatListToString(const PropertySchema& schema, const Value& value)
        {
            const auto& values = std::get<std::vector<double>>(value);
            const auto format = GetFloatFormat(schema);
            return ini::IniValueConverter::ToMultiValueString(values, format);
        }

        std::string EnumToString(const PropertySchema& schema, const Value& value)
        {
            auto enumValue = std::get<EnumValue>(value);
            for (const auto& ev : schema.enum_values)
                if (ev.value == enumValue.value) return ev.label;
            throw std::out_of_range(
                std::format("Enum value {} is out of range for property '{}'.", enumValue.value, schema.key));
        }

        std::string IntEnumToString(const Value& value)
        {
            return ini::IniValueConverter::ToString(std::get<EnumValue>(value).value);
        }

    } // namespace

    std::optional<Value> MduValueConverter::FromString(const PropertySchema& schema, const std::string& raw)
    {
        switch (schema.value_type)
        {
            case ValueType::String:
                return TryFromString<std::string>(raw);
            case ValueType::Int:
                return TryFromString<int>(raw);
            case ValueType::Float:
                return TryFromString<double>(raw);
            case ValueType::IntBool:
                return TryFromString<bool>(raw);
            case ValueType::Path:
                return TryFromString<std::filesystem::path>(raw);
            case ValueType::DateTime:
                return TryFromString<std::chrono::system_clock::time_point>(raw);
            case ValueType::StringList:
                return TryFromMultiValueString<std::string>(raw);
            case ValueType::PathList:
                return TryFromMultiValueString<std::filesystem::path>(raw);
            case ValueType::FloatList:
                return TryFromMultiValueString<double>(raw);
            case ValueType::Enum:
                return TryEnumFromString(schema, raw);
            case ValueType::IntEnum:
                return TryIntEnumFromString(schema, raw);
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
                return ValueToString<std::string>(value);
            case ValueType::Int:
                return ValueToString<int>(value);
            case ValueType::Float:
                return FloatToString(schema, value);
            case ValueType::IntBool:
                return ValueToString<bool>(value);
            case ValueType::Path:
                return ValueToString<std::filesystem::path>(value);
            case ValueType::DateTime:
                return DateTimeToString(schema, value);
            case ValueType::StringList:
                return MultiValueToString<std::string>(value);
            case ValueType::PathList:
                return MultiValueToString<std::filesystem::path>(value);
            case ValueType::FloatList:
                return FloatListToString(schema, value);
            case ValueType::Enum:
                return EnumToString(schema, value);
            case ValueType::IntEnum:
                return IntEnumToString(value);
            default:
                throw std::logic_error(
                    std::format("Unhandled ValueType for property '{}'.", schema.key));
        }
    }

} // namespace dflowfm_io