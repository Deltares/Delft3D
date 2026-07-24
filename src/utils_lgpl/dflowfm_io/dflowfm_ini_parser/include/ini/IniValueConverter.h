#pragma once

#include "ini/StringUtils.h"

#include <chrono>
#include <filesystem>
#include <sstream>
#include <stdexcept>
#include <string>
#include <type_traits>
#include <vector>

namespace ini
{
    /// @brief Describes the string format for boolean values.
    enum class BoolFormat
    {
        ZeroOne,
        YesNo,
        TrueFalse
    };

    /// @brief Describes the string format for floating point values.
    enum class FloatFormat
    {
        General,
        Fixed,
        Scientific
    };

    /// @brief Describes the string format for date/time values.
    enum class TimePointFormat
    {
        CompactDateOnly, // 20200130
        CompactDateTime, // 20200130120000
        IsoDate,         // 2020-01-30
        IsoDateTime,     // 2020-01-30 12:00:00
        SlashDate,       // 2020/01/30
        SlashDateTime,   // 2020/01/30 12:00:00
    };

    /// @brief Provides methods for converting values from and to string representations suitable for INI files.
    class IniValueConverter
    {
    public:
        using time_point = std::chrono::system_clock::time_point;
        using path = std::filesystem::path;

        /// @brief Converts a value of the specified type @p T to its INI string representation.
        /// @tparam T The type of the value to convert.
        /// @param value The value to convert.
        /// @return The string representation of the value.
        template <typename T>
        static std::string ToString(const T& value)
        {
            if constexpr (std::is_same_v<T, bool>)
            {
                return BoolToString(value);
            }
            else if constexpr (std::is_floating_point_v<T>)
            {
                return FloatingPointToString(value);
            }
            else if constexpr (std::is_same_v<T, time_point>)
            {
                return TimePointToString(value);
            }
            else if constexpr (std::is_same_v<T, path>)
            {
                return PathToString(value);
            }
            else
            {
                return DefaultToString(value);
            }
        }

        /// @brief Converts a bool value to its INI string representation using the specified format.
        /// @param value The bool value to convert.
        /// @param format The bool string format to apply to the value.
        /// @return The string representation of the bool value.
        static std::string ToString(bool value, BoolFormat format) { return BoolToString(value, format); }

        /// @brief Converts a floating point value to its INI string representation using the specified format.
        /// @tparam T The floating point type to convert.
        /// @param value The floating point value to convert.
        /// @param format The numeric string format to apply to the value.
        /// @return The string representation of the floating point value.
        template <std::floating_point T>
        static std::string ToString(T value, FloatFormat format)
        {
            return FloatingPointToString(value, format);
        }

        /// @brief Converts a time_point value to its INI string representation using the specified format.
        /// @param value The time_point value to convert.
        /// @param format The date/time string format to apply to the value.
        /// @return The string representation of the time_point value.
        static std::string ToString(time_point value, TimePointFormat format)
        {
            return TimePointToString(value, format);
        }

        /// @brief Converts a collection of values to a delimited INI string representation.
        /// @tparam T The type of each value to convert.
        /// @param values The collection of values to convert.
        /// @param separator Character used to separate values. Default is a whitespace.
        /// @return Delimited string representation of @p values.
        template <typename T>
        static std::string ToMultiValueString(const std::vector<T>& values, char separator = ' ')
        {
            return JoinValues(values, separator, [](const T& value) { return ToString(value); });
        }

        /// @brief Converts a collection of bool values to a delimited INI string using the specified format.
        /// @param values The bool values to convert.
        /// @param format The bool string format to apply to each value.
        /// @param separator Character used to separate values. Default is a whitespace.
        /// @return Delimited string representation of @p values in the requested @p format.
        static std::string ToMultiValueString(const std::vector<bool>& values, BoolFormat format, char separator = ' ')
        {
            return JoinValues(values, separator, [format](bool value) { return ToString(value, format); });
        }

        /// @brief Converts a collection of floating point values to a delimited INI string using the specified format.
        /// @tparam T The floating point type to convert.
        /// @param values The floating point values to convert.
        /// @param format The numeric string format to apply to each value.
        /// @param separator Character used to separate values. Default is a whitespace.
        /// @return Delimited string representation of @p values in the requested @p format.
        template <std::floating_point T>
        static std::string ToMultiValueString(const std::vector<T>& values, FloatFormat format, char separator = ' ')
        {
            return JoinValues(values, separator, [format](T value) { return ToString(value, format); });
        }

        /// @brief Converts a collection of time_point values to a delimited INI string using the specified format.
        /// @param values The time_point values to convert.
        /// @param format The date/time string format to apply to each value.
        /// @param separator Character used to separate values. Default is a whitespace.
        /// @return Delimited string representation of @p values in the requested @p format.
        static std::string ToMultiValueString(const std::vector<time_point>& values, TimePointFormat format, char separator = ' ')
        {
            return JoinValues(values, separator, [format](time_point value) {
                return ToString(value, format);
            });
        }

        /// @brief Converts a string to a value of the specified type @p T.
        /// @tparam T The target type to convert to.
        /// @param value The string representation of the value.
        /// @return The converted value of the specified type.
        /// @throws std::invalid_argument When @p value is empty or does not represent a valid format.
        template <typename T>
        static T FromString(const std::string& value)
        {
            const std::string trimmed = trim(value);

            if constexpr (std::is_same_v<T, std::string>)
            {
                return trimmed;
            }
            else if constexpr (std::is_same_v<T, bool>)
            {
                return BoolFromString(trimmed);
            }
            else if constexpr (std::is_floating_point_v<T>)
            {
                return FloatingPointFromString<T>(trimmed);
            }
            else if constexpr (std::is_same_v<T, time_point>)
            {
                return TimePointFromString(trimmed);
            }
            else if constexpr (std::is_same_v<T, path>)
            {
                return PathFromString(trimmed);
            }
            else
            {
                return DefaultFromString<T>(trimmed);
            }
        }

        /// @brief Converts a string to a time_point value, restricting parsing to formats matching @p format.
        /// @param value The string representation of the time_point value.
        /// @param format The expected date/time string format of @p value.
        /// @return The converted time_point value.
        /// @throws std::invalid_argument When @p value is empty or does not match the expected format.
        static time_point FromString(const std::string& value, TimePointFormat format)
        {
            return TimePointFromString(trim(value), format);
        }

        /// @brief Converts a delimited string to a collection of values of type @p T.
        /// @tparam T The target type to convert each value to.
        /// @param value Delimited string containing the values.
        /// @param delimiter The character used to separate the values in @p value. Default is a whitespace.
        /// @return A vector containing the converted values of the specified type.
        /// @throws std::invalid_argument When one or more values do not represent a valid format.
        /// @remarks Newline characters (\r\n) are always treated as separators, regardless of @p delimiter.
        template <typename T>
        static std::vector<T> FromMultiValueString(const std::string& value, char delimiter = ' ')
        {
            const std::string delimiters = {delimiter, '\r', '\n'};

            std::vector<T> result;
            std::size_t start = 0;
            std::size_t end;

            while ((end = value.find_first_of(delimiters, start)) != std::string::npos)
            {
                const std::string trimmed = trim(value.substr(start, end - start));
                if (!trimmed.empty())
                {
                    result.push_back(FromString<T>(trimmed));
                }
                start = end + 1;
            }

            const std::string trimmed = trim(value.substr(start));
            if (!trimmed.empty())
            {
                result.push_back(FromString<T>(trimmed));
            }

            return result;
        }

    private:
        static std::string BoolToString(bool value, BoolFormat format = BoolFormat::TrueFalse);
        static std::string TimePointToString(time_point value, TimePointFormat format = TimePointFormat::CompactDateTime);
        static std::string PathToString(const path& value);

        template <std::floating_point T>
        static std::string FloatingPointToString(T value, FloatFormat format = FloatFormat::General)
        {
            switch (format)
            {
                case FloatFormat::Fixed:
                    return std::format("{:f}", value);
                case FloatFormat::Scientific:
                    return std::format("{:e}", value);
                case FloatFormat::General:
                default: {
                    std::string result = std::format("{:.7g}", value);

                    if (result.find('.') == std::string::npos && 
                        result.find('e') == std::string::npos &&
                        result.find('E') == std::string::npos)
                    {
                        result += ".0";
                    }
                    return result;
                }
            }
        }

        template <typename T>
        static std::string DefaultToString(const T& value)
        {
            std::ostringstream oss;
            oss << value;
            return oss.str();
        }

        template <typename T, typename Converter>
        static std::string JoinValues(const std::vector<T>& values, char separator, Converter&& converter)
        {
            std::ostringstream oss;
            for (std::size_t i = 0; i < values.size(); ++i)
            {
                if (i > 0)
                {
                    oss << separator;
                }
                oss << converter(values[i]);
            }

            return oss.str();
        }

        static bool BoolFromString(const std::string& value);
        static time_point TimePointFromString(const std::string& value, std::optional<TimePointFormat> format = std::nullopt);
        static path PathFromString(const std::string& value) { return path(value); }

        template <std::floating_point T>
        static T FloatingPointFromString(const std::string& value)
        {
            // Fortran uses 'D'/'d' as the exponent marker (e.g. 1.234D+05).
            // Replace it with 'e' so std::istringstream can parse it.
            std::string normalized = value;
            const std::size_t pos = normalized.find_first_of("dD");
            if (pos != std::string::npos)
            {
                normalized[pos] = 'e';
            }

            return DefaultFromString<T>(normalized);
        }

        template <typename T>
        static T DefaultFromString(const std::string& value)
        {
            T result{};
            std::istringstream iss(value);

            if (!(iss >> result))
            {
                throw std::invalid_argument("String '" + value + "' could not be converted to the target type.");
            }

            // Ensure the entire string was consumed (no trailing characters)
            char remaining;
            if (iss >> remaining)
            {
                throw std::invalid_argument("String '" + value + "' could not be converted to the target type.");
            }

            return result;
        }
    };

} // namespace ini