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

    /// @brief Provides methods for converting values from and to string representations suitable for INI files.
    class IniValueConverter
    {
    public:
        /// @brief Converts the specified value to its string representation for INI serialization.
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
            else if constexpr (std::is_same_v<T, std::chrono::system_clock::time_point>)
            {
                return TimePointToString(value);
            }
            else if constexpr (std::is_same_v<T, std::filesystem::path>)
            {
                return PathToString(value);
            }
            else
            {
                return DefaultToString(value);
            }
        }

        /// @brief Converts the specified collection of values to its string representation for INI serialization.
        /// @tparam T The type of each value to convert.
        /// @param values The collection of values to convert.
        /// @param separator The character used to separate the values in the resulting string. Default is a whitespace.
        /// @return The string representation of the collection of values.
        template <typename T>
        static std::string ToMultiValueString(const std::vector<T>& values, char separator = ' ')
        {
            std::ostringstream oss;
            for (std::size_t i = 0; i < values.size(); ++i)
            {
                if (i > 0)
                {
                    oss << separator;
                }
                oss << ToString(values[i]);
            }

            return oss.str();
        }

        /// @brief Converts the specified string representation to a value of the specified type.
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
            else if constexpr (std::is_same_v<T, std::chrono::system_clock::time_point>)
            {
                return TimePointFromString(trimmed);
            }
            else if constexpr (std::is_same_v<T, std::filesystem::path>)
            {
                return PathFromString(trimmed);
            }
            else
            {
                return DefaultFromString<T>(trimmed);
            }
        }

        /// @brief Converts the specified string representation to a collection of values of the specified type.
        /// @tparam T The target type to convert each value to.
        /// @param value The string representation of the multiple values.
        /// @param delimiter The character used to separate the values in @p value. Default is a whitespace.
        ///                  Newline characters (\r\n) are always treated as separators, regardless of this value.
        /// @return A vector containing the converted values of the specified type.
        /// @throws std::invalid_argument When one or more values do not represent a valid format.
        template <typename T>
        static std::vector<T> FromMultiValueString(const std::string& value, char delimiter = ' ')
        {
            std::vector<T> result;
            const std::string delimiters = {delimiter, '\r', '\n'};
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
        static std::string BoolToString(bool value);
        static std::string TimePointToString(std::chrono::system_clock::time_point value);
        static std::string PathToString(const std::filesystem::path& value);

        template <std::floating_point T>
        static std::string FloatingPointToString(T value)
        {
            std::string result = std::format("{:.7g}", value);

            if (result.find('.') == std::string::npos &&
                result.find('e') == std::string::npos &&
                result.find('E') == std::string::npos)
            {
                result += ".0";
            }
            return result;
        }

        template <typename T>
        static std::string DefaultToString(const T& value)
        {
            std::ostringstream oss;
            oss << value;
            return oss.str();
        }

        static bool BoolFromString(const std::string& value);
        static std::chrono::system_clock::time_point TimePointFromString(const std::string& value);
        static std::filesystem::path PathFromString(const std::string& value);

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