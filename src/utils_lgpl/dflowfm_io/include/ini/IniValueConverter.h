#pragma once

#include "ini/StringUtils.h"

#include <dflowfm_io/dflowfm_io_export.h>

#include <chrono>
#include <sstream>
#include <stdexcept>
#include <string>
#include <type_traits>
#include <vector>

namespace ini
{

    /// @brief Provides methods for converting values from and to string representations suitable for INI files.
    class DFLOWFM_IO_EXPORT IniValueConverter
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
                return FloatToString(value);
            }
            else if constexpr (std::is_same_v<T, std::chrono::system_clock::time_point>)
            {
                return TimePointToString(value);
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
            else if constexpr (std::is_same_v<T, std::chrono::system_clock::time_point>)
            {
                return TimePointFromString(trimmed);
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
        /// @return A vector containing the converted values of the specified type.
        /// @throws std::invalid_argument When one or more values do not represent a valid format.
        template <typename T>
        static std::vector<T> FromMultiValueString(const std::string& value, char delimiter = ' ')
        {
            std::vector<T> result;
            std::istringstream iss(value);
            std::string token;

            while (std::getline(iss, token, delimiter))
            {
                const std::string trimmed = trim(token);
                if (!trimmed.empty())
                {
                    result.push_back(FromString<T>(trimmed));
                }
            }

            return result;
        }

    private:
        static std::string BoolToString(bool value);
        static std::string FloatToString(double value);
        static std::string FloatToString(float value);
        static std::string TimePointToString(std::chrono::system_clock::time_point value);

        template <typename T>
        static std::string DefaultToString(const T& value)
        {
            std::ostringstream oss;
            oss << value;
            return oss.str();
        }

        static bool BoolFromString(const std::string& value);
        static std::chrono::system_clock::time_point TimePointFromString(const std::string& value);

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