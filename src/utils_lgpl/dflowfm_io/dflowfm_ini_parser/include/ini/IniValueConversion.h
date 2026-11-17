#pragma once

#include <chrono>
#include <filesystem>
#include <string>
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

    /// @brief Provides named conversions between typed values and INI strings.
    /// @details Parsing trims surrounding whitespace and throws std::invalid_argument for invalid values.
    ///          List parsing ignores empty items and always treats newlines as separators.
    namespace conversion
    {
        using time_point = std::chrono::system_clock::time_point;
        using path = std::filesystem::path;

        /// @brief Formats a boolean using the specified representation.
        std::string ToString(bool value, BoolFormat format = BoolFormat::TrueFalse);

        /// @brief Formats an int without changing its stream representation.
        std::string ToString(int value);

        /// @brief Formats a double using the specified representation.
        std::string ToString(float value, FloatFormat format = FloatFormat::General);

        /// @brief Formats a double using the specified representation.
        std::string ToString(double value, FloatFormat format = FloatFormat::General);

        /// @brief Formats a time point using the required date/time format, truncated to seconds.
        std::string ToString(time_point value, TimePointFormat format);

        /// @brief Converts a path to its native string representation.
        std::string ToString(const path& value);

        /// @brief Returns a string unchanged for writing to an INI file.
        std::string ToString(const std::string& value);

        /// @brief Formats booleans as a delimited string.
        std::string ToMultiValueString(const std::vector<bool>& values,
                                       BoolFormat format = BoolFormat::TrueFalse, char separator = ' ');

        /// @brief Formats int values as a delimited string.
        std::string ToMultiValueString(const std::vector<int>& values, char separator = ' ');

        /// @brief Formats double values as a delimited string.
        std::string ToMultiValueString(const std::vector<double>& values,
                                       FloatFormat format = FloatFormat::General, char separator = ' ');

        /// @brief Formats time points as a delimited string using the required date/time format.
        std::string ToMultiValueString(const std::vector<time_point>& values, TimePointFormat format,
                                       char separator = ' ');

        /// @brief Formats paths as a delimited string.
        std::string ToMultiValueString(const std::vector<path>& values, char separator = ' ');

        /// @brief Joins strings without changing their contents.
        std::string ToMultiValueString(const std::vector<std::string>& values, char separator = ' ');

        // --------------------------------------------------------------------------------------------------------

        /// @brief Parses a string as the requested supported type.
        template <typename T>
        T FromString(const std::string& value);

        /// @brief Parses true/false, yes/no or 1/0, case-insensitively.
        template <>
        bool FromString<bool>(const std::string& value);

        /// @brief Parses an int, requiring the entire trimmed string to be consumed.
        template <>
        int FromString<int>(const std::string& value);

        /// @brief Parses a double, including Fortran D/d exponents.
        template <>
        double FromString<double>(const std::string& value);

        /// @brief Parses a time point using the required date/time format.
        time_point FromString(const std::string& value, TimePointFormat format);

        /// @brief Creates a path from the trimmed string.
        template <>
        path FromString<path>(const std::string& value);

        /// @brief Returns the trimmed string.
        template <>
        std::string FromString<std::string>(const std::string& value);

        /// @brief Parses a delimited string as a list of the requested supported type.
        template <typename T>
        std::vector<T> FromMultiValueString(const std::string& value, char delimiter = ' ');

        /// @brief Parses a delimited list of booleans.
        template <>
        std::vector<bool> FromMultiValueString<bool>(const std::string& value, char delimiter);

        /// @brief Parses a delimited list of int values.
        template <>
        std::vector<int> FromMultiValueString<int>(const std::string& value, char delimiter);

        /// @brief Parses a delimited list of double values.
        template <>
        std::vector<double> FromMultiValueString<double>(const std::string& value, char delimiter);

        /// @brief Parses a delimited list of time points using the required date/time format.
        /// @remarks Use a non-space delimiter when the date/time format itself contains spaces.
        std::vector<time_point> FromMultiValueString(const std::string& value, TimePointFormat format,
                                                     char delimiter = ' ');

        /// @brief Parses a delimited list of paths.
        template <>
        std::vector<path> FromMultiValueString<path>(const std::string& value, char delimiter);

        /// @brief Splits a delimited string into trimmed, non-empty strings.
        template <>
        std::vector<std::string> FromMultiValueString<std::string>(const std::string& value, char delimiter);
    } // namespace conversion

} // namespace ini
