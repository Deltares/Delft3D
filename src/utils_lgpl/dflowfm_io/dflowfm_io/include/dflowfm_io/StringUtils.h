#pragma once

#include <algorithm>
#include <cctype>
#include <string>
#include <string_view>

namespace dflowfm_io
{

    /// @brief Returns true if two strings are equal, ignoring ASCII case differences.
    /// @param a First string to compare.
    /// @param b Second string to compare.
    /// @return @c true if @p a and @p b have the same length and are character-wise
    ///         equal when both are lowercased; @c false otherwise.
    inline bool iequals(std::string_view a, std::string_view b)
    {
        return a.size() == b.size() && std::equal(a.begin(), a.end(), b.begin(), [](unsigned char x, unsigned char y) {
                   return std::tolower(x) == std::tolower(y);
               });
    }

    /// @brief Returns a lowercased copy of the given string.
    /// @param str The input string to convert.
    /// @return A new @c std::string with every ASCII character lowercased.
    inline std::string tolower(std::string_view str)
    {
        std::string result(str);
        std::ranges::transform(result, result.begin(), [](unsigned char c) { return std::tolower(c); });
        return result;
    }

    /// @brief Formats a fully qualified property key from a section name and property key.
    /// @param section The section name.
    /// @param property The property key.
    /// @return A lowercase dot-separated string in the form "sectionname.propertykey".
    inline std::string FormatKey(const std::string& section, const std::string& property)
    {
        return tolower(section + "." + property);
    }

} // namespace dflowfm_io