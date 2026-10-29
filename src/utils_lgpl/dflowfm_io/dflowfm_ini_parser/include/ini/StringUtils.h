#pragma once

#include <algorithm>
#include <cctype>
#include <string_view>
#include <string>

namespace ini
{

    /// @brief Returns whether two strings are equal, ignoring case.
    /// @param a The first string to compare.
    /// @param b The second string to compare.
    /// @return @c true if @p a and @p b are equal ignoring case; otherwise @c false.
    inline bool iequals(std::string_view a, std::string_view b)
    {
        return a.size() == b.size() && std::equal(a.begin(), a.end(), b.begin(), [](unsigned char x, unsigned char y) {
                   return std::tolower(x) == std::tolower(y);
               });
    }

    /// @brief Removes leading and trailing whitespace from the specified string.
    /// @param s The string to trim.
    /// @return A copy of @p s with leading and trailing whitespace removed.
    inline std::string trim(std::string s)
    {
        s.erase(s.begin(), std::find_if(s.begin(), s.end(), [](unsigned char c) { return !std::isspace(c); }));
        s.erase(std::find_if(s.rbegin(), s.rend(), [](unsigned char c) { return !std::isspace(c); }).base(), s.end());
        return s;
    }

    /// @brief Removes trailing occurrences of the specified character from the specified string.
    /// @param s The string to trim.
    /// @param c The character to remove from the end of @p s.
    /// @return A copy of @p s with all trailing occurrences of @p c removed.
    inline std::string trim_end(std::string s, unsigned c)
    {
        s.erase(std::find_if(s.rbegin(), s.rend(), [c](unsigned ch) { return ch != c; }).base(), s.end());
        return s;
    }

} // namespace ini