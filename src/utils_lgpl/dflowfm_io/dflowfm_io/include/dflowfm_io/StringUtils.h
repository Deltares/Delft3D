#pragma once

#include <algorithm>
#include <cctype>
#include <string_view>

namespace dflowfm_io
{

    inline bool iequals(std::string_view a, std::string_view b)
    {
        return a.size() == b.size() && std::equal(a.begin(), a.end(), b.begin(), [](unsigned char x, unsigned char y) {
                   return std::tolower(x) == std::tolower(y);
               });
    }

    inline std::string to_lowercase(std::string_view str)
    {
        std::string result(str);
        std::ranges::transform(result, result.begin(), [](unsigned char c) { return std::tolower(c); });
        return result;
    }

} // namespace dflowfm_io