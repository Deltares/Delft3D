#include "ini/IniValueConverter.h"

#include <algorithm>
#include <array>
#include <format>
#include <sstream>
#include <stdexcept>
#include <unordered_map>

namespace ini
{

    std::string IniValueConverter::BoolToString(bool value) { return value ? "True" : "False"; }

    std::string IniValueConverter::TimePointToString(std::chrono::system_clock::time_point value)
    {
        auto truncated = std::chrono::floor<std::chrono::seconds>(value);
        auto dayTruncated = std::chrono::floor<std::chrono::days>(truncated);

        if (truncated == dayTruncated)
        {
            return std::format("{:%Y%m%d}", truncated);
        }
        return std::format("{:%Y%m%d%H%M%S}", truncated);
    }

    std::string IniValueConverter::PathToString(const std::filesystem::path& value) { return value.string(); }

    bool IniValueConverter::BoolFromString(const std::string& value)
    {
        static const std::unordered_map<std::string, bool> mappings = {
            {"true", true}, {"false", false}, {"yes", true}, {"no", false}, {"1", true}, {"0", false},
        };

        std::string lower = value;
        std::transform(lower.begin(), lower.end(), lower.begin(), ::tolower);

        const auto it = mappings.find(lower);
        if (it != mappings.end())
        {
            return it->second;
        }

        throw std::invalid_argument("String '" + value + "' was not recognized as a valid boolean.");
    }

    std::chrono::system_clock::time_point IniValueConverter::TimePointFromString(const std::string& value)
    {
        static constexpr std::array formats = {
            "%Y-%m-%d %H:%M:%S", // 2020-01-30 12:00:00
            "%Y/%m/%d %H:%M:%S", // 2020/01/30 12:00:00
            "%Y-%m-%d",          // 2020-01-30
            "%Y/%m/%d",          // 2020/01/30
            "%Y%m%d%H%M%S",      // 20200130120000
            "%Y%m%d",            // 20200130
        };

        for (const auto* fmt : formats)
        {
            std::chrono::system_clock::time_point result;
            std::istringstream iss(value);
            if (iss >> std::chrono::parse(fmt, result))
            {
                // Ensure the entire string was consumed (no trailing characters)
                char remaining;
                if (!(iss >> remaining))
                {
                    return result;
                }
            }
        }

        throw std::invalid_argument("String '" + value + "' was not recognized as a valid date/time.");
    }

    std::filesystem::path IniValueConverter::PathFromString(const std::string& value)
    {
        return std::filesystem::path(value);
    }

} // namespace ini