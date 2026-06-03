#include "ini/IniValueConverter.h"

#include <algorithm>
#include <array>
#include <format>
#include <iomanip>
#include <sstream>
#include <stdexcept>
#include <unordered_map>

namespace ini
{

    std::string IniValueConverter::BoolToString(bool value) { return value ? "True" : "False"; }

    std::string IniValueConverter::FloatToString(double value) { return std::format("{:.7e}", value); }

    std::string IniValueConverter::FloatToString(float value) { return std::format("{:.7e}", value); }

    std::string IniValueConverter::TimePointToString(std::chrono::system_clock::time_point value)
    {
        return std::format("{:%Y-%m-%d %H:%M:%S}", std::chrono::floor<std::chrono::seconds>(value));
    }

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
        };

        for (const auto* fmt : formats)
        {
            std::chrono::system_clock::time_point result;
            std::istringstream iss(value);
            if (iss >> std::chrono::parse(fmt, result))
            {
                return result;
            }
        }

        throw std::invalid_argument("String '" + value + "' was not recognized as a valid date/time.");
    }

    std::string IniValueConverter::Trim(const std::string& value)
    {
        constexpr std::string_view whitespace = " \t\r\n";

        const auto begin = value.find_first_not_of(whitespace);
        if (begin == std::string::npos)
        {
            return {};
        }

        const auto end = value.find_last_not_of(whitespace);
        return value.substr(begin, end - begin + 1);
    }

} // namespace ini