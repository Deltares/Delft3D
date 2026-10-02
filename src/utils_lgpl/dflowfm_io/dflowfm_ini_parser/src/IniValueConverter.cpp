#include "ini/IniValueConverter.h"

#include <algorithm>
#include <array>
#include <format>
#include <sstream>
#include <stdexcept>
#include <unordered_map>

namespace ini
{

    using time_point = std::chrono::system_clock::time_point;
    using path = std::filesystem::path;

    static const std::vector<std::pair<TimePointFormat, std::string>> timePointFormats = {
        {TimePointFormat::IsoDateTime, "%Y-%m-%d %H:%M:%S"},
        {TimePointFormat::SlashDateTime, "%Y/%m/%d %H:%M:%S"},
        {TimePointFormat::IsoDate, "%Y-%m-%d"},
        {TimePointFormat::SlashDate, "%Y/%m/%d"},
        {TimePointFormat::CompactDateTime, "%Y%m%d%H%M%S"},
        {TimePointFormat::CompactDateOnly, "%Y%m%d"},
    };

    std::string IniValueConverter::BoolToString(bool value, BoolFormat format)
    {
        switch (format)
        {
            case BoolFormat::ZeroOne:
                return value ? "1" : "0";
            case BoolFormat::YesNo:
                return value ? "yes" : "no";
            case BoolFormat::TrueFalse:
            default:
                return value ? "True" : "False";
        }
    }

    std::string IniValueConverter::TimePointToString(time_point value, TimePointFormat format)
    {
        auto truncated = std::chrono::floor<std::chrono::seconds>(value);
        for (const auto& [fmt, formatString] : timePointFormats)
        {
            if (fmt == format)
            {
                return std::vformat("{:" + formatString + "}", std::make_format_args(truncated));
            }
        }

        throw std::invalid_argument("Unknown TimePointFormat.");
    }

    std::string IniValueConverter::PathToString(const path& value) { return value.string(); }

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

    time_point IniValueConverter::TimePointFromString(const std::string& value, std::optional<TimePointFormat> format)
    {
        for (const auto& [fmt, formatString] : timePointFormats)
        {
            if (format.has_value() && fmt != *format)
            {
                continue;
            }

            time_point result;
            std::istringstream iss(value);
            if (iss >> std::chrono::parse(formatString, result))
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

} // namespace ini