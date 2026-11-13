#include "ini/IniValueConversion.h"
#include "ini/StringUtils.h"

#include <algorithm>
#include <cctype>
#include <format>
#include <sstream>
#include <stdexcept>
#include <unordered_map>

namespace ini::conversion
{

    namespace
    {
        const char* GetTimePointFormatString(TimePointFormat format)
        {
            switch (format)
            {
                case TimePointFormat::IsoDateTime:
                    return "%Y-%m-%d %H:%M:%S";
                case TimePointFormat::SlashDateTime:
                    return "%Y/%m/%d %H:%M:%S";
                case TimePointFormat::IsoDate:
                    return "%Y-%m-%d";
                case TimePointFormat::SlashDate:
                    return "%Y/%m/%d";
                case TimePointFormat::CompactDateTime:
                    return "%Y%m%d%H%M%S";
                case TimePointFormat::CompactDateOnly:
                    return "%Y%m%d";
            }

            throw std::invalid_argument("Unknown TimePointFormat.");
        }

        template <typename T, typename Converter>
        std::string JoinValues(const std::vector<T>& values, char separator, Converter&& converter)
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

        template <typename T, typename Converter>
        std::vector<T> SplitValues(const std::string& value, char delimiter, Converter&& converter)
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
                    result.push_back(converter(trimmed));
                }
                start = end + 1;
            }

            const std::string trimmed = trim(value.substr(start));
            if (!trimmed.empty())
            {
                result.push_back(converter(trimmed));
            }

            return result;
        }

        template <typename T>
        T ParseNumber(const std::string& value)
        {
            T result{};
            std::istringstream iss(value);
            if (!(iss >> result))
            {
                throw std::invalid_argument("String '" + value + "' could not be converted to the target type.");
            }

            char remaining;
            if (iss >> remaining)
            {
                throw std::invalid_argument("String '" + value + "' could not be converted to the target type.");
            }

            return result;
        }
    } // namespace

    std::string BoolToString(bool value, BoolFormat format)
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

    std::string IntegerToString(int value)
    {
        std::ostringstream oss;
        oss << value;
        return oss.str();
    }

    std::string FloatingPointToString(double value, FloatFormat format)
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

    std::string TimePointToString(time_point value, TimePointFormat format)
    {
        auto truncated = std::chrono::floor<std::chrono::seconds>(value);
        return std::vformat("{:" + std::string(GetTimePointFormatString(format)) + "}",
                            std::make_format_args(truncated));
    }

    std::string PathToString(const path& value) { return value.string(); }

    std::string StringToString(const std::string& value) { return value; }

    std::string BoolListToString(const std::vector<bool>& values, BoolFormat format, char separator)
    {
        return JoinValues(values, separator, [format](bool value) { return BoolToString(value, format); });
    }

    std::string IntegerListToString(const std::vector<int>& values, char separator)
    {
        return JoinValues(values, separator, [](int value) { return IntegerToString(value); });
    }

    std::string FloatingPointListToString(const std::vector<double>& values, FloatFormat format,
                                                           char separator)
    {
        return JoinValues(values, separator, [format](double value) { return FloatingPointToString(value, format); });
    }

    std::string TimePointListToString(const std::vector<time_point>& values, TimePointFormat format,
                                                       char separator)
    {
        return JoinValues(values, separator, [format](time_point value) { return TimePointToString(value, format); });
    }

    std::string PathListToString(const std::vector<path>& values, char separator)
    {
        return JoinValues(values, separator, [](const path& value) { return PathToString(value); });
    }

    std::string StringListToString(const std::vector<std::string>& values, char separator)
    {
        return JoinValues(values, separator, [](const std::string& value) { return StringToString(value); });
    }

    bool BoolFromString(const std::string& value)
    {
        static const std::unordered_map<std::string, bool> mappings = {
            {"true", true}, {"false", false}, {"yes", true}, {"no", false}, {"1", true}, {"0", false},
        };

        std::string lower = trim(value);
        std::transform(lower.begin(), lower.end(), lower.begin(), ::tolower);

        const auto it = mappings.find(lower);
        if (it != mappings.end())
        {
            return it->second;
        }

        throw std::invalid_argument("String '" + value + "' was not recognized as a valid boolean.");
    }

    int IntegerFromString(const std::string& value)
    {
        return ParseNumber<int>(trim(value));
    }

    double FloatingPointFromString(const std::string& value)
    {
        std::string normalized = trim(value);
        const std::size_t pos = normalized.find_first_of("dD");
        if (pos != std::string::npos)
        {
            normalized[pos] = 'e';
        }

        return ParseNumber<double>(normalized);
    }

    time_point TimePointFromString(const std::string& value, TimePointFormat format)
    {
        time_point result;
        std::istringstream iss(trim(value));
        if (iss >> std::chrono::parse(GetTimePointFormatString(format), result))
        {
            // Ensure the entire string was consumed (no trailing characters)
            char remaining;
            if (!(iss >> remaining))
            {
                return result;
            }
        }

        throw std::invalid_argument("String '" + value + "' was not recognized as a valid date/time.");
    }

    path PathFromString(const std::string& value) { return path(trim(value)); }

    std::string StringFromString(const std::string& value) { return trim(value); }

    std::vector<bool> BoolListFromString(const std::string& value, char delimiter)
    {
        return SplitValues<bool>(value, delimiter, [](const std::string& item) { return BoolFromString(item); });
    }

    std::vector<int> IntegerListFromString(const std::string& value, char delimiter)
    {
        return SplitValues<int>(value, delimiter, [](const std::string& item) { return IntegerFromString(item); });
    }

    std::vector<double> FloatingPointListFromString(const std::string& value, char delimiter)
    {
        return SplitValues<double>(value, delimiter, [](const std::string& item) {
            return FloatingPointFromString(item);
        });
    }

    std::vector<time_point> TimePointListFromString(const std::string& value, TimePointFormat format,
                                                                   char delimiter)
    {
        return SplitValues<time_point>(value, delimiter, [format](const std::string& item) {
            return TimePointFromString(item, format);
        });
    }

    std::vector<path> PathListFromString(const std::string& value, char delimiter)
    {
        return SplitValues<path>(value, delimiter, [](const std::string& item) { return PathFromString(item); });
    }

    std::vector<std::string> StringListFromString(const std::string& value, char delimiter)
    {
        return SplitValues<std::string>(value, delimiter, [](const std::string& item) {
            return StringFromString(item);
        });
    }

} // namespace ini::conversion