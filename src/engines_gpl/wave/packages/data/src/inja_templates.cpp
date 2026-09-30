#include "inja_templates.h"

#include <inja/inja.hpp>

#include <algorithm>
#include <chrono>
#include <cstdint>
#include <cstring>
#include <fstream>
#include <iomanip>
#include <iterator>
#include <limits>
#include <sstream>
#include <stdexcept>
#include <string>

namespace {

struct parsed_date_time {
    std::chrono::year_month_day date;
    int hour;
    int minute;
    int second;
};

int parse_digits(const std::string& value, std::size_t offset, std::size_t length)
{
    int result = 0;
    for (std::size_t index = offset; index < offset + length; ++index) {
        const char digit = value[index];
        if (digit < '0' || digit > '9') {
            throw std::invalid_argument("Date must use YYYYMMDD.HHmmss format.");
        }
        result = result * 10 + (digit - '0');
    }
    return result;
}

parsed_date_time parse_date_time(const nlohmann::json& value)
{
    if (!value.is_string()) {
        throw std::invalid_argument("DATE must be a string in YYYYMMDD.HHmmss format.");
    }

    const std::string text = value.get<std::string>();
    if (text.size() != 15 || text[8] != '.') {
        throw std::invalid_argument("DATE must use YYYYMMDD.HHmmss format.");
    }

    const int year = parse_digits(text, 0, 4);
    const int month = parse_digits(text, 4, 2);
    const int day = parse_digits(text, 6, 2);
    const int hour = parse_digits(text, 9, 2);
    const int minute = parse_digits(text, 11, 2);
    const int second = parse_digits(text, 13, 2);
    const std::chrono::year_month_day date{
        std::chrono::year{year}, std::chrono::month{static_cast<unsigned>(month)},
        std::chrono::day{static_cast<unsigned>(day)}};

    if (year < 1 || !date.ok() || hour > 23 || minute > 59 || second > 59) {
        throw std::invalid_argument("DATE contains an invalid calendar date or time.");
    }

    return {date, hour, minute, second};
}

std::string format_date_time(const std::chrono::year_month_day& date, int hour,
                             int minute, int second)
{
    std::ostringstream stream;
    stream << std::setfill('0') << std::setw(4) << static_cast<int>(date.year())
           << std::setw(2) << static_cast<unsigned>(date.month())
           << std::setw(2) << static_cast<unsigned>(date.day()) << '.'
           << std::setw(2) << hour << std::setw(2) << minute << std::setw(2) << second;
    return stream.str();
}

nlohmann::json date_add_callback(inja::Arguments& args)
{
    if (args.size() != 3 || !args[1]->is_string() || !args[2]->is_number_integer()) {
        throw std::invalid_argument("date_add expects (DATE, UNIT, integer AMOUNT).");
    }

    const parsed_date_time input = parse_date_time(*args[0]);
    const std::string unit = args[1]->get<std::string>();
    const std::int64_t amount = args[2]->get<std::int64_t>();
    std::chrono::year_month_day result_date = input.date;
    int result_hour = input.hour;
    int result_minute = input.minute;
    int result_second = input.second;

    const int input_year = static_cast<int>(input.date.year());
    const unsigned input_month = static_cast<unsigned>(input.date.month());
    const unsigned input_day = static_cast<unsigned>(input.date.day());

    if (unit == "YEAR" || unit == "MONTH") {
        std::int64_t month_index = (input_year - 1) * 12 + input_month - 1;
        if (unit == "YEAR") {
            if (amount < 1 - input_year || amount > 9999 - input_year) {
                throw std::out_of_range("date_add result is outside years 0001 through 9999.");
            }
            month_index += amount * 12;
        } else {
            if (amount < -month_index || amount > 9999 * 12 - 1 - month_index) {
                throw std::out_of_range("date_add result is outside years 0001 through 9999.");
            }
            month_index += amount;
        }

        const int result_year = static_cast<int>(month_index / 12 + 1);
        const unsigned result_month = static_cast<unsigned>(month_index % 12 + 1);
        const std::chrono::year_month_day_last last_day{
            std::chrono::year{result_year},
            std::chrono::month_day_last{std::chrono::month{result_month}}};
        const unsigned result_day = std::min(input_day, static_cast<unsigned>(last_day.day()));
        result_date = {std::chrono::year{result_year}, std::chrono::month{result_month},
                       std::chrono::day{result_day}};
    } else {
        std::int64_t seconds_per_unit = 0;
        if (unit == "DAY") {
            seconds_per_unit = 86400;
        } else if (unit == "HOUR") {
            seconds_per_unit = 3600;
        } else if (unit == "MINUTE") {
            seconds_per_unit = 60;
        } else if (unit == "SECOND") {
            seconds_per_unit = 1;
        } else {
            throw std::invalid_argument("UNIT must be YEAR, MONTH, DAY, HOUR, MINUTE, or SECOND.");
        }

        if (amount > std::numeric_limits<std::int64_t>::max() / seconds_per_unit ||
            amount < std::numeric_limits<std::int64_t>::min() / seconds_per_unit) {
            throw std::out_of_range("date_add amount is too large.");
        }

        const auto timestamp = std::chrono::sys_seconds{
            std::chrono::sys_days{input.date}.time_since_epoch() +
            std::chrono::hours{input.hour} + std::chrono::minutes{input.minute} +
            std::chrono::seconds{input.second}};
        const std::int64_t current_seconds = timestamp.time_since_epoch().count();
        const std::int64_t delta = amount * seconds_per_unit;
        if ((delta > 0 && current_seconds > std::numeric_limits<std::int64_t>::max() - delta) ||
            (delta < 0 && current_seconds < std::numeric_limits<std::int64_t>::min() - delta)) {
            throw std::out_of_range("date_add result is outside the supported date range.");
        }

        const auto result_timestamp = std::chrono::sys_seconds{
            std::chrono::seconds{current_seconds + delta}};
        const auto day_start = std::chrono::floor<std::chrono::days>(result_timestamp);
        result_date = std::chrono::year_month_day{day_start};
        const auto time_of_day = result_timestamp - day_start;
        result_hour = static_cast<int>(std::chrono::duration_cast<std::chrono::hours>(time_of_day).count());
        const auto after_hours = time_of_day - std::chrono::hours{result_hour};
        result_minute = static_cast<int>(std::chrono::duration_cast<std::chrono::minutes>(after_hours).count());
        result_second = static_cast<int>(std::chrono::duration_cast<std::chrono::seconds>(
            after_hours - std::chrono::minutes{result_minute}).count());

        const int result_year = static_cast<int>(result_date.year());
        if (result_year < 1 || result_year > 9999) {
            throw std::out_of_range("date_add result is outside years 0001 through 9999.");
        }
    }

    return format_date_time(result_date, result_hour, result_minute, result_second);
}

nlohmann::json pad_int_callback(inja::Arguments& args)
{
    if (args.size() != 2) {
        return nlohmann::json("");
    }

    const nlohmann::json& value = *args[0];
    const nlohmann::json& width = *args[1];

    if (!width.is_number_integer()) {
        return value;
    }

    const int target_width = width.get<int>();
    if (target_width <= 0) {
        return value;
    }

    long long numeric_value = 0;
    if (value.is_number_integer()) {
        numeric_value = value.get<long long>();
    } else if (value.is_string()) {
        try {
            numeric_value = std::stoll(value.get<std::string>());
        } catch (const std::exception&) {
            return value;
        }
    } else {
        return value;
    }

    std::ostringstream stream;
    stream << std::setw(target_width) << std::setfill('0') << numeric_value;
    return stream.str();
}

} // namespace

struct inja_context {
    nlohmann::json data;
    std::string last_error;
};

namespace {

void set_error(inja_context* context, const std::string& message)
{
    if (context != nullptr) {
        context->last_error = message;
    }
}

} // namespace

inja_context* inja_create_context(void)
{
    try {
        return new inja_context{};
    } catch (const std::exception&) {
        return nullptr;
    }
}

int inja_add_string(inja_context* context, const char* key, const char* value)
{
    if (context == nullptr || key == nullptr || value == nullptr) {
        set_error(context, "Context, key, and value must not be null.");
        return -1;
    }

    try {
        context->data[key] = value;
        context->last_error.clear();
        return 0;
    } catch (const std::exception& exception) {
        set_error(context, exception.what());
        return -1;
    }
}

void inja_destroy_context(inja_context* context)
{
    delete context;
}

int inja_render_file(inja_context* context, const char* template_file,
                     const char* dest_file)
{
    if (context == nullptr || template_file == nullptr || dest_file == nullptr) {
        set_error(context, "Context, template file, and destination file must not be null.");
        return -1;
    }

    try {
        std::ifstream input(template_file, std::ios::binary);
        if (!input) {
            set_error(context, std::string("Unable to open template file: ") + template_file);
            return -1;
        }

        const std::string template_text((std::istreambuf_iterator<char>(input)),
                                        std::istreambuf_iterator<char>());

        inja::Environment env;
        env.add_callback("pad_int", 2, pad_int_callback);
        env.add_callback("zfill", 2, pad_int_callback);
        env.add_callback("date_add", 3, date_add_callback);

        const std::string rendered = env.render(template_text, context->data);

        std::ofstream output(dest_file, std::ios::binary);
        if (!output) {
            set_error(context, std::string("Unable to open destination file: ") + dest_file);
            return -1;
        }

        output.write(rendered.data(), static_cast<std::streamsize>(rendered.size()));
        if (!output.good()) {
            set_error(context, std::string("Unable to write destination file: ") + dest_file);
            return -1;
        }

        context->last_error.clear();
        return 0;
    } catch (const std::exception& exception) {
        set_error(context, std::string("Unable to render template file ") + template_file + ": " + exception.what());
        return -1;
    }
}

int inja_get_last_error(const inja_context* context, char* result, int result_size)
{
    if (context == nullptr || result == nullptr || result_size <= 0) {
        return -1;
    }

    const std::size_t count = std::min(context->last_error.size(), static_cast<std::size_t>(result_size - 1));
    std::memcpy(result, context->last_error.data(), count);
    result[count] = '\0';
    return static_cast<int>(count);
}
