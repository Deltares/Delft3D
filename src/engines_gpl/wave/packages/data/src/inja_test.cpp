#include "inja_test.h"

#include <inja/inja.hpp>

#include <algorithm>
#include <cstring>
#include <fstream>
#include <iomanip>
#include <iterator>
#include <sstream>
#include <string>

namespace {

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
};

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
        return -1;
    }

    try {
        context->data[key] = value;
        return 0;
    } catch (const std::exception&) {
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
        return -1;
    }

    try {
        std::ifstream input(template_file, std::ios::binary);
        if (!input) {
            return -1;
        }

        const std::string template_text((std::istreambuf_iterator<char>(input)),
                                        std::istreambuf_iterator<char>());

        inja::Environment env;
        env.add_callback("pad_int", 2, pad_int_callback);
        env.add_callback("zfill", 2, pad_int_callback);

        const std::string rendered = env.render(template_text, context->data);

        std::ofstream output(dest_file, std::ios::binary);
        if (!output) {
            return -1;
        }

        output.write(rendered.data(), static_cast<std::streamsize>(rendered.size()));
        return output.good() ? 0 : -1;
    } catch (const std::exception&) {
        return -1;
    }
}
