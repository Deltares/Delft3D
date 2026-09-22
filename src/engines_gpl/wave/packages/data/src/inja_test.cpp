#include "inja_test.h"

#include <inja/inja.hpp>

#include <algorithm>
#include <cstring>
#include <fstream>
#include <iterator>
#include <string>

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
        const std::string rendered = inja::render(template_text, context->data);

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

int inja_render_test(const char* template_text, const char* name, char* result, int result_size)
{
    if (template_text == nullptr || name == nullptr || result == nullptr || result_size <= 0) {
        return -1;
    }

    try {
        nlohmann::json data;
        data["name"] = name;

        const std::string rendered = inja::render(template_text, data);

        const std::size_t count = std::min(rendered.size(), static_cast<std::size_t>(result_size - 1));
        std::memcpy(result, rendered.data(), count);
        result[count] = '\0';

        return static_cast<int>(count);
    } catch (const std::exception&) {
        result[0] = '\0';
        return -1;
    }
}
