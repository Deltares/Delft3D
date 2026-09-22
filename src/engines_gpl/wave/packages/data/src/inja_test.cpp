#include "inja_test.h"

#include <inja/inja.hpp>

#include <algorithm>
#include <cstring>
#include <string>

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
