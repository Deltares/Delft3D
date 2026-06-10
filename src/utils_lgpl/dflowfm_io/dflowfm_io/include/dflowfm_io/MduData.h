#pragma once

#include <string>
#include <string_view>
#include <variant>
#include <unordered_map>
#include <vector>
#include <stdexcept>
#include <filesystem>

#include "dflowfm_io/StringUtils.h"

namespace dflowfm_io
{
    struct MduData
    {
        int GetDummyValue() const;

        template <typename T>
        T& getValueAs(std::string_view key)
        {
            auto it = data_entries.find(dflowfm_io::to_lowercase(key));
            if (it == data_entries.end())
            {
                throw std::runtime_error("key/value pair not found: " + std::string(key));
            }
            return std::get<T>(it->second);
        }

        using Value = std::variant<std::filesystem::path, std::string, double, int, bool, std::vector<std::string>, std::vector<std::filesystem::path>>;
        std::unordered_map<std::string, Value> data_entries;
    };

} // namespace dflowfm_io