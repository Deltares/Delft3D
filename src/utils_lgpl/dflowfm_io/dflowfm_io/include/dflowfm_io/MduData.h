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

        bool hasValue(std::string_view key) const
        {
            return data_entries.find(dflowfm_io::to_lowercase(key)) != data_entries.end();
        }

        template <typename T>
        const T& getValueAs(std::string_view key) const
        {
            auto it = data_entries.find(dflowfm_io::to_lowercase(key));
            if (it == data_entries.end())
            {
                throw std::runtime_error("key/value pair not found: " + std::string(key));
            }
            return std::get<T>(it->second);
        }

        template <typename T>
        T& getValueAs(std::string_view key)
        {
            return const_cast<T&>(std::as_const(*this).getValueAs<T>(key));
        }

        using Value = std::variant<std::filesystem::path, std::string, double, int, bool, std::vector<std::string>, std::vector<std::filesystem::path>>;
        std::unordered_map<std::string, Value> data_entries;
    };

} // namespace dflowfm_io