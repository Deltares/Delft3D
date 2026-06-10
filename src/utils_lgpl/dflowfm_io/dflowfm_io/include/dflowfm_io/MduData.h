#pragma once

#include <string>
#include <string_view>
#include <variant>
#include <unordered_map>
#include <vector>
#include <stdexcept>

#include "dflowfm_io/StringUtils.h"

namespace dflowfm_io
{

    struct General
    {
        std::string program{"D-Flow FM"};
        std::string fileVersion;
    };

    struct Geometry
    {
        std::string netFile;
        bool useCaching{true};
    };

    struct Numerics
    {
        float cflMax{0.7f};
        int kmx{0};
    };

    struct MduData
    {
        MduData();
        ~MduData();

        int GetDummyValue() const;

        General general;
        Geometry geometry;
        Numerics numerics;

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

        using Value = std::variant<std::string, double, int, std::vector<std::string>>;
        std::unordered_map<std::string, Value> data_entries;
    };

} // namespace dflowfm_io