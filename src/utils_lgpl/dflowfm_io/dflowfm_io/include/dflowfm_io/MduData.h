#pragma once

#include <string>
#include <string_view>
#include <unordered_map>

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

        double getValueAsDouble(std::string_view key) const;
        int getValueAsInt(std::string_view key) const;
        std::string getValueAsString(std::string_view key) const;

        std::unordered_map<std::string, std::string> entries_string;
        std::unordered_map<std::string, double> entries_double;
        std::unordered_map<std::string, int> entries_int;
    };

} // namespace dflowfm_io