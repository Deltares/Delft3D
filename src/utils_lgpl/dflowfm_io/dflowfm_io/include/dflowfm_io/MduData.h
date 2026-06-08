#pragma once

#include <string>

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
    };

} // namespace dflowfm_io