#pragma once

#include <filesystem>
#include <istream>
#include <ostream>

#include <dflowfm_io/MduData.h>

namespace dflowfm_io
{

    class MduFile
    {
    public:
        MduFile() = delete;

        static MduData Load(std::istream& in);
        static MduData Load(const std::filesystem::path& path);

        static void Save(std::ostream& out, const MduData& data);
        static void Save(const std::filesystem::path& path, const MduData& data);
    };

} // namespace dflowfm_io