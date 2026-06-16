#pragma once

#include <filesystem>
#include <istream>
#include <ostream>

#include "dflowfm_io/MduData.h"
#include "dflowfm_io/IssueReport.h"

namespace dflowfm_io
{

    class MduFile
    {
    public:
        MduFile() = delete;

        static std::pair<MduData, IssueReport> Load(std::istream& in);
        static std::pair<MduData, IssueReport> Load(const std::filesystem::path& path);

        static void Save(std::ostream& out, const MduData& data);
        static void Save(const std::filesystem::path& path, const MduData& data);
    };

} // namespace dflowfm_io