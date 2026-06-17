#pragma once

#include <dflowfm_io/IssueReport.h>
#include <dflowfm_io/MduData.h>

#include <utility>

namespace ini
{
    class IniData;
}

namespace dflowfm_io
{
    class MduConverter
    {
    public:
        static std::pair<MduData, IssueReport> Convert(const ini::IniData& iniData);
        static ini::IniData Convert(const MduData& mduData);
    };

} // namespace dflowfm_io