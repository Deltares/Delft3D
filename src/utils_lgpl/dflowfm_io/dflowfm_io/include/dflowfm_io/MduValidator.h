#pragma once

#include <dflowfm_io/MduSchema.h>
#include <dflowfm_io/IssueReport.h>

namespace ini
{
    class IniData;
}

namespace dflowfm_io
{

    class MduValidator
    {
    public:
        static IssueReport Validate(const ini::IniData& iniData);

    private:
        static void ValidateRequired(const ini::IniData& iniData, IssueReport& report);
        static void ValidateUnsupported(const ini::IniData& iniData, IssueReport& report);
    };

} // namespace dflowfm_io