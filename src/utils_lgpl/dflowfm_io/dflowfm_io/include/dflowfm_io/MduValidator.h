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
        explicit MduValidator();

        IssueReport Validate(const ini::IniData& iniData) const;

    private:
        void ValidateRequired(const ini::IniData& iniData, IssueReport& report) const;
        void ValidateUnsupported(const ini::IniData& iniData, IssueReport& report) const;
    };

} // namespace dflowfm_io