#pragma once

#include <dflowfm_io/MduSchema.h>
#include <dflowfm_io/IssueReport.h>
#include <ini/IniData.h>

namespace dflowfm_io
{

    class MduValidator
    {
    public:
        explicit MduValidator(MduSchema schema = BuildMduSchema());

        IssueReport Validate(const ini::IniData& iniData) const;

    private:
        void ValidateRequired(const ini::IniData& iniData, IssueReport& report) const;
        void ValidateUnsupported(const ini::IniData& iniData, IssueReport& report) const;

        const MduSchema schema;
    };

} // namespace dflowfm_io