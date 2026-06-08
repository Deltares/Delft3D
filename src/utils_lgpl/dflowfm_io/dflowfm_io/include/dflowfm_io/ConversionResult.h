#pragma once

#include <dflowfm_io/IssueReport.h>

namespace dflowfm_io
{

    template <typename T>
    struct ConversionResult
    {
        T value;
        IssueReport report;

        bool IsValid() const { return !report.HasErrors(); }
        bool HasIssues() const { return !report.empty(); }
        
        std::string FormatIssues() const { return report.Format(); }
    };

} // namespace dflowfm_io