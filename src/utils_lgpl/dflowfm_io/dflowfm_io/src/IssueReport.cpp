#include <algorithm>
#include <sstream>

#include <dflowfm_io/IssueReport.h>

namespace dflowfm_io
{
    void IssueReport::AddIssue(Severity severity, std::optional<int> lineNumber, std::string message)
    {
        auto it = std::upper_bound(issues.begin(), issues.end(), lineNumber,
                                [](const std::optional<int>& line, const Issue& issue) {
                                    return line < issue.lineNumber;
                                });
        issues.emplace(it, severity, std::move(message), lineNumber);
    }

    std::span<const Issue> IssueReport::GetIssues() const { return issues; }

    std::string IssueReport::Format(Severity minSeverity) const
    {
        std::ostringstream oss;
        for (const auto& issue : issues)
        {
            if (issue.severity < minSeverity)
            {
                continue;
            }

            std::string severityStr;
            switch (issue.severity)
            {
                case Severity::Debug:
                    severityStr = "Debug";
                    break;
                case Severity::Info:
                    severityStr = "Info";
                    break;
                case Severity::Warning:
                    severityStr = "Warning";
                    break;
                case Severity::Error:
                    severityStr = "Error";
                    break;
            }

            if (issue.lineNumber)
            {
                oss << std::format("{} on line {}: {}\n", severityStr, *issue.lineNumber, issue.message);
            }
            else
            {
                oss << std::format("{}: {}\n", severityStr, issue.message);
            }
        }

        return oss.str();
    }

} // namespace dflowfm_io