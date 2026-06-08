#include <algorithm>
#include <sstream>

#include <dflowfm_io/IssueReport.h>

namespace dflowfm_io
{

    void IssueReport::AddIssue(Severity severity, std::optional<int> lineNumber, std::string message)
    {
        auto it = std::lower_bound(issues.begin(), issues.end(), lineNumber,
                                   [](const Issue& issue, const std::optional<int>& line) {
                                       if (!issue.lineNumber) return true;
                                       if (!line) return false;
                                       return *issue.lineNumber < *line;
                                   });
        issues.emplace(it, severity, std::move(message), lineNumber);
    }

    bool IssueReport::HasInfos() const { return HasSeverity(Severity::Info); }

    bool IssueReport::HasWarnings() const { return HasSeverity(Severity::Warning); }

    bool IssueReport::HasErrors() const { return HasSeverity(Severity::Error); }

    bool IssueReport::HasSeverity(Severity severity) const
    {
        return std::any_of(issues.begin(), issues.end(),
                           [severity](const Issue& issue) { return issue.severity == severity; });
    }

    void IssueReport::Merge(const IssueReport& other)
    {
        for (const auto& issue : other.issues)
        {
            AddIssue(issue.severity, issue.lineNumber, issue.message);
        }
    }

    std::string IssueReport::Format() const
    {
        std::ostringstream oss;
        for (const auto& issue : issues)
        {
            std::string severityStr;
            switch (issue.severity)
            {
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

    bool IssueReport::empty() const { return issues.empty(); }

    std::size_t IssueReport::size() const { return issues.size(); }

    std::vector<Issue>::iterator IssueReport::begin() { return issues.begin(); }

    std::vector<Issue>::const_iterator IssueReport::begin() const { return issues.begin(); }

    std::vector<Issue>::iterator IssueReport::end() { return issues.end(); }

    std::vector<Issue>::const_iterator IssueReport::end() const { return issues.end(); }

    Issue& IssueReport::operator[](std::size_t index) { return issues[index]; }

    const Issue& IssueReport::operator[](std::size_t index) const { return issues[index]; }

} // namespace dflowfm_io