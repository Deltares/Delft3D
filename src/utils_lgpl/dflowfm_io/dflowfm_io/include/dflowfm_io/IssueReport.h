#pragma once

#include <format>
#include <optional>
#include <string>
#include <vector>

namespace dflowfm_io
{

    enum class Severity
    {
        Info,
        Warning,
        Error
    };

    struct Issue
    {
        Severity severity;
        std::string message;
        std::optional<int> lineNumber;
    };

    class IssueReport
    {
    public:
        template <typename... Args>
        void AddError(std::format_string<Args...> fmt, Args&&... args)
        {
            AddIssue(Severity::Error, std::nullopt, std::format(fmt, std::forward<Args>(args)...));
        }

        template <typename... Args>
        void AddWarning(std::format_string<Args...> fmt, Args&&... args)
        {
            AddIssue(Severity::Warning, std::nullopt, std::format(fmt, std::forward<Args>(args)...));
        }

        template <typename... Args>
        void AddInfo(std::format_string<Args...> fmt, Args&&... args)
        {
            AddIssue(Severity::Info, std::nullopt, std::format(fmt, std::forward<Args>(args)...));
        }

        template <typename... Args>
        void AddError(int lineNumber, std::format_string<Args...> fmt, Args&&... args)
        {
            AddIssue(Severity::Error, lineNumber, std::format(fmt, std::forward<Args>(args)...));
        }

        template <typename... Args>
        void AddWarning(int lineNumber, std::format_string<Args...> fmt, Args&&... args)
        {
            AddIssue(Severity::Warning, lineNumber, std::format(fmt, std::forward<Args>(args)...));
        }

        template <typename... Args>
        void AddInfo(int lineNumber, std::format_string<Args...> fmt, Args&&... args)
        {
            AddIssue(Severity::Info, lineNumber, std::format(fmt, std::forward<Args>(args)...));
        }

        bool HasInfos() const;
        bool HasWarnings() const;
        bool HasErrors() const;

        void Merge(const IssueReport& other);

        std::string Format() const;

        bool empty() const;
        std::size_t size() const;

        std::vector<Issue>::iterator begin();
        std::vector<Issue>::const_iterator begin() const;
        std::vector<Issue>::iterator end();
        std::vector<Issue>::const_iterator end() const;

        Issue& operator[](std::size_t index);
        const Issue& operator[](std::size_t index) const;

    private:
        std::vector<Issue> issues;

        void AddIssue(Severity severity, std::optional<int> lineNumber, std::string message);
        bool HasSeverity(Severity severity) const;
    };

} // namespace dflowfm_io