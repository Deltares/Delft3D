#pragma once

#include <format>
#include <optional>
#include <string>
#include <vector>

namespace dflowfm_io
{

    /// @brief Severity level of a reported issue.
    enum class Severity
    {
        Info,
        Warning,
        Error
    };

    /// @brief A single diagnostic issue.
    struct Issue
    {
        Severity severity;             ///< Severity level of the issue.
        std::string message;           ///< Human-readable description of the issue.
        std::optional<int> lineNumber; ///< Line number in the source file where the issue occurred, if known.
    };

    /// @brief Collects diagnostic issues produced during parsing or validation.
    ///
    /// Issues are stored sorted by line number. Issues without a line number are placed before
    /// those with one. Multiple issues at the same line number are ordered by insertion order.
    class IssueReport
    {
    public:
        /// @brief Adds an error issue without a line number.
        /// @tparam Args Types of the format arguments.
        /// @param fmt A std::format-compatible format string.
        /// @param args Arguments to substitute into the format string.
        template <typename... Args>
        void AddError(std::format_string<Args...> fmt, Args&&... args)
        {
            AddIssue(Severity::Error, std::nullopt, std::format(fmt, std::forward<Args>(args)...));
        }

        /// @brief Adds a warning issue without a line number.
        /// @tparam Args Types of the format arguments.
        /// @param fmt A std::format-compatible format string.
        /// @param args Arguments to substitute into the format string.
        template <typename... Args>
        void AddWarning(std::format_string<Args...> fmt, Args&&... args)
        {
            AddIssue(Severity::Warning, std::nullopt, std::format(fmt, std::forward<Args>(args)...));
        }

        /// @brief Adds an informational issue without a line number.
        /// @tparam Args Types of the format arguments.
        /// @param fmt A std::format-compatible format string.
        /// @param args Arguments to substitute into the format string.
        template <typename... Args>
        void AddInfo(std::format_string<Args...> fmt, Args&&... args)
        {
            AddIssue(Severity::Info, std::nullopt, std::format(fmt, std::forward<Args>(args)...));
        }

        /// @brief Adds an error issue associated with a specific source line.
        /// @tparam Args Types of the format arguments.
        /// @param lineNumber 1-based line number in the source file.
        /// @param fmt A std::format-compatible format string.
        /// @param args Arguments to substitute into the format string.
        template <typename... Args>
        void AddError(int lineNumber, std::format_string<Args...> fmt, Args&&... args)
        {
            AddIssue(Severity::Error, lineNumber, std::format(fmt, std::forward<Args>(args)...));
        }

        /// @brief Adds a warning issue associated with a specific source line.
        /// @tparam Args Types of the format arguments.
        /// @param lineNumber 1-based line number in the source file.
        /// @param fmt A std::format-compatible format string.
        /// @param args Arguments to substitute into the format string.
        template <typename... Args>
        void AddWarning(int lineNumber, std::format_string<Args...> fmt, Args&&... args)
        {
            AddIssue(Severity::Warning, lineNumber, std::format(fmt, std::forward<Args>(args)...));
        }

        /// @brief Adds an informational issue associated with a specific source line.
        /// @tparam Args Types of the format arguments.
        /// @param lineNumber 1-based line number in the source file.
        /// @param fmt A std::format-compatible format string.
        /// @param args Arguments to substitute into the format string.
        template <typename... Args>
        void AddInfo(int lineNumber, std::format_string<Args...> fmt, Args&&... args)
        {
            AddIssue(Severity::Info, lineNumber, std::format(fmt, std::forward<Args>(args)...));
        }

        /// @brief Returns true if the report contains at least one informational issue.
        bool HasInfos() const;

        /// @brief Returns true if the report contains at least one warning issue.
        bool HasWarnings() const;

        /// @brief Returns true if the report contains at least one error issue.
        bool HasErrors() const;

        /// @brief Formats all issues into a human-readable multi-line string.
        /// @details Each issue is rendered on its own line as:
        ///          - `"<Severity>: <message>\n"` when no line number is present, or
        ///          - `"<Severity> on line <n>: <message>\n"` when a line number is present.
        ///          Issues are ordered by line number (issues without a line number first).
        /// @return A string containing all formatted issues, or an empty string if there are none.
        std::string Format() const;

        /// @brief Returns true if no issues have been recorded.
        bool empty() const { return issues.empty(); }

        /// @brief Returns the total number of recorded issues.
        std::size_t size() const { return issues.size(); }

        /// @brief Returns an iterator to the first issue.
        std::vector<Issue>::iterator begin() { return issues.begin(); }

        /// @brief Returns a const iterator to the first issue.
        std::vector<Issue>::const_iterator begin() const { return issues.begin(); }

        /// @brief Returns an iterator past the last issue.
        std::vector<Issue>::iterator end() { return issues.end(); }

        /// @brief Returns a const iterator past the last issue.
        std::vector<Issue>::const_iterator end() const { return issues.end(); }

        /// @brief Returns a reference to the issue at the given index.
        /// @param index Zero-based index into the sorted issue list.
        Issue& operator[](std::size_t index) { return issues[index]; }

        /// @brief Returns a const reference to the issue at the given index.
        /// @param index Zero-based index into the sorted issue list.
        const Issue& operator[](std::size_t index) const { return issues[index]; }

    private:
        std::vector<Issue> issues;

        void AddIssue(Severity severity, std::optional<int> lineNumber, std::string message);
        bool HasSeverity(Severity severity) const;
    };

} // namespace dflowfm_io