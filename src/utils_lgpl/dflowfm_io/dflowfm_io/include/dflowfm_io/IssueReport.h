#pragma once

#include <format>
#include <optional>
#include <span>
#include <string>
#include <utility>
#include <vector>

namespace dflowfm_io
{

    /// @brief Severity level of a reported issue.
    enum class Severity
    {
        Debug,
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
        /// @brief Adds an issue without a line number.
        /// @tparam Level Severity level of the issue.
        /// @tparam Args Types of the format arguments.
        /// @param fmt A std::format-compatible format string.
        /// @param args Arguments to substitute into the format string.
        template <Severity Level, typename... Args>
        void Add(std::format_string<Args...> fmt, Args&&... args)
        {
            AddIssue(Level, std::nullopt, std::format(fmt, std::forward<Args>(args)...));
        }

        /// @brief Adds an issue associated with a specific source line.
        /// @tparam Level Severity level of the issue.
        /// @tparam Args Types of the format arguments.
        /// @param lineNumber 1-based line number in the source file.
        /// @param fmt A std::format-compatible format string.
        /// @param args Arguments to substitute into the format string.
        template <Severity Level, typename... Args>
        void Add(int lineNumber, std::format_string<Args...> fmt, Args&&... args)
        {
            AddIssue(Level, lineNumber, std::format(fmt, std::forward<Args>(args)...));
        }

        /// @brief Formats all issues into a human-readable multi-line string.
        /// @details Each issue is rendered on its own line as:
        ///          - `"<Severity>: <message>\n"` when no line number is present, or
        ///          - `"<Severity> on line <n>: <message>\n"` when a line number is present.
        ///          Issues are ordered by line number (issues without a line number first).
        /// @param minSeverity Only issues with a severity greater than or equal to this value are
        ///                    included. Defaults to @ref Severity::Debug (includes all issues).
        /// @return A string containing all formatted issues, or an empty string if there are none.
        std::string Format(Severity minSeverity = Severity::Debug) const;

        /// @brief Returns the recorded issues in sorted order.
        std::span<const Issue> GetIssues() const;

    private:
        std::vector<Issue> issues;

        void AddIssue(Severity severity, std::optional<int> lineNumber, std::string message);
    };

} // namespace dflowfm_io