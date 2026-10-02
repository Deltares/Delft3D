#pragma once

#include <format>
#include <optional>
#include <span>
#include <string>
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

        /// @brief Adds an debug issue without a line number.
        /// @tparam Args Types of the format arguments.
        /// @param fmt A std::format-compatible format string.
        /// @param args Arguments to substitute into the format string.
        template <typename... Args>
        void AddDebug(std::format_string<Args...> fmt, Args&&... args)
        {
            AddIssue(Severity::Debug, std::nullopt, std::format(fmt, std::forward<Args>(args)...));
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

        /// @brief Adds an debug issue associated with a specific source line.
        /// @tparam Args Types of the format arguments.
        /// @param lineNumber 1-based line number in the source file.
        /// @param fmt A std::format-compatible format string.
        /// @param args Arguments to substitute into the format string.
        template <typename... Args>
        void AddDebug(int lineNumber, std::format_string<Args...> fmt, Args&&... args)
        {
            AddIssue(Severity::Debug, lineNumber, std::format(fmt, std::forward<Args>(args)...));
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