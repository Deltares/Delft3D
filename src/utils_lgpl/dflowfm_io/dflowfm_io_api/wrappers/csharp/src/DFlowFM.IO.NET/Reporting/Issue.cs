namespace DFlowFM.IO.Reporting;

/// <summary>
/// Represents a single issue found during the validation or conversion of a model file.
/// </summary>
/// <param name="severity">The severity level of the issue.</param>
/// <param name="message">A description of the issue.</param>
/// <param name="lineNumber">The line number in the source file where the issue was found, if available.</param>
public sealed class Issue(IssueSeverity severity, string message, int? lineNumber = null)
{
    /// <summary>
    /// Gets the severity level of the issue.
    /// </summary>
    public IssueSeverity Severity { get; } = severity;

    /// <summary>
    /// Gets a description of the issue.
    /// </summary>
    public string Message { get; } = message;

    /// <summary>
    /// Gets the line number in the source file where the issue was found,
    /// or <see langword="null" /> if not available.
    /// </summary>
    public int? LineNumber { get; } = lineNumber;

    /// <inheritdoc />
    public override string ToString()
    {
        return LineNumber is { } line
            ? $"[{Severity}] - Line {line}: {Message}"
            : $"[{Severity}] - {Message}";
    }
}