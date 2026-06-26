namespace DFlowFM.IO.Reporting;

/// <summary>
/// A report of <see cref="Issue"/> instances produced during validation or conversion of a model file.
/// </summary>
/// <param name="issues">The issues to include in the report.</param>
public sealed class IssueReport(IReadOnlyList<Issue> issues)
{
    /// <summary>
    /// Gets an empty report with no issues.
    /// </summary>
    public static IssueReport Empty { get; } = new([]);

    /// <summary>
    /// Gets all issues in the report.
    /// </summary>
    public IReadOnlyList<Issue> Issues { get; } = issues;

    /// <summary>
    /// Gets a value indicating whether the report contains any issues.
    /// </summary>
    public bool HasIssues => Issues.Count > 0;

    /// <summary>
    /// Gets a value indicating whether the report contains any errors.
    /// </summary>
    public bool HasErrors => Errors.Any();

    /// <summary>
    /// Gets a value indicating whether the report contains any warnings.
    /// </summary>
    public bool HasWarnings => Warnings.Any();

    /// <summary>
    /// Gets all issues with <see cref="IssueSeverity.Error"/> severity.
    /// </summary>
    public IEnumerable<Issue> Errors => IssuesOf(IssueSeverity.Error);

    /// <summary>
    /// Gets all issues with <see cref="IssueSeverity.Warning"/> severity.
    /// </summary>
    public IEnumerable<Issue> Warnings => IssuesOf(IssueSeverity.Warning);

    /// <summary>
    /// Gets all issues with <see cref="IssueSeverity.Info"/> severity.
    /// </summary>
    public IEnumerable<Issue> Infos => IssuesOf(IssueSeverity.Info);

    /// <inheritdoc/>
    public override string ToString() =>
        HasIssues
            ? string.Join(Environment.NewLine, Issues)
            : "No issues.";

    private IEnumerable<Issue> IssuesOf(IssueSeverity severity) =>
        Issues.Where(i => i.Severity == severity);
}