namespace DFlowFM.IO.Reporting;

/// <summary>
/// A report of <see cref="Issue" /> instances produced during validation or conversion of a model file.
/// </summary>
public sealed class IssueReport
{
    /// <summary>
    /// Initializes a new instance of <see cref="IssueReport" />.
    /// </summary>
    /// <param name="issues">The issues to include in the report.</param>
    public IssueReport(IReadOnlyList<Issue> issues)
    {
        Issues = issues;

        ILookup<IssueSeverity, Issue> lookup = issues.ToLookup(i => i.Severity);
        Errors = lookup[IssueSeverity.Error].ToArray();
        Warnings = lookup[IssueSeverity.Warning].ToArray();
        Infos = lookup[IssueSeverity.Info].ToArray();

        HasIssues = Issues.Count > 0;
        HasErrors = Errors.Count > 0;
        HasWarnings = Warnings.Count > 0;
    }

    /// <summary>
    /// Gets an empty report with no issues.
    /// </summary>
    public static IssueReport Empty { get; } = new([]);

    /// <summary>
    /// Gets all issues in the report.
    /// </summary>
    public IReadOnlyList<Issue> Issues { get; }

    /// <summary>
    /// Gets a value indicating whether the report contains any issues.
    /// </summary>
    public bool HasIssues { get; }

    /// <summary>
    /// Gets a value indicating whether the report contains any errors.
    /// </summary>
    public bool HasErrors { get; }

    /// <summary>
    /// Gets a value indicating whether the report contains any warnings.
    /// </summary>
    public bool HasWarnings { get; }

    /// <summary>
    /// Gets all issues with <see cref="IssueSeverity.Error" /> severity.
    /// </summary>
    public IReadOnlyList<Issue> Errors { get; }

    /// <summary>
    /// Gets all issues with <see cref="IssueSeverity.Warning" /> severity.
    /// </summary>
    public IReadOnlyList<Issue> Warnings { get; }

    /// <summary>
    /// Gets all issues with <see cref="IssueSeverity.Info" /> severity.
    /// </summary>
    public IReadOnlyList<Issue> Infos { get; }

    /// <inheritdoc />
    public override string ToString()
    {
        return HasIssues
            ? string.Join(Environment.NewLine, Issues)
            : "No issues.";
    }
}