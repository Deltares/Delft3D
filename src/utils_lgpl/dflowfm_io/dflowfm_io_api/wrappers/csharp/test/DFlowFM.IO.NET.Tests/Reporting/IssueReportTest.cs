using DFlowFM.IO.Reporting;

using NUnit.Framework;

namespace DFlowFM.IO.Tests.Reporting;

[TestFixture]
public class IssueReportTests
{
    [Test]
    public void Empty_HasNoIssues()
    {
        IssueReport report = IssueReport.Empty;

        using (Assert.EnterMultipleScope())
        {
            Assert.That(report.Issues, Is.Empty);
            Assert.That(report.HasIssues, Is.False);
            Assert.That(report.HasErrors, Is.False);
            Assert.That(report.HasWarnings, Is.False);
        }
    }

    [Test]
    public void Empty_HasEmptyFilteredLists()
    {
        IssueReport report = IssueReport.Empty;

        using (Assert.EnterMultipleScope())
        {
            Assert.That(report.ErrorIssues, Is.Empty);
            Assert.That(report.WarningIssues, Is.Empty);
            Assert.That(report.InfoIssues, Is.Empty);
            Assert.That(report.DebugIssues, Is.Empty);
        }
    }

    [Test]
    public void Constructor_WithErrors_SetsHasErrors()
    {
        List<Issue> issues = [new(IssueSeverity.Error, "error message")];

        IssueReport report = new(issues);

        using (Assert.EnterMultipleScope())
        {
            Assert.That(report.HasIssues, Is.True);
            Assert.That(report.HasErrors, Is.True);
            Assert.That(report.HasWarnings, Is.False);
        }
    }

    [Test]
    public void Constructor_WithWarnings_SetsHasWarnings()
    {
        List<Issue> issues = [new(IssueSeverity.Warning, "warning message")];

        IssueReport report = new(issues);

        using (Assert.EnterMultipleScope())
        {
            Assert.That(report.HasIssues, Is.True);
            Assert.That(report.HasErrors, Is.False);
            Assert.That(report.HasWarnings, Is.True);
        }
    }

    [Test]
    public void Constructor_WithInfos_DoesNotSetHasErrorsOrWarnings()
    {
        List<Issue> issues = [new(IssueSeverity.Info, "info message")];

        IssueReport report = new(issues);

        using (Assert.EnterMultipleScope())
        {
            Assert.That(report.HasIssues, Is.True);
            Assert.That(report.HasErrors, Is.False);
            Assert.That(report.HasWarnings, Is.False);
            Assert.That(report.InfoIssues, Has.Count.EqualTo(1));
        }
    }

    [Test]
    public void Constructor_WithDebugs_DoesNotSetHasErrorsOrWarnings()
    {
        List<Issue> issues = [new(IssueSeverity.Debug, "debug message")];

        IssueReport report = new(issues);

        using (Assert.EnterMultipleScope())
        {
            Assert.That(report.HasIssues, Is.True);
            Assert.That(report.HasErrors, Is.False);
            Assert.That(report.HasWarnings, Is.False);
            Assert.That(report.DebugIssues, Has.Count.EqualTo(1));
        }
    }

    [Test]
    public void Constructor_FiltersSeveritiesCorrectly()
    {
        List<Issue> issues =
        [
            new(IssueSeverity.Error, "error 1"),
            new(IssueSeverity.Error, "error 2"),
            new(IssueSeverity.Warning, "warning 1"),
            new(IssueSeverity.Info, "info 1"),
            new(IssueSeverity.Debug, "debug 1")
        ];

        IssueReport report = new(issues);

        using (Assert.EnterMultipleScope())
        {
            Assert.That(report.Issues, Has.Count.EqualTo(5));
            Assert.That(report.ErrorIssues, Has.Count.EqualTo(2));
            Assert.That(report.WarningIssues, Has.Count.EqualTo(1));
            Assert.That(report.InfoIssues, Has.Count.EqualTo(1));
            Assert.That(report.DebugIssues, Has.Count.EqualTo(1));
        }
    }

    [Test]
    public void ToString_WithIssues_ReturnsJoinedMessages()
    {
        List<Issue> issues =
        [
            new(IssueSeverity.Error, "error 1"),
            new(IssueSeverity.Warning, "warning 1"),
            new(IssueSeverity.Info, "info 1"),
            new(IssueSeverity.Debug, "debug 1")
        ];

        IssueReport report = new(issues);
        string result = report.ToString();

        Assert.That(result, Does.Contain("error 1"));
        Assert.That(result, Does.Contain("warning 1"));
        Assert.That(result, Does.Contain("info 1"));
        Assert.That(result, Does.Contain("debug 1"));
    }

    [Test]
    public void ToString_WithNoIssues_ReturnsNoIssuesMessage()
    {
        IssueReport report = IssueReport.Empty;

        Assert.That(report.ToString(), Is.EqualTo("No issues."));
    }

    [Test]
    public void ToString_WithLineNumber_IncludesLineNumberInOutput()
    {
        List<Issue> issues = [new(IssueSeverity.Error, "error message", 42)];

        IssueReport report = new(issues);

        Assert.That(report.ToString(), Does.Contain("42"));
    }
}