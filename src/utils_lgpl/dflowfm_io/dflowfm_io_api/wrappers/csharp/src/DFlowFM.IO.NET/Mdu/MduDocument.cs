using DFlowFM.IO.Reporting;

namespace DFlowFM.IO.Mdu;

/// <summary>
/// Represents a D-Flow FM Model Definition Unstructured (MDU) file.
/// </summary>
public sealed partial class MduDocument : IDisposable
{
    private readonly MduApi api = new MduApi();

    /// <summary>
    /// Gets the issue report produced after the last load operation.
    /// </summary>
    public IssueReport Report { get; private set; } = IssueReport.Empty;

    /// <summary>
    /// Loads the MDU document from a file on disk.
    /// </summary>
    /// <param name="path">The path to the MDU file to load.</param>
    public void LoadFromFile(string path)
    {
        api.LoadFromFile(path);
        Report = api.GetIssueReport();
    }

    /// <summary>
    /// Loads the MDU document from a string containing the file contents.
    /// </summary>
    /// <param name="content">The MDU file contents as a string.</param>
    public void LoadFromString(string content)
    {
        api.LoadFromString(content);
        Report = api.GetIssueReport();
    }

    /// <summary>
    /// Saves the MDU document to a file on disk.
    /// </summary>
    /// <param name="path">The path of the file to write.</param>
    public void SaveToFile(string path) => api.SaveToFile(path);

    /// <summary>
    /// Saves the MDU document to a string and returns the contents.
    /// </summary>
    /// <returns>The MDU file contents as a string.</returns>
    public string SaveToString() => api.SaveToString();

    /// <inheritdoc/>
    public void Dispose() => api.Dispose();
}