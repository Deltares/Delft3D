using DFlowFM.IO.Mdu;
using DFlowFM.IO.Reporting;
using NUnit.Framework;

namespace DFlowFM.IO.Tests.Mdu;

// ReSharper disable AccessToDisposedClosure
// ReSharper disable ConvertClosureToMethodGroup
[TestFixture]
public class MduDocumentApiTest
{
    [Test]
    public void Dispose_TwiceDoesNotThrow()
    {
        var api = new MduDocumentApi();

        api.Dispose();

        Assert.DoesNotThrow(() => api.Dispose());
    }

    [Test]
    public void LoadFromFile_NonExistentPath_ThrowsInvalidOperationException()
    {
        using var api = new MduDocumentApi();

        Assert.Throws<InvalidOperationException>(() => api.LoadFromFile("nonexistent_file.mdu"));
    }

    [Test]
    public void LoadFromString_ValidContent_DoesNotThrow()
    {
        using var api = new MduDocumentApi();

        Assert.DoesNotThrow(() => api.LoadFromString(MduTestFixtures.ValidMduContent));
    }

    [Test]
    public void SaveToString_ContainsExpectedContent()
    {
        using MduDocumentApi api = CreateWithValidContent();

        string content = api.SaveToString();

        IEnumerable<string> actualLines = NormalizeMduLines(content);
        IEnumerable<string> expectedLines = NormalizeMduLines(MduTestFixtures.ValidMduContent);

        Assert.That(expectedLines, Is.SubsetOf(actualLines).IgnoreCase);
    }

    [Test]
    public void GetInt_KnownKey_ReturnsExpectedValue()
    {
        using MduDocumentApi api = CreateWithValidContent();

        int result = api.GetInt("geometry.kmx");

        Assert.That(result, Is.EqualTo(3));
    }

    [Test]
    public void GetInt_UnknownKey_ThrowsInvalidOperationException()
    {
        using MduDocumentApi api = CreateWithValidContent();

        Assert.That(() => api.GetInt("unknown.key"), Throws.TypeOf<InvalidOperationException>());
    }

    [Test]
    public void SetInt_KnownKey_UpdatesValue()
    {
        using MduDocumentApi api = CreateWithValidContent();

        api.SetInt("geometry.kmx", 5);

        Assert.That(api.GetInt("geometry.kmx"), Is.EqualTo(5));
    }

    [Test]
    public void SetInt_UnknownKey_ThrowsInvalidOperationException()
    {
        using MduDocumentApi api = CreateWithValidContent();

        Assert.That(() => api.SetInt("nonexisting.key", 42), Throws.TypeOf<InvalidOperationException>());
    }

    [Test]
    public void GetDouble_KnownKey_ReturnsExpectedValue()
    {
        using MduDocumentApi api = CreateWithValidContent();

        double result = api.GetDouble("numerics.cflmax");

        Assert.That(result, Is.EqualTo(1.5));
    }

    [Test]
    public void GetDouble_UnknownKey_ThrowsInvalidOperationException()
    {
        using MduDocumentApi api = CreateWithValidContent();

        Assert.That(() => api.GetDouble("unknown.key"), Throws.TypeOf<InvalidOperationException>());
    }

    [Test]
    public void SetDouble_KnownKey_UpdatesValue()
    {
        using MduDocumentApi api = CreateWithValidContent();

        api.SetDouble("numerics.cflmax", 0.9);

        Assert.That(api.GetDouble("numerics.cflmax"), Is.EqualTo(0.9));
    }

    [Test]
    public void SetDouble_UnknownKey_ThrowsInvalidOperationException()
    {
        using MduDocumentApi api = CreateWithValidContent();

        Assert.That(() => api.SetDouble("nonexisting.key", 3.14), Throws.TypeOf<InvalidOperationException>());
    }

    [Test]
    public void GetBool_KnownKey_ReturnsExpectedValue()
    {
        using MduDocumentApi api = CreateWithValidContent();

        Assert.That(api.GetBool("geometry.usecaching"), Is.True);
    }

    [Test]
    public void GetBool_UnknownKey_ThrowsInvalidOperationException()
    {
        using MduDocumentApi api = CreateWithValidContent();

        Assert.That(() => api.GetBool("unknown.key"), Throws.TypeOf<InvalidOperationException>());
    }

    [Test]
    public void SetBool_KnownKey_UpdatesValue()
    {
        using MduDocumentApi api = CreateWithValidContent();

        api.SetBool("geometry.usecaching", false);

        Assert.That(api.GetBool("geometry.usecaching"), Is.False);
    }

    [Test]
    public void SetBool_UnknownKey_ThrowsInvalidOperationException()
    {
        using MduDocumentApi api = CreateWithValidContent();

        Assert.That(() => api.SetBool("nonexisting.key", true), Throws.TypeOf<InvalidOperationException>());
    }

    [Test]
    public void GetString_KnownKey_ReturnsExpectedValue()
    {
        using MduDocumentApi api = CreateWithValidContent();

        Assert.That(api.GetString("general.program"), Is.EqualTo("D-Flow FM"));
    }

    [Test]
    public void GetString_UnknownKey_ThrowsInvalidOperationException()
    {
        using MduDocumentApi api = CreateWithValidContent();

        Assert.That(() => api.GetString("unknown.key"), Throws.TypeOf<InvalidOperationException>());
    }

    [Test]
    public void SetString_KnownKey_UpdatesValue()
    {
        using MduDocumentApi api = CreateWithValidContent();

        api.SetString("general.program", "My Program");

        Assert.That(api.GetString("general.program"), Is.EqualTo("My Program"));
    }

    [Test]
    public void SetString_UnknownKey_ThrowsInvalidOperationException()
    {
        using MduDocumentApi api = CreateWithValidContent();

        Assert.That(() => api.SetString("nonexisting.key", "hello"), Throws.TypeOf<InvalidOperationException>());
    }

    [Test]
    public void GetPath_KnownKey_ReturnsExpectedValue()
    {
        using MduDocumentApi api = CreateWithValidContent();

        string result = api.GetPath("geometry.netfile");

        Assert.That(result, Is.EqualTo("f34_net.nc"));
    }

    [Test]
    public void GetPath_UnknownKey_ThrowsInvalidOperationException()
    {
        using MduDocumentApi api = CreateWithValidContent();

        Assert.That(() => api.GetPath("unknown.key"), Throws.TypeOf<InvalidOperationException>());
    }

    [Test]
    public void SetPath_KnownKey_UpdatesValue()
    {
        using MduDocumentApi api = CreateWithValidContent();

        api.SetPath("geometry.netfile", "new_net.nc");

        Assert.That(api.GetPath("geometry.netfile"), Is.EqualTo("new_net.nc"));
    }

    [Test]
    public void SetPath_UnknownKey_ThrowsInvalidOperationException()
    {
        using MduDocumentApi api = CreateWithValidContent();

        Assert.That(() => api.SetPath("nonexisting.key", "some/path.nc"), Throws.TypeOf<InvalidOperationException>());
    }

    [Test]
    public void GetPathList_KnownKey_ReturnsExpectedValues()
    {
        using MduDocumentApi api = CreateWithValidContent();

        List<string> result = api.GetPathList("geometry.thindamfile").ToList();

        Assert.That(result, Is.EqualTo(["thd1.pli", "thd2.pli", "thd3.pli"]));
    }

    [Test]
    public void GetPathList_UnknownKey_ThrowsInvalidOperationException()
    {
        using MduDocumentApi api = CreateWithValidContent();

        Assert.That(() => api.GetPathList("unknown.key").ToList(), Throws.TypeOf<InvalidOperationException>());
    }

    [Test]
    public void SetPathList_KnownKey_UpdatesValue()
    {
        using MduDocumentApi api = CreateWithValidContent();

        var newPaths = new[] { "a.pol", "b.xyz", "c.nc" };
        api.SetPathList("geometry.thindamfile", newPaths);

        List<string> result = api.GetPathList("geometry.thindamfile").ToList();

        Assert.That(result, Is.EqualTo(newPaths));
    }

    [Test]
    public void SetPathList_UnknownKey_ThrowsInvalidOperationException()
    {
        using MduDocumentApi api = CreateWithValidContent();

        Assert.That(() => api.SetPathList("nonexisting.key", ["a.pol"]), Throws.TypeOf<InvalidOperationException>());
    }

    [Test]
    public void GetDoubleList_KnownKey_ReturnsExpectedValues()
    {
        using MduDocumentApi api = CreateWithValidContent();

        List<double> result = api.GetDoubleList("output.hisinterval").ToList();

        Assert.That(result, Is.EqualTo([300.0, 500.0]));
    }

    [Test]
    public void GetDoubleList_UnknownKey_ThrowsInvalidOperationException()
    {
        using MduDocumentApi api = CreateWithValidContent();

        Assert.That(() => api.GetDoubleList("unknown.key").ToList(), Throws.TypeOf<InvalidOperationException>());
    }

    [Test]
    public void SetDoubleList_KnownKey_UpdatesValue()
    {
        using MduDocumentApi api = CreateWithValidContent();

        var newValues = new[] { 100.0, 200.0, 300.0 };
        api.SetDoubleList("output.hisinterval", newValues);

        List<double> result = api.GetDoubleList("output.hisinterval").ToList();

        Assert.That(result, Is.EqualTo(newValues));
    }

    [Test]
    public void SetDoubleList_EmptyList_ResultsInEmptyList()
    {
        using MduDocumentApi api = CreateWithValidContent();

        api.SetDoubleList("output.hisinterval", []);

        Assert.That(api.GetDoubleList("output.hisinterval").ToList(), Is.Empty);
    }

    [Test]
    public void SetDoubleList_SingleValue_UpdatesValue()
    {
        using MduDocumentApi api = CreateWithValidContent();

        api.SetDoubleList("output.hisinterval", [200.0]);

        List<double> result = api.GetDoubleList("output.hisinterval").ToList();

        Assert.That(result, Is.EqualTo([200.0]));
    }

    [Test]
    public void SetDoubleList_UnknownKey_ThrowsInvalidOperationException()
    {
        using MduDocumentApi api = CreateWithValidContent();

        Assert.That(() => api.SetDoubleList("nonexisting.key", [1.0, 2.0]), Throws.TypeOf<InvalidOperationException>());
    }

    [Test]
    public void GetEnum_StringEnumKnownKey_ReturnsExpectedValue()
    {
        using MduDocumentApi api = CreateWithValidContent();

        Assert.That(api.GetEnum("time.tunit"), Is.EqualTo(1));
    }

    [Test]
    public void GetEnum_IntEnumKnownKey_ReturnsExpectedValue()
    {
        using MduDocumentApi api = CreateWithValidContent();

        Assert.That(api.GetEnum("numerics.timesteptype"), Is.EqualTo(3));
    }

    [Test]
    public void GetEnum_UnknownKey_ThrowsInvalidOperationException()
    {
        using MduDocumentApi api = CreateWithValidContent();

        Assert.That(() => api.GetEnum("unknown.key"), Throws.TypeOf<InvalidOperationException>());
    }

    [Test]
    public void SetEnum_StringEnumKnownKey_UpdatesValue()
    {
        using MduDocumentApi api = CreateWithValidContent();

        api.SetEnum("time.tunit", 3);

        Assert.That(api.GetEnum("time.tunit"), Is.EqualTo(3));
    }

    [Test]
    public void SetEnum_IntEnumKnownKey_UpdatesValue()
    {
        using MduDocumentApi api = CreateWithValidContent();

        api.SetEnum("numerics.timesteptype", 2);

        Assert.That(api.GetEnum("numerics.timesteptype"), Is.EqualTo(2));
    }

    [Test]
    public void SetEnum_OutOfRange_ThrowsInvalidOperationException()
    {
        using MduDocumentApi api = CreateWithValidContent();

        Assert.That(() => api.SetEnum("numerics.timesteptype", -1), Throws.TypeOf<InvalidOperationException>());
    }

    [Test]
    public void SetEnum_UnknownKey_ThrowsInvalidOperationException()
    {
        using MduDocumentApi api = CreateWithValidContent();

        Assert.That(() => api.SetEnum("nonexisting.key", 0), Throws.TypeOf<InvalidOperationException>());
    }

    [Test]
    public void GetDateTime_KnownKey_ReturnsExpectedValue()
    {
        using MduDocumentApi api = CreateWithValidContent();

        DateTime result = api.GetDateTime("time.refdate");

        Assert.That(result, Is.EqualTo(new DateTime(2026, 1, 1, 0, 0, 0, DateTimeKind.Utc)));
    }

    [Test]
    public void GetDateTime_UnknownKey_ThrowsInvalidOperationException()
    {
        using MduDocumentApi api = CreateWithValidContent();

        Assert.That(() => api.GetDateTime("nonexisting.key"), Throws.TypeOf<InvalidOperationException>());
    }

    [Test]
    public void SetDateTime_KnownKey_UpdatesValue()
    {
        using MduDocumentApi api = CreateWithValidContent();

        var newDateTime = new DateTime(2025, 6, 11, 8, 30, 22, DateTimeKind.Utc);
        api.SetDateTime("time.refdate", newDateTime);

        Assert.That(api.GetDateTime("time.refdate"), Is.EqualTo(newDateTime));
    }

    [Test]
    public void SetDateTime_UnknownKey_ThrowsInvalidOperationException()
    {
        using MduDocumentApi api = CreateWithValidContent();

        Assert.That(() => api.SetDateTime("nonexisting.key", DateTime.UtcNow), Throws.TypeOf<InvalidOperationException>());
    }

    [Test]
    public void GetIssueReport_ReturnsIssueReport()
    {
        using MduDocumentApi api = CreateWithValidContent();

        IssueReport report = api.GetIssueReport();

        Assert.That(report, Is.Not.Null);
        Assert.That(report.Issues, Is.Not.Null);
    }

    [Test]
    public void GetIssueReport_ValidFile_HasNoErrors()
    {
        using MduDocumentApi api = CreateWithValidContent();

        IssueReport report = api.GetIssueReport();

        Assert.That(report.HasErrors, Is.False);
    }

    [Test]
    public void GetIssueReport_InvalidFile_HasErrors()
    {
        using MduDocumentApi api = CreateWithInvalidContent();

        IssueReport report = api.GetIssueReport();

        Assert.That(report.HasErrors, Is.True);
    }

    [Test]
    public void MultipleInstances_AreIndependent()
    {
        using MduDocumentApi api1 = CreateWithValidContent();
        using MduDocumentApi api2 = CreateWithValidContent();

        using (Assert.EnterMultipleScope())
        {
            Assert.That(api1.GetString("general.program"), Is.EqualTo("D-Flow FM"));
            Assert.That(api2.GetString("general.program"), Is.EqualTo("D-Flow FM"));
        }

        api1.SetString("general.program", "Modified");
        using (Assert.EnterMultipleScope())
        {
            Assert.That(api1.GetString("general.program"), Is.EqualTo("Modified"));
            Assert.That(api2.GetString("general.program"), Is.EqualTo("D-Flow FM"));
        }
    }

    private static MduDocumentApi CreateWithValidContent()
        => CreateWithContent(MduTestFixtures.ValidMduContent);

    private static MduDocumentApi CreateWithInvalidContent()
        => CreateWithContent(MduTestFixtures.InvalidMduContent);

    private static MduDocumentApi CreateWithContent(string content)
    {
        var api = new MduDocumentApi();
        api.LoadFromString(content);
        return api;
    }

    private static IEnumerable<string> NormalizeMduLines(string content) =>
        content.Split('\n')
               .Select(line => line.Contains('#') ? line[..line.IndexOf('#')] : line)
               .Select(line => line.Trim())
               .Select(line => FormatMduLine(line));

    private static string FormatMduLine(string line)
    {
        int equalsIndex = line.IndexOf('=');
        if (equalsIndex < 0)
        {
            return line;
        }

        string key = line[..equalsIndex].Trim();
        string value = line[(equalsIndex + 1)..].Trim();

        return $"{key} = {value}";
    }
}