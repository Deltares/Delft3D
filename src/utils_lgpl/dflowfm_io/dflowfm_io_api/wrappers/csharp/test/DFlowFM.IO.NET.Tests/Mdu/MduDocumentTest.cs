using System.Text;

using DFlowFM.IO.Mdu;
using DFlowFM.IO.Reporting;

using NUnit.Framework;

namespace DFlowFM.IO.Tests.Mdu;

// ReSharper disable AccessToDisposedClosure
[TestFixture]
public class MduDocumentTest
{
    [Test]
    public void Dispose_TwiceDoesNotThrow()
    {
        MduDocument document = new();

        document.Dispose();

        Assert.DoesNotThrow(document.Dispose);
    }

    [Test]
    public void Report_BeforeLoad_IsEmpty()
    {
        using MduDocument document = new();

        Assert.That(document.Report, Is.EqualTo(IssueReport.Empty));
    }

    [TestCase(null)]
    [TestCase("")]
    [TestCase("   ")]
    public void LoadFromFile_NullOrWhitespacePath_ThrowsArgumentException(string? path)
    {
        using MduDocument document = new();

        Assert.That(() => document.LoadFromFile(path!), Throws.ArgumentException);
    }

    [Test]
    public void LoadFromString_NullContent_ThrowsArgumentNullException()
    {
        using MduDocument document = new();

        Assert.That(() => document.LoadFromString(null!), Throws.ArgumentNullException);
    }

    [Test]
    public void LoadFromString_ValidContent_DoesNotThrow()
    {
        using MduDocument document = new();

        Assert.DoesNotThrow(() => document.LoadFromString(MduTestFixtures.ValidMduContent));
    }

    [Test]
    public void LoadFromString_ValidContent_ReportHasNoErrors()
    {
        using MduDocument document = CreateWithValidContent();

        Assert.That(document.Report.HasErrors, Is.False);
    }

    [Test]
    public void LoadFromString_InvalidContent_ReportHasErrors()
    {
        using MduDocument document = CreateWithInvalidContent();

        Assert.That(document.Report.HasErrors, Is.True);
    }

    [Test]
    public void LoadFromString_CalledTwice_RefreshesReport()
    {
        using MduDocument document = CreateWithInvalidContent();
        Assert.That(document.Report.HasErrors, Is.True);

        document.LoadFromString(MduTestFixtures.ValidMduContent);

        Assert.That(document.Report.HasErrors, Is.False);
    }

    [Test]
    public void LoadFromStream_NullStream_ThrowsArgumentNullException()
    {
        using MduDocument document = new();

        Assert.That(() => document.LoadFromStream(null!), Throws.ArgumentNullException);
    }

    [Test]
    public void LoadFromStream_ValidContentUnicode_DoesNotThrow()
    {
        using MduDocument document = new();

        byte[] bytes = Encoding.UTF8.GetBytes(MduTestFixtures.ValidMduContentUnicode);
        MemoryStream stream = new(bytes);

        Assert.DoesNotThrow(() => document.LoadFromStream(stream));
    }

    [TestCase(null)]
    [TestCase("")]
    [TestCase("   ")]
    public void SaveToFile_NullOrWhitespacePath_ThrowsArgumentException(string? path)
    {
        using MduDocument document = new();

        Assert.That(() => document.SaveToFile(path!), Throws.ArgumentException);
    }

    [Test]
    public void SaveToString_ContainsExpectedContent()
    {
        using MduDocument document = CreateWithValidContent();

        string content = document.SaveToString();

        Assert.That(content, Does.Contain("[general]"));
        Assert.That(content, Does.Contain("D-Flow FM"));
    }

    [Test]
    public void SaveToString_RoundTripsModifiedValue()
    {
        using MduDocument document = CreateWithValidContent();
        document.SetProperty("general.program", "Round Trip");

        string content = document.SaveToString();

        using MduDocument reloaded = new();
        reloaded.LoadFromString(content);

        Assert.That(reloaded.GetProperty<string>("general.program"), Is.EqualTo("Round Trip"));
    }

    [Test]
    public void SaveToStream_NullStream_ThrowsArgumentNullException()
    {
        using MduDocument document = new();

        Assert.That(() => document.SaveToStream(null!), Throws.ArgumentNullException);
    }

    [Test]
    public void SaveToStream_ContainsExpectedContent()
    {
        using MduDocument document = CreateWithValidContent();
        using MemoryStream stream = new();

        document.SaveToStream(stream);
        string content = Encoding.UTF8.GetString(stream.ToArray());

        Assert.That(content, Does.Contain("[general]"));
        Assert.That(content, Does.Contain("D-Flow FM"));
    }

    [Test]
    public void SaveToStream_UnicodeContent_RoundTrips()
    {
        using MduDocument document = CreateWithValidContentUnicode();
        using MemoryStream stream = new();

        document.SaveToStream(stream);
        stream.Position = 0;

        using MduDocument reloaded = new();
        reloaded.LoadFromStream(stream);

        Assert.That(reloaded.GetProperty<string>("general.program"), Is.EqualTo("D-Flow FM éü中文"));
    }

    [TestCase(null)]
    [TestCase("")]
    [TestCase("   ")]
    public void GetProperty_NullOrWhitespaceKey_ThrowsArgumentException(string? key)
    {
        using MduDocument document = CreateWithValidContent();

        Assert.That(() => document.GetProperty(key!), Throws.ArgumentException);
    }

    [Test]
    public void GetProperty_UnknownKey_ThrowsKeyNotFoundException()
    {
        using MduDocument document = CreateWithValidContent();

        Assert.That(() => document.GetProperty("unknown.key"), Throws.TypeOf<KeyNotFoundException>());
    }

    [Test]
    public void GetProperty_IntValueType_ReturnsInt()
    {
        using MduDocument document = CreateWithValidContent();

        Assert.That(document.GetProperty("geometry.kmx"), Is.EqualTo(3));
    }

    [Test]
    public void GetProperty_DoubleValueType_ReturnsDouble()
    {
        using MduDocument document = CreateWithValidContent();

        Assert.That(document.GetProperty("numerics.cflmax"), Is.EqualTo(1.5));
    }

    [Test]
    public void GetProperty_BoolValueType_ReturnsBool()
    {
        using MduDocument document = CreateWithValidContent();

        Assert.That(document.GetProperty("geometry.usecaching"), Is.True);
    }

    [Test]
    public void GetProperty_StringValueType_ReturnsString()
    {
        using MduDocument document = CreateWithValidContent();

        Assert.That(document.GetProperty("general.program"), Is.EqualTo("D-Flow FM"));
    }

    [Test]
    public void GetProperty_StringValueTypeUnicode_ReturnsUnicodeString()
    {
        using MduDocument document = CreateWithValidContentUnicode();

        Assert.That(document.GetProperty("general.program"), Is.EqualTo("D-Flow FM éü中文"));
    }

    [Test]
    public void GetProperty_PathValueType_ReturnsString()
    {
        using MduDocument document = CreateWithValidContent();

        Assert.That(document.GetProperty("geometry.netfile"), Is.EqualTo("f34_net.nc"));
    }

    [Test]
    public void GetProperty_PathValueTypeUnicode_ReturnsUnicodeString()
    {
        using MduDocument document = CreateWithValidContentUnicode();

        Assert.That(document.GetProperty("geometry.netfile"), Is.EqualTo("réseau/données_éü中文.nc"));
    }

    [Test]
    public void GetProperty_DateTimeValueType_ReturnsDateTime()
    {
        using MduDocument document = CreateWithValidContent();

        Assert.That(document.GetProperty("time.refdate"),
            Is.EqualTo(new DateTime(2026, 1, 1, 0, 0, 0, DateTimeKind.Utc)));
    }

    [Test]
    public void GetProperty_EnumValueType_ReturnsInt()
    {
        using MduDocument document = CreateWithValidContent();

        Assert.That(document.GetProperty("time.tunit"), Is.EqualTo(1));
    }

    [Test]
    public void GetProperty_DoubleListValueType_ReturnsDoubleList()
    {
        using MduDocument document = CreateWithValidContent();

        IEnumerable<double> result = (IEnumerable<double>)document.GetProperty("output.hisinterval");

        Assert.That(result, Is.EqualTo([300.0, 500.0]));
    }

    [Test]
    public void GetProperty_PathListValueType_ReturnsPathList()
    {
        using MduDocument document = CreateWithValidContent();

        IEnumerable<string> result = (IEnumerable<string>)document.GetProperty("geometry.thindamfile");

        Assert.That(result, Is.EqualTo(["thd1.pli", "thd2.pli", "thd3.pli"]));
    }

    [Test]
    public void GetPropertyT_IntValueType_ReturnsInt()
    {
        using MduDocument document = CreateWithValidContent();

        int result = document.GetProperty<int>("geometry.kmx");

        Assert.That(result, Is.EqualTo(3));
    }

    [Test]
    public void GetPropertyT_DoubleValueType_ReturnsDouble()
    {
        using MduDocument document = CreateWithValidContent();

        double result = document.GetProperty<double>("numerics.cflmax");

        Assert.That(result, Is.EqualTo(1.5));
    }

    [Test]
    public void GetPropertyT_WrongValueType_ThrowsInvalidCastException()
    {
        using MduDocument document = CreateWithValidContent();

        Assert.That(() => document.GetProperty<string>("geometry.kmx"), Throws.TypeOf<InvalidCastException>());
    }

    [TestCase(null)]
    [TestCase("")]
    [TestCase("   ")]
    public void SetProperty_NullOrWhitespaceKey_ThrowsArgumentException(string? key)
    {
        using MduDocument document = CreateWithValidContent();

        Assert.That(() => document.SetProperty(key!, 1), Throws.ArgumentException);
    }

    [Test]
    public void SetProperty_UnknownKey_ThrowsKeyNotFoundException()
    {
        using MduDocument document = CreateWithValidContent();

        Assert.That(() => document.SetProperty("unknown.key", 1), Throws.TypeOf<KeyNotFoundException>());
    }

    [Test]
    public void SetProperty_IntValueType_UpdatesValue()
    {
        using MduDocument document = CreateWithValidContent();

        document.SetProperty("geometry.kmx", 5);

        Assert.That(document.GetProperty<int>("geometry.kmx"), Is.EqualTo(5));
    }

    [Test]
    public void SetProperty_DoubleValueType_UpdatesValue()
    {
        using MduDocument document = CreateWithValidContent();

        document.SetProperty("numerics.cflmax", 0.9);

        Assert.That(document.GetProperty<double>("numerics.cflmax"), Is.EqualTo(0.9));
    }

    [Test]
    public void SetProperty_BoolValueType_UpdatesValue()
    {
        using MduDocument document = CreateWithValidContent();

        document.SetProperty("geometry.usecaching", false);

        Assert.That(document.GetProperty<bool>("geometry.usecaching"), Is.False);
    }

    [Test]
    public void SetProperty_StringValueType_UpdatesValue()
    {
        using MduDocument document = CreateWithValidContent();

        document.SetProperty("general.program", "My Program");

        Assert.That(document.GetProperty<string>("general.program"), Is.EqualTo("My Program"));
    }

    [Test]
    public void SetProperty_DateTimeValueType_UpdatesValue()
    {
        using MduDocument document = CreateWithValidContent();
        DateTime newDateTime = new(2025, 6, 11, 8, 30, 22, DateTimeKind.Utc);

        document.SetProperty("time.refdate", newDateTime);

        Assert.That(document.GetProperty<DateTime>("time.refdate"), Is.EqualTo(newDateTime));
    }

    [Test]
    public void SetProperty_EnumValueType_UpdatesValue()
    {
        using MduDocument document = CreateWithValidContent();

        document.SetProperty("time.tunit", 3);

        Assert.That(document.GetProperty<int>("time.tunit"), Is.EqualTo(3));
    }

    [Test]
    public void SetProperty_DoubleListValueType_UpdatesValue()
    {
        using MduDocument document = CreateWithValidContent();
        double[] newValues = [100.0, 200.0, 300.0];

        document.SetProperty("output.hisinterval", newValues);

        IEnumerable<double> result = (IEnumerable<double>)document.GetProperty("output.hisinterval");
        Assert.That(result, Is.EqualTo(newValues));
    }

    [Test]
    public void SetProperty_PathListValueType_UpdatesValue()
    {
        using MduDocument document = CreateWithValidContent();
        string[] newPaths = ["a.pol", "b.xyz", "c.nc"];

        document.SetProperty("geometry.thindamfile", newPaths);

        IEnumerable<string> result = (IEnumerable<string>)document.GetProperty("geometry.thindamfile");
        Assert.That(result, Is.EqualTo(newPaths));
    }

    [Test]
    public void SetProperty_WrongValueType_ThrowsInvalidCastException()
    {
        using MduDocument document = CreateWithValidContent();

        Assert.That(() => document.SetProperty("geometry.kmx", "not-an-int"), Throws.TypeOf<InvalidCastException>());
    }

    [Test]
    public void Indexer_Get_UnknownKey_ThrowsKeyNotFoundException()
    {
        using MduDocument document = CreateWithValidContent();

        Assert.That(() => _ = document["unknown.key"], Throws.TypeOf<KeyNotFoundException>());
    }

    [Test]
    public void Indexer_Set_UnknownKey_ThrowsKeyNotFoundException()
    {
        using MduDocument document = CreateWithValidContent();

        Assert.That(() => document["unknown.key"] = 1, Throws.TypeOf<KeyNotFoundException>());
    }

    [Test]
    public void Indexer_Get_ReturnsSameAsGetProperty()
    {
        using MduDocument document = CreateWithValidContent();

        Assert.That(document["geometry.kmx"], Is.EqualTo(document.GetProperty("geometry.kmx")));
    }

    [Test]
    public void Indexer_Set_UpdatesValue()
    {
        using MduDocument document = CreateWithValidContent();

        document["geometry.kmx"] = 7;

        Assert.That(document.GetProperty<int>("geometry.kmx"), Is.EqualTo(7));
    }

    [TestCase(null)]
    [TestCase("")]
    [TestCase("   ")]
    public void GetPropertySchema_NullOrWhitespaceKey_ThrowsArgumentException(string? key)
    {
        Assert.That(() => MduDocument.GetPropertySchema(key!), Throws.ArgumentException);
    }

    [Test]
    public void GetPropertySchema_UnknownKey_ThrowsKeyNotFoundException()
    {
        Assert.That(() => MduDocument.GetPropertySchema("unknown.key"), Throws.TypeOf<KeyNotFoundException>());
    }

    [Test]
    public void GetPropertySchema_KnownKey_ReturnsSchema()
    {
        MduPropertySchema schema = MduDocument.GetPropertySchema("geometry.netfile");

        Assert.That(schema.FullyQualifiedKey, Is.EqualTo("geometry.netfile"));
    }

    [Test]
    public void GetPropertySchema_IsCaseInsensitive()
    {
        MduPropertySchema schema = MduDocument.GetPropertySchema("GEOMETRY.NETFILE");

        Assert.That(schema.FullyQualifiedKey, Is.EqualTo("geometry.netfile"));
    }

    [Test]
    public void MultipleInstances_AreIndependent()
    {
        using MduDocument document1 = CreateWithValidContent();
        using MduDocument document2 = CreateWithValidContent();

        document1.SetProperty("general.program", "Modified");

        using (Assert.EnterMultipleScope())
        {
            Assert.That(document1.GetProperty<string>("general.program"), Is.EqualTo("Modified"));
            Assert.That(document2.GetProperty<string>("general.program"), Is.EqualTo("D-Flow FM"));
        }
    }

    private static MduDocument CreateWithValidContent()
    {
        return CreateWithContent(MduTestFixtures.ValidMduContent);
    }

    private static MduDocument CreateWithValidContentUnicode()
    {
        return CreateWithContent(MduTestFixtures.ValidMduContentUnicode);
    }

    private static MduDocument CreateWithInvalidContent()
    {
        return CreateWithContent(MduTestFixtures.InvalidMduContent);
    }

    private static MduDocument CreateWithContent(string content)
    {
        MduDocument document = new();
        document.LoadFromString(content);
        return document;
    }
}