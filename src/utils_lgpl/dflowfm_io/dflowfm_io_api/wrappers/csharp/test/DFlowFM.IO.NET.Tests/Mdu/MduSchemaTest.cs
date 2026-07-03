using DFlowFM.IO.Mdu;

using NUnit.Framework;

namespace DFlowFM.IO.Tests.Mdu;

[TestFixture]
public sealed class MduSchemaTest
{
    [Test]
    public void Sections_IsNotEmpty()
    {
        Assert.That(MduSchema.Sections, Is.Not.Empty);
    }

    [Test]
    public void AllProperties_IsNotEmpty()
    {
        Assert.That(MduSchema.AllProperties, Is.Not.Empty);
    }

    [Test]
    public void PropertiesByKey_IsNotEmpty()
    {
        Assert.That(MduSchema.PropertiesByKey, Is.Not.Empty);
    }

    [Test]
    public void AllProperties_EqualsFlattenedSectionProperties()
    {
        IEnumerable<MduPropertySchema> expected =
            MduSchema.Sections.SelectMany(section => section.Properties);

        Assert.That(MduSchema.AllProperties, Is.EqualTo(expected));
    }

    [Test]
    public void PropertiesByKey_ContainsEveryProperty()
    {
        IEnumerable<string> expectedKeys =
            MduSchema.AllProperties.Select(property => property.FullyQualifiedKey);

        Assert.That(MduSchema.PropertiesByKey.Keys, Is.EqualTo(expectedKeys));
    }

    [Test]
    public void PropertiesByKey_IsKeyedByFullyQualifiedKey()
    {
        Assert.That(
            MduSchema.PropertiesByKey,
            Has.All.Matches<KeyValuePair<string, MduPropertySchema>>(entry =>
                entry.Key == entry.Value.FullyQualifiedKey));
    }

    [Test]
    public void FullyQualifiedKeys_AreUnique()
    {
        IEnumerable<string> keys =
            MduSchema.AllProperties.Select(property => property.FullyQualifiedKey);

        Assert.That(keys, Is.Unique);
    }

    [Test]
    public void FullyQualifiedKey_CombinesSectionAndKey()
    {
        Assert.That(
            MduSchema.AllProperties,
            Has.All.Matches<MduPropertySchema>(property =>
                property.FullyQualifiedKey.Equals($"{property.Section}.{property.Key}",
                    StringComparison.InvariantCultureIgnoreCase)));
    }

    [Test]
    public void EverySection_HasProperties()
    {
        Assert.That(MduSchema.Sections,
            Has.All.Matches<MduSectionSchema>(section =>
                !string.IsNullOrEmpty(section.Name) && section.Properties.Any()));
    }

    [Test]
    public void SectionNames_AreUnique()
    {
        IEnumerable<string> names =
            MduSchema.Sections.Select(section => section.Name);

        Assert.That(names, Is.Unique);
    }

    [Test]
    public void EveryProperty_HasRequiredMetadata()
    {
        Assert.That(MduSchema.AllProperties,
            Has.All.Matches<MduPropertySchema>(property =>
                !string.IsNullOrEmpty(property.Key)
                && !string.IsNullOrEmpty(property.FullyQualifiedKey)
                && !string.IsNullOrEmpty(property.Section)));
    }

    [Test]
    public void EveryEnumProperty_HasEnumValues()
    {
        Assert.That(
            MduSchema.AllProperties.Where(p => p.ValueType is MduValueType.Enum),
            Has.All.Matches<MduPropertySchema>(p => p.EnumValues.Any()));
    }

    [Test]
    public void EveryNonEnumProperty_HasNoEnumValues()
    {
        Assert.That(
            MduSchema.AllProperties.Where(p => p.ValueType is not MduValueType.Enum),
            Has.All.Matches<MduPropertySchema>(p => !p.EnumValues.Any()));
    }

    [Test]
    public void EveryPropertySection_MatchesItsOwningSection()
    {
        IEnumerable<(string Section, MduPropertySchema Property)> sectionProperties =
            MduSchema.Sections.SelectMany(section => section.Properties
                .Select(property => (Section: section.Name, Property: property)));

        Assert.That(sectionProperties,
            Has.All.Matches<(string Section, MduPropertySchema Property)>(pair =>
                pair.Property.Section == pair.Section));
    }

    [Test]
    public void TryGetProperty_WithKnownKey_ReturnsTrueAndSchema()
    {
        MduPropertySchema propertySchema = MduSchema.AllProperties[0];

        bool found = MduSchema.TryGetProperty(propertySchema.FullyQualifiedKey, out MduPropertySchema? actual);

        using (Assert.EnterMultipleScope())
        {
            Assert.That(found, Is.True);
            Assert.That(actual, Is.SameAs(propertySchema));
        }
    }

    [Test]
    public void TryGetProperty_IsCaseInsensitive()
    {
        MduPropertySchema propertySchema = MduSchema.AllProperties[0];
        string uppercaseKey = propertySchema.FullyQualifiedKey.ToUpperInvariant();

        bool found = MduSchema.TryGetProperty(uppercaseKey, out MduPropertySchema? actual);

        using (Assert.EnterMultipleScope())
        {
            Assert.That(found, Is.True);
            Assert.That(actual, Is.SameAs(propertySchema));
        }
    }

    [Test]
    public void TryGetProperty_WithUnknownKey_ReturnsFalseAndNull()
    {
        bool found = MduSchema.TryGetProperty("does.notexist", out MduPropertySchema? schema);

        using (Assert.EnterMultipleScope())
        {
            Assert.That(found, Is.False);
            Assert.That(schema, Is.Null);
        }
    }

    [TestCase(null)]
    [TestCase("")]
    [TestCase("   ")]
    public void TryGetProperty_WithNullEmptyOrWhitespaceKey_ReturnsFalse(string? key)
    {
        bool found = MduSchema.TryGetProperty(key, out MduPropertySchema? schema);

        using (Assert.EnterMultipleScope())
        {
            Assert.That(found, Is.False);
            Assert.That(schema, Is.Null);
        }
    }
}