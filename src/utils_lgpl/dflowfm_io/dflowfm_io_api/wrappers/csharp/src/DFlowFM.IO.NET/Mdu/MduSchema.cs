namespace DFlowFM.IO.Mdu;

/// <summary>
/// The complete generated schema for an MDU file.
/// </summary>
public static partial class MduSchema
{
    /// <summary>
    /// Initializes the <see cref="MduSchema" /> class.
    /// </summary>
    static MduSchema()
    {
        Sections = _sections;
        AllProperties = Sections.SelectMany(section => section.Properties).ToList();
        PropertiesByKey = AllProperties.ToDictionary(property => property.FullyQualifiedKey);
    }

    /// <summary>
    /// All sections, in declaration order.
    /// </summary>
    public static IReadOnlyList<MduSectionSchema> Sections { get; }

    /// <summary>
    /// All known properties, in declaration order.
    /// </summary>
    public static IReadOnlyList<MduPropertySchema> AllProperties { get; }

    /// <summary>
    /// Property schema lookup by fully-qualified key (e.g. "geometry.netfile").
    /// </summary>
    public static IReadOnlyDictionary<string, MduPropertySchema> PropertiesByKey { get; }

    /// <summary>
    /// Tries to find a property schema by its fully-qualified key.
    /// </summary>
    /// <param name="fullyQualifiedKey">
    /// The fully-qualified property key (e.g. "geometry.netfile").
    /// Lookup is case-insensitive. A null, empty, or whitespace key yields <c>false</c>.
    /// </param>
    /// <param name="schema">The matching property schema, if found; otherwise, <c>null</c>.</param>
    /// <returns><c>true</c> if a matching property exists; otherwise, <c>false</c>.</returns>
    public static bool TryGetProperty(string? fullyQualifiedKey, out MduPropertySchema? schema)
    {
        if (!string.IsNullOrWhiteSpace(fullyQualifiedKey))
        {
            return PropertiesByKey.TryGetValue(NormalizeKey(fullyQualifiedKey!), out schema);
        }

        schema = null;
        return false;
    }

    private static string NormalizeKey(string fullyQualifiedKey)
    {
        return fullyQualifiedKey.ToLowerInvariant();
    }
}