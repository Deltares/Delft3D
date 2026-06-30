namespace DFlowFM.IO.Mdu;

/// <summary>
/// Describes a section and the properties it contains.
/// </summary>
public sealed class MduSectionSchema
{
    /// <summary>
    /// Section name (e.g. "geometry").
    /// </summary>
    public required string Name { get; init; }

    /// <summary>
    /// Human-readable description.
    /// </summary>
    public required string Description { get; init; }

    /// <summary>
    /// The properties contained in the section, in declaration order.
    /// </summary>
    public required IReadOnlyList<MduPropertySchema> Properties { get; init; }
}