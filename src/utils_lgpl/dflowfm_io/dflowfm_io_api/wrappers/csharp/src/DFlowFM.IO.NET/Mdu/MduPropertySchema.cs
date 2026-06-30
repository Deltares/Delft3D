namespace DFlowFM.IO.Mdu;

/// <summary>
/// Describes a single MDU property: its key, type, and metadata.
/// </summary>
public sealed class MduPropertySchema
{
    /// <summary>
    /// Property key within the section (e.g. "netFile").
    /// </summary>
    public required string Key { get; init; }

    /// <summary>
    /// Fully-qualified key in lowercase (e.g. "geometry.netfile") used with <see cref="MduApi" />.
    /// </summary>
    public required string FullyQualifiedKey { get; init; }

    /// <summary>
    /// Section name (e.g. "geometry").
    /// </summary>
    public required string Section { get; init; }

    /// <summary>
    /// The data type and access method.
    /// </summary>
    public required MduValueType ValueType { get; init; }

    /// <summary>
    /// Human-readable description.
    /// </summary>
    public required string Description { get; init; }

    /// <summary>
    /// Physical unit (e.g. "m/s"), if any.
    /// </summary>
    public string? Unit { get; init; }
}