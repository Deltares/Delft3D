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
    /// Section name (e.g. "geometry").
    /// </summary>
    public required string Section { get; init; }

    /// <summary>
    /// Fully-qualified key in lowercase (e.g. "geometry.netfile") used with <see cref="MduApi" />.
    /// </summary>
    public required string FullyQualifiedKey { get; init; }

    /// <summary>
    /// Human-readable description.
    /// </summary>
    public required string Description { get; init; }

    /// <summary>
    /// The data type and access method.
    /// </summary>
    public required MduValueType ValueType { get; init; }

    /// <summary>
    /// The lifecycle status. Defaults to <see cref="MduPropertyStatus.Available" />.
    /// </summary>
    public required MduPropertyStatus Status { get; init; }

    /// <summary>
    /// Physical unit (e.g. "m/s"), if any.
    /// </summary>
    public string? Unit { get; init; }

    /// <summary>
    /// The default value as a string representation, or <c>null</c> if none.
    /// </summary>
    public string? DefaultValue { get; init; }

    /// <summary>
    /// The minimum allowed value as a string representation, or <c>null</c> if none.
    /// </summary>
    public string? MinValue { get; init; }

    /// <summary>
    /// The maximum allowed value as a string representation, or <c>null</c> if none.
    /// </summary>
    public string? MaxValue { get; init; }

    /// <summary>
    /// Allowed enum values. Empty for non-enum properties.
    /// </summary>
    public IReadOnlyList<MduEnumValue> EnumValues { get; init; } = [];

    /// <inheritdoc />
    public override string ToString()
    {
        return Key;
    }
}