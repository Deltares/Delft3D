namespace DFlowFM.IO.Mdu;

/// <summary>
/// A single allowed value of an enum property.
/// </summary>
public sealed class MduEnumValue
{
    /// <summary>
    /// The integer representation: the raw value for integer-keyed enums, or the
    /// zero-based declaration index for string-keyed enums.
    /// </summary>
    public required int IntValue { get; init; }

    /// <summary>
    /// The string representation: the raw value for string-keyed enums, or the raw
    /// integer rendered as text for integer-keyed enums.
    /// </summary>
    public required string StringValue { get; init; }

    /// <summary>
    /// Human-readable description of this value.
    /// </summary>
    public required string Description { get; init; }
}