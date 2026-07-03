namespace DFlowFM.IO.Mdu;

/// <summary>
/// The lifecycle status of an MDU property.
/// </summary>
public enum MduPropertyStatus
{
    /// <summary>
    /// Generally available and supported.
    /// </summary>
    Available,

    /// <summary>
    /// Research-only; not intended for general use.
    /// </summary>
    Research,

    /// <summary>
    /// Deprecated; may be removed in a future version.
    /// </summary>
    Deprecated
}