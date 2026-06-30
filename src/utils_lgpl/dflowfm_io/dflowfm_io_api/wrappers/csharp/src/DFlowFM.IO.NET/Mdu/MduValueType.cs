namespace DFlowFM.IO.Mdu;

/// <summary>
/// Identifies the data type and access method for an MDU property.
/// </summary>
public enum MduValueType
{
    Int,
    Bool,
    Double,
    String,
    Path,
    DateTime,
    Enum,
    DoubleList,
    StringList,
    PathList
}