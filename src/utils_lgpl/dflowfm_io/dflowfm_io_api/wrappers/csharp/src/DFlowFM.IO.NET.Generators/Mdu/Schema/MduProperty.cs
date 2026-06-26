using System.Text.Json.Serialization;

namespace DFlowFM.IO.NET.Generators.Mdu.Schema;

internal sealed class MduProperty
{
    [JsonPropertyName("key")]
    public string Key { get; set; } = "";

    [JsonPropertyName("description")]
    public string Description { get; set; } = "";

    [JsonPropertyName("value_type")]
    public string ValueType { get; set; } = "";

    [JsonPropertyName("enum_values")]
    public Dictionary<string, string> EnumValues { get; set; } = new();
}