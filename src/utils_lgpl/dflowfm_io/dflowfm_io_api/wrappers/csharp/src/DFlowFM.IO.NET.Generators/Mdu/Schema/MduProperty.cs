using System.Text.Json.Serialization;

using DFlowFM.IO.NET.Generators.Mdu.Json;

namespace DFlowFM.IO.NET.Generators.Mdu.Schema;

internal sealed class MduProperty
{
    [JsonPropertyName("key")]
    public string Key { get; set; } = "";

    [JsonPropertyName("description")]
    public string Description { get; set; } = "";

    [JsonPropertyName("default_value")]
    [JsonConverter(typeof(JsonValueToStringConverter))]
    public string? DefaultValue { get; set; }

    [JsonPropertyName("value_type")]
    public string ValueType { get; set; } = "";

    [JsonPropertyName("unit")]
    public string? Unit { get; set; }

    [JsonPropertyName("status")]
    public MduStatus? Status { get; set; }

    [JsonPropertyName("validation")]
    public MduValidation? Validation { get; set; }

    [JsonPropertyName("enum_values")]
    public Dictionary<string, string> EnumValues { get; set; } = [];
}