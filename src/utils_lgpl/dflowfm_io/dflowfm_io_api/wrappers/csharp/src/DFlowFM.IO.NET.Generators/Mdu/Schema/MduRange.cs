using System.Text.Json.Serialization;

using DFlowFM.IO.NET.Generators.Mdu.Json;

namespace DFlowFM.IO.NET.Generators.Mdu.Schema;

internal sealed class MduRange
{
    [JsonPropertyName("min_inclusive")]
    [JsonConverter(typeof(JsonValueToStringConverter))]
    public string? MinInclusive { get; set; }

    [JsonPropertyName("max_inclusive")]
    [JsonConverter(typeof(JsonValueToStringConverter))]
    public string? MaxInclusive { get; set; }

    [JsonPropertyName("min_exclusive")]
    [JsonConverter(typeof(JsonValueToStringConverter))]
    public string? MinExclusive { get; set; }

    [JsonPropertyName("max_exclusive")]
    [JsonConverter(typeof(JsonValueToStringConverter))]
    public string? MaxExclusive { get; set; }
}