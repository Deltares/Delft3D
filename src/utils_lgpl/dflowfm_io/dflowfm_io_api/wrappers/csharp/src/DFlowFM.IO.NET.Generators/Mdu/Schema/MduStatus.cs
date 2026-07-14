using System.Text.Json.Serialization;

namespace DFlowFM.IO.NET.Generators.Mdu.Schema;

internal sealed class MduStatus
{
    [JsonPropertyName("value")]
    public string Value { get; set; } = "GA";
}