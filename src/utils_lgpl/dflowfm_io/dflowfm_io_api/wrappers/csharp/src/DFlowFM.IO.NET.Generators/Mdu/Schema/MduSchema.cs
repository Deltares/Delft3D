using System.Text.Json.Serialization;

namespace DFlowFM.IO.NET.Generators.Mdu.Schema;

internal sealed class MduSchema
{
    [JsonPropertyName("ini_sections")]
    public List<MduSection> Sections { get; set; } = [];
}