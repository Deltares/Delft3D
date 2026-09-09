using System.Text.Json.Serialization;

namespace DFlowFM.IO.NET.Generators.Mdu.Schema;

internal sealed class MduSection
{
    [JsonPropertyName("name")]
    public string Name { get; set; } = "";

    [JsonPropertyName("description")]
    public string Description { get; set; } = "";

    [JsonPropertyName("ini_properties")]
    public List<MduProperty> Properties { get; set; } = [];
}