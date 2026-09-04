using System.Text.Json.Serialization;

namespace DFlowFM.IO.NET.Generators.Mdu.Schema;

internal sealed class MduValidation
{
    [JsonPropertyName("range")]
    public MduRange? Range { get; set; }
}