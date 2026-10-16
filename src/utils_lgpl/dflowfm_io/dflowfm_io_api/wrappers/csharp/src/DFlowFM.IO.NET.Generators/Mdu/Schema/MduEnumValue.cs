using System.Text.Json.Serialization;

using DFlowFM.IO.NET.Generators.Mdu.Json;

namespace DFlowFM.IO.NET.Generators.Mdu.Schema;

[JsonConverter(typeof(MduEnumValueConverter))]
internal sealed class MduEnumValue
{
    public string Description { get; set; } = "";

    public string? Status { get; set; }
}