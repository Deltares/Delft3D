using System.Text.Json;
using System.Text.Json.Serialization;

namespace DFlowFM.IO.NET.Generators.Mdu.Json;

internal sealed class JsonValueToStringConverter : JsonConverter<string?>
{
    public override string? Read(ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options)
    {
        using JsonDocument doc = JsonDocument.ParseValue(ref reader);
        JsonElement root = doc.RootElement;

        return root.ValueKind switch
        {
            JsonValueKind.Null => null,
            JsonValueKind.String => root.GetString(),
            JsonValueKind.Array => string.Join(" ", root.EnumerateArray().Select(e => e.ToString())),
            _ => root.ToString()
        };
    }

    public override void Write(Utf8JsonWriter writer, string? value, JsonSerializerOptions options)
    {
        throw new NotSupportedException();
    }
}