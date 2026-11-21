using System.Text.Json;
using System.Text.Json.Serialization;

using DFlowFM.IO.NET.Generators.Mdu.Schema;

namespace DFlowFM.IO.NET.Generators.Mdu.Json;

internal sealed class MduEnumValueConverter : JsonConverter<MduEnumValue>
{
    public override MduEnumValue Read(ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options)
    {
        if (reader.TokenType == JsonTokenType.String)
        {
            return new MduEnumValue { Description = reader.GetString() ?? "" };
        }

        using JsonDocument doc = JsonDocument.ParseValue(ref reader);
        JsonElement root = doc.RootElement;

        MduEnumValue result = new();

        if (root.TryGetProperty("description", out JsonElement description))
        {
            result.Description = description.GetString() ?? "";
        }

        if (root.TryGetProperty("status", out JsonElement status))
        {
            result.Status = status.TryGetProperty("value", out JsonElement statusValue)
                ? statusValue.GetString()
                : status.GetString();
        }

        return result;
    }

    public override void Write(Utf8JsonWriter writer, MduEnumValue value, JsonSerializerOptions options)
    {
        throw new NotSupportedException();
    }
}