using System.Text;
using System.Text.Json;

using DFlowFM.IO.NET.Generators.Mdu.Schema;

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Text;

namespace DFlowFM.IO.NET.Generators.Mdu;

[Generator]
internal sealed class MduSourceGenerator : IIncrementalGenerator
{
    public void Initialize(IncrementalGeneratorInitializationContext context)
    {
        IncrementalValuesProvider<AdditionalText> mduJson =
            context.AdditionalTextsProvider.Where(static f => Path.GetFileName(f.Path) == "mdu.json");

        context.RegisterSourceOutput(mduJson, static (ctx, file) =>
        {
            string json = file.GetText(ctx.CancellationToken)?.ToString()!;
            JsonSerializerOptions options = new() { PropertyNamingPolicy = JsonNamingPolicy.SnakeCaseLower };
            MduSchema schema = JsonSerializer.Deserialize<MduSchema>(json, options)!;

            WriteDocument(ctx, schema);
        });
    }

    private static void WriteDocument(SourceProductionContext ctx, MduSchema schema)
    {
        StringBuilder sb = new();

        AppendHeader(sb);

        // Usings
        sb.AppendLine("using System;");
        sb.AppendLine("using System.Collections.Generic;");
        sb.AppendLine();
        sb.AppendLine("namespace DFlowFM.IO.Mdu;");
        sb.AppendLine();

        // Class
        sb.AppendLine("/// <summary>Represents a D-Flow FM Model Definition Unstructured (MDU) file.</summary>");
        sb.AppendLine("public sealed partial class MduDocument");
        sb.AppendLine("{");

        // Constructor
        sb.AppendLine("    /// <summary>Initializes a new instance of <see cref=\"MduDocument\"/> class.</summary>");
        sb.AppendLine("    public MduDocument()");
        sb.AppendLine("    {");

        foreach (string sectionName in schema.Sections.Select(section => ToPascalCase(section.Name)))
        {
            sb.AppendLine($"        {sectionName} = new {sectionName}Section(_api);");
        }

        sb.AppendLine("    }");
        sb.AppendLine();

        // Properties
        foreach (MduSection section in schema.Sections)
        {
            string sectionName = ToPascalCase(section.Name);
            sb.AppendLine($"    /// <summary>{EscapeXml(section.Description)}</summary>");
            sb.AppendLine($"    public {sectionName}Section {sectionName} {{ get; }}");
            sb.AppendLine();
        }

        // Nested section classes
        foreach (MduSection section in schema.Sections)
        {
            sb.AppendLine();
            WriteSection(sb, section);
        }

        sb.AppendLine("}");

        ctx.AddSource("MduDocument.g.cs", SourceText.From(sb.ToString(), Encoding.UTF8));
    }

    private static void WriteSection(StringBuilder sb, MduSection section)
    {
        string sectionName = ToPascalCase(section.Name);

        sb.AppendLine($"    /// <summary>{EscapeXml(section.Description)}</summary>");
        sb.AppendLine($"    public sealed class {sectionName}Section");
        sb.AppendLine("    {");
        sb.AppendLine("        private readonly MduApi _api;");
        sb.AppendLine();
        sb.AppendLine(
            $"         /// <summary>Initializes a new instance of <see cref=\"{sectionName}Section\"/> class.</summary>");
        sb.AppendLine($"        internal {sectionName}Section(MduApi api) => _api = api;");

        foreach (MduProperty property in section.Properties)
        {
            sb.AppendLine();
            WriteProperty(sb, section, property);
        }

        sb.AppendLine("    }");
    }

    private static void WriteProperty(StringBuilder sb, MduSection section, MduProperty property)
    {
        string propertyName = ToPascalCase(property.Key);
        string key = ToFullyQualifiedKey(section.Name, property.Key);

        if (!string.IsNullOrEmpty(property.Description))
        {
            sb.AppendLine($"        /// <summary>{EscapeXml(property.Description)}</summary>");
        }

        switch (property.ValueType)
        {
            case "string":
                sb.AppendLine($"        public string {propertyName}");
                sb.AppendLine("        {");
                sb.AppendLine($"            get => _api.GetString(\"{key}\");");
                sb.AppendLine($"            set => _api.SetString(\"{key}\", value);");
                sb.AppendLine("        }");
                break;
            case "int":
                sb.AppendLine($"        public int {propertyName}");
                sb.AppendLine("        {");
                sb.AppendLine($"            get => _api.GetInt(\"{key}\");");
                sb.AppendLine($"            set => _api.SetInt(\"{key}\", value);");
                sb.AppendLine("        }");
                break;
            case "float":
                sb.AppendLine($"        public double {propertyName}");
                sb.AppendLine("        {");
                sb.AppendLine($"            get => _api.GetDouble(\"{key}\");");
                sb.AppendLine($"            set => _api.SetDouble(\"{key}\", value);");
                sb.AppendLine("        }");
                break;
            case "intbool":
                sb.AppendLine($"        public bool {propertyName}");
                sb.AppendLine("        {");
                sb.AppendLine($"            get => _api.GetBool(\"{key}\");");
                sb.AppendLine($"            set => _api.SetBool(\"{key}\", value);");
                sb.AppendLine("        }");
                break;
            case "path":
                sb.AppendLine($"        public string {propertyName}");
                sb.AppendLine("        {");
                sb.AppendLine($"            get => _api.GetPath(\"{key}\");");
                sb.AppendLine($"            set => _api.SetPath(\"{key}\", value);");
                sb.AppendLine("        }");
                break;
            case "enum":
            case "intenum":
                sb.AppendLine($"        public int {propertyName}");
                sb.AppendLine("        {");
                sb.AppendLine($"            get => _api.GetEnum(\"{key}\");");
                sb.AppendLine($"            set => _api.SetEnum(\"{key}\", value);");
                sb.AppendLine("        }");
                break;
            case "double_list":
                sb.AppendLine($"        public IEnumerable<double> {propertyName}");
                sb.AppendLine("        {");
                sb.AppendLine($"            get => _api.GetDoubleList(\"{key}\");");
                sb.AppendLine($"            set => _api.SetDoubleList(\"{key}\", value);");
                sb.AppendLine("        }");
                break;
            case "list[path]":
                sb.AppendLine($"        public IEnumerable<string> {propertyName}");
                sb.AppendLine("        {");
                sb.AppendLine($"            get => _api.GetPathList(\"{key}\");");
                sb.AppendLine($"            set => _api.SetPathList(\"{key}\", value);");
                sb.AppendLine("        }");
                break;
            case "list[string]":
                sb.AppendLine($"        public IEnumerable<string> {propertyName}");
                sb.AppendLine("        {");
                sb.AppendLine($"            get => _api.GetStringList(\"{key}\");");
                sb.AppendLine($"            set => _api.SetStringList(\"{key}\", value);");
                sb.AppendLine("        }");
                break;
            case "list[float]":
                sb.AppendLine($"        public IEnumerable<double> {propertyName}");
                sb.AppendLine("        {");
                sb.AppendLine($"            get => _api.GetDoubleList(\"{key}\");");
                sb.AppendLine($"            set => _api.SetDoubleList(\"{key}\", value);");
                sb.AppendLine("        }");
                break;
            case "datetime":
                sb.AppendLine($"        public DateTime {propertyName}");
                sb.AppendLine("        {");
                sb.AppendLine($"            get => _api.GetDateTime(\"{key}\");");
                sb.AppendLine($"            set => _api.SetDateTime(\"{key}\", value);");
                sb.AppendLine("        }");
                break;
        }
    }

    private static void AppendHeader(StringBuilder sb)
    {
        sb.AppendLine("// <auto-generated>");
        sb.AppendLine("//   This file was generated from mdu.json. Do not edit manually.");
        sb.AppendLine("// </auto-generated>");
        sb.AppendLine();
    }

    private static string ToPascalCase(string name)
    {
        string[] parts = name.Split([' ', '_', '-'], StringSplitOptions.RemoveEmptyEntries);
        string result = string.Concat(parts.Select(p => char.ToUpperInvariant(p[0]) + p.Substring(1)));
        return char.IsDigit(result[0]) ? "_" + result : result;
    }

    private static string ToFullyQualifiedKey(string name, string key)
    {
        return $"{name.ToLowerInvariant()}.{key.ToLowerInvariant()}";
    }

    private static string EscapeXml(string text)
    {
        return text.Replace("&", "&amp;")
            .Replace("<", "&lt;")
            .Replace(">", "&gt;")
            .Replace("\"", "&quot;");
    }
}