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
            var json = file.GetText(ctx.CancellationToken)?.ToString()!;
            var options = new JsonSerializerOptions { PropertyNamingPolicy = JsonNamingPolicy.SnakeCaseLower };
            var schema = JsonSerializer.Deserialize<MduSchema>(json, options)!;

            WriteDocument(ctx, schema);
        });
    }

    private static void WriteDocument(SourceProductionContext ctx, MduSchema schema)
    {
        var sb = new StringBuilder();

        AppendHeader(sb);

        // Usings
        sb.AppendLine("using System;");
        sb.AppendLine("using System.Collections.Generic;");
        sb.AppendLine("using DFlowFM.IO.Reporting;");
        sb.AppendLine();
        sb.AppendLine("namespace DFlowFM.IO.Mdu;");
        sb.AppendLine();

        // Class
        sb.AppendLine("/// <summary>Represents a D-Flow FM Model Definition Unstructured (MDU) file.</summary>");
        sb.AppendLine("public sealed class MduDocument : IDisposable");
        sb.AppendLine("{");
        sb.AppendLine("    private readonly MduDocumentApi api;");
        sb.AppendLine();

        // Constructor
        sb.AppendLine("    /// <summary>Initializes a new instance of <see cref=\"MduDocument\"/> class.</summary>");
        sb.AppendLine("    public MduDocument()");
        sb.AppendLine("    {");
        sb.AppendLine("        api = new MduDocumentApi();");
        sb.AppendLine("        Report = IssueReport.Empty;");

        foreach (string sectionName in schema.Sections.Select(section => ToPascalCase(section.Name)))
        {
            sb.AppendLine($"        {sectionName} = new {sectionName}Section(api);");
        }

        sb.AppendLine("    }");
        sb.AppendLine();

        // Properties
        sb.AppendLine("    /// <summary>Gets the issue report produced after the last load operation.</summary>");
        sb.AppendLine("    public IssueReport Report { get; private set; }");

        foreach (MduSection section in schema.Sections)
        {
            string sectionName = ToPascalCase(section.Name);
            if (!string.IsNullOrEmpty(section.Description))
            {
                sb.AppendLine($"    /// <summary>{EscapeXml(section.Description)}</summary>");
            }

            sb.AppendLine($"    public {sectionName}Section {sectionName} {{ get; }}");
        }
        sb.AppendLine();

        // Methods
        sb.AppendLine("    /// <summary>Loads the MDU document from a file on disk.</summary>");
        sb.AppendLine("    /// <param name=\"path\">The path to the MDU file to load.</param>");
        sb.AppendLine("    public void LoadFromFile(string path)");
        sb.AppendLine("    {");
        sb.AppendLine("        api.LoadFromFile(path);");
        sb.AppendLine("        Report = api.GetIssueReport();");
        sb.AppendLine("    }");
        sb.AppendLine();
        sb.AppendLine("    /// <summary>Loads the MDU document from a string containing the file contents.</summary>");
        sb.AppendLine("    /// <param name=\"content\">The MDU file contents as a string.</param>");
        sb.AppendLine("    public void LoadFromString(string content)");
        sb.AppendLine("    {");
        sb.AppendLine("        api.LoadFromString(content);");
        sb.AppendLine("        Report = api.GetIssueReport();");
        sb.AppendLine("    }");
        sb.AppendLine();
        sb.AppendLine("    /// <summary>Saves the MDU document to a file on disk.</summary>");
        sb.AppendLine("    /// <param name=\"path\">The path of the file to write.</param>");
        sb.AppendLine("    public void SaveToFile(string path) => api.SaveToFile(path);");
        sb.AppendLine();
        sb.AppendLine("    /// <summary>Saves the MDU document to a string and returns the contents.</summary>");
        sb.AppendLine("    /// <returns>The MDU file contents as a string.</returns>");
        sb.AppendLine("    public string SaveToString() => api.SaveToString();");
        sb.AppendLine();
        sb.AppendLine("    /// <inheritdoc/>");
        sb.AppendLine("    public void Dispose() => api.Dispose();");

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

        if (!string.IsNullOrEmpty(section.Description))
        {
            sb.AppendLine($"    /// <summary>{EscapeXml(section.Description)}</summary>");
        }

        sb.AppendLine($"    public sealed class {sectionName}Section");
        sb.AppendLine("    {");
        sb.AppendLine("        private readonly MduDocumentApi api;");
        sb.AppendLine();
        sb.AppendLine($"        internal {sectionName}Section(MduDocumentApi api) => this.api = api;");

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
                sb.AppendLine($"            get => api.GetString(\"{key}\");");
                sb.AppendLine($"            set => api.SetString(\"{key}\", value);");
                sb.AppendLine("        }");
                break;
            case "int":
                sb.AppendLine($"        public int {propertyName}");
                sb.AppendLine("        {");
                sb.AppendLine($"            get => api.GetInt(\"{key}\");");
                sb.AppendLine($"            set => api.SetInt(\"{key}\", value);");
                sb.AppendLine("        }");
                break;
            case "float":
                sb.AppendLine($"        public double {propertyName}");
                sb.AppendLine("        {");
                sb.AppendLine($"            get => api.GetDouble(\"{key}\");");
                sb.AppendLine($"            set => api.SetDouble(\"{key}\", value);");
                sb.AppendLine("        }");
                break;
            case "intbool":
                sb.AppendLine($"        public bool {propertyName}");
                sb.AppendLine("        {");
                sb.AppendLine($"            get => api.GetBool(\"{key}\");");
                sb.AppendLine($"            set => api.SetBool(\"{key}\", value);");
                sb.AppendLine("        }");
                break;
            case "path":
                sb.AppendLine($"        public string {propertyName}");
                sb.AppendLine("        {");
                sb.AppendLine($"            get => api.GetPath(\"{key}\");");
                sb.AppendLine($"            set => api.SetPath(\"{key}\", value);");
                sb.AppendLine("        }");
                break;
            case "enum":
            case "intenum":
                sb.AppendLine($"        public int {propertyName}");
                sb.AppendLine("        {");
                sb.AppendLine($"            get => api.GetEnum(\"{key}\");");
                sb.AppendLine($"            set => api.SetEnum(\"{key}\", value);");
                sb.AppendLine("        }");
                break;
            case "double_list":
                sb.AppendLine($"        public IEnumerable<double> {propertyName}");
                sb.AppendLine("        {");
                sb.AppendLine($"            get => api.GetDoubleList(\"{key}\");");
                sb.AppendLine($"            set => api.SetDoubleList(\"{key}\", value);");
                sb.AppendLine("        }");
                break;
            case "list[path]":
                sb.AppendLine($"        public IEnumerable<string> {propertyName}");
                sb.AppendLine("        {");
                sb.AppendLine($"            get => api.GetPathList(\"{key}\");");
                sb.AppendLine($"            set => api.SetPathList(\"{key}\", value);");
                sb.AppendLine("        }");
                break;
            case "list[string]":
                sb.AppendLine($"        public IEnumerable<string> {propertyName}");
                sb.AppendLine("        {");
                sb.AppendLine($"            get => api.GetStringList(\"{key}\");");
                sb.AppendLine($"            set => api.SetStringList(\"{key}\", value);");
                sb.AppendLine("        }");
                break;
            case "list[float]":
                sb.AppendLine($"        public IEnumerable<double> {propertyName}");
                sb.AppendLine("        {");
                sb.AppendLine($"            get => api.GetDoubleList(\"{key}\");");
                sb.AppendLine($"            set => api.SetDoubleList(\"{key}\", value);");
                sb.AppendLine("        }");
                break;
            case "datetime":
                sb.AppendLine($"        public DateTime {propertyName}");
                sb.AppendLine("        {");
                sb.AppendLine($"            get => api.GetDateTime(\"{key}\");");
                sb.AppendLine($"            set => api.SetDateTime(\"{key}\", value);");
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

    private static string ToFullyQualifiedKey(string name, string key) =>
        $"{name.ToLowerInvariant()}.{key.ToLowerInvariant()}";

    private static string EscapeXml(string text) =>
        text.Replace("&", "&amp;")
            .Replace("<", "&lt;")
            .Replace(">", "&gt;")
            .Replace("\"", "&quot;");
}