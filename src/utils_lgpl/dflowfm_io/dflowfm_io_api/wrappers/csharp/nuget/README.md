# DFlowFM.IO.NET

A .NET library for reading, writing, validating, and migrating D-Flow FM model files.

## Features

- **Read** files with built-in automatic migration to the latest file version.
- **Write** files with configurable formatting, comment preservation, and structure preservation.
- **Validate** files with clear, consistent, and detailed messages.
- **Create** new files from scratch, initialized with sensible defaults.
- **Edit** strongly typed data models, even when the resulting model is invalid.

## Getting started

```csharp
using DFlowFM.IO.Mdu;

using var document = new MduDocument();
document.LoadFromFile("model.mdu");

// Read a property by its fully-qualified, case-insensitive key.
string? netFile = document.GetProperty<string>("geometry.netfile");

// Set a property.
document.SetProperty("geometry.bedlevuni", -5.0);

// Inspect load issues.
foreach (var issue in document.Report.Issues)
{
    Console.WriteLine(issue);
}

document.SaveToFile("model.mdu");
```

## Platform support

- `.NET Standard 2.0`