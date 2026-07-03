using System.Text;

using DFlowFM.IO.Reporting;

namespace DFlowFM.IO.Mdu;

/// <summary>
/// Represents a D-Flow FM Model Definition Unstructured (MDU) file.
/// </summary>
public sealed partial class MduDocument : IDisposable
{
    private static readonly UTF8Encoding Utf8NoBom = new(false);
    private readonly MduApi _api = new();

    /// <summary>
    /// Initializes a new instance of <see cref="MduDocument" /> class.
    /// </summary>
    public MduDocument()
    {
        InitializeSections();
    }

    /// <summary>
    /// Gets the issue report produced after the last load operation.
    /// </summary>
    public IssueReport Report { get; private set; } = IssueReport.Empty;

    /// <summary>
    /// Gets or sets a property value by its fully-qualified key (e.g. "geometry.netfile").
    /// </summary>
    /// <param name="key">The fully-qualified property key.</param>
    /// <exception cref="ArgumentException">When <paramref name="key" /> is null or whitespace.</exception>
    /// <exception cref="KeyNotFoundException">When <paramref name="key" /> is not a known property.</exception>
    public object this[string key]
    {
        get => GetProperty(key);
        set => SetProperty(key, value);
    }

    /// <inheritdoc />
    public void Dispose()
    {
        _api.Dispose();
    }

    /// <summary>
    /// Loads the MDU document from a file on disk.
    /// </summary>
    /// <param name="path">The path to the MDU file to load.</param>
    /// <exception cref="ArgumentException">When <paramref name="path" /> is null or whitespace.</exception>
    public void LoadFromFile(string path)
    {
        if (string.IsNullOrWhiteSpace(path))
        {
            throw new ArgumentException("The property path must not be null or whitespace.", nameof(path));
        }

        _api.LoadFromFile(path);
        Report = _api.GetIssueReport();
    }

    /// <summary>
    /// Loads the MDU document from a string containing the file contents.
    /// </summary>
    /// <param name="content">The MDU file contents as a string.</param>
    /// <exception cref="ArgumentNullException">When <paramref name="content" /> is null.</exception>
    public void LoadFromString(string content)
    {
        if (content == null)
        {
            throw new ArgumentNullException(nameof(content));
        }

        _api.LoadFromString(content);
        Report = _api.GetIssueReport();
    }

    /// <summary>
    /// Loads the MDU document from a stream.
    /// </summary>
    /// <param name="stream">The stream containing the MDU file contents.</param>
    /// <exception cref="ArgumentNullException">When <paramref name="stream" /> is null.</exception>
    public void LoadFromStream(Stream stream)
    {
        if (stream == null)
        {
            throw new ArgumentNullException(nameof(stream));
        }

        using StreamReader reader = new(stream, Utf8NoBom, true, 1024, true);
        LoadFromString(reader.ReadToEnd());
    }

    /// <summary>
    /// Saves the MDU document to a file on disk.
    /// </summary>
    /// <param name="path">The path of the file to write.</param>
    /// <exception cref="ArgumentException">When <paramref name="path" /> is null or whitespace.</exception>
    public void SaveToFile(string path)
    {
        if (string.IsNullOrWhiteSpace(path))
        {
            throw new ArgumentException("The property path must not be null or whitespace.", nameof(path));
        }

        _api.SaveToFile(path);
    }

    /// <summary>
    /// Saves the MDU document to a string and returns the contents.
    /// </summary>
    /// <returns>The MDU file contents as a string.</returns>
    public string SaveToString()
    {
        return _api.SaveToString();
    }

    /// <summary>
    /// Saves the MDU document to a stream.
    /// </summary>
    /// <param name="stream">The stream to write the MDU file contents to.</param>
    /// <exception cref="ArgumentNullException">When <paramref name="stream" /> is null.</exception>
    public void SaveToStream(Stream stream)
    {
        if (stream == null)
        {
            throw new ArgumentNullException(nameof(stream));
        }

        byte[] bytes = Utf8NoBom.GetBytes(SaveToString());
        stream.Write(bytes, 0, bytes.Length);
    }

    /// <summary>
    /// Gets a property value by its fully-qualified key (e.g. "geometry.netfile").
    /// </summary>
    /// <param name="key">The fully-qualified property key.</param>
    /// <returns>The property value.</returns>
    /// <exception cref="ArgumentException">When <paramref name="key" /> is null or whitespace.</exception>
    /// <exception cref="KeyNotFoundException">When <paramref name="key" /> is not a known property.</exception>
    public object GetProperty(string key)
    {
        MduPropertySchema schema = GetPropertySchema(key);

        return schema.ValueType switch
        {
            MduValueType.Int => _api.GetInt(key),
            MduValueType.Bool => _api.GetBool(key),
            MduValueType.Double => _api.GetDouble(key),
            MduValueType.String => _api.GetString(key),
            MduValueType.Path => _api.GetPath(key),
            MduValueType.DateTime => _api.GetDateTime(key),
            MduValueType.Enum => _api.GetEnum(key),
            MduValueType.DoubleList => _api.GetDoubleList(key),
            MduValueType.StringList => _api.GetStringList(key),
            MduValueType.PathList => _api.GetPathList(key),
            _ => throw new NotSupportedException(
                $"Value type '{schema.ValueType}' is not supported.")
        };
    }

    /// <summary>
    /// Gets a property value by its fully-qualified key (e.g. "geometry.netfile").
    /// </summary>
    /// <typeparam name="T">The type of the property value.</typeparam>
    /// <param name="key">The fully-qualified property key.</param>
    /// <returns>The property value.</returns>
    /// <exception cref="ArgumentException">When <paramref name="key" /> is null or whitespace.</exception>
    /// <exception cref="KeyNotFoundException">When <paramref name="key" /> is not a known property.</exception>
    public T GetProperty<T>(string key)
    {
        return (T)GetProperty(key);
    }

    /// <summary>
    /// Sets a property value by its fully-qualified key (e.g. "geometry.netfile").
    /// </summary>
    /// <param name="key">The fully-qualified property key.</param>
    /// <param name="value">The value to assign.</param>
    /// <exception cref="ArgumentException">When <paramref name="key" /> is null or whitespace.</exception>
    /// <exception cref="KeyNotFoundException">When <paramref name="key" /> is not a known property.</exception>
    public void SetProperty(string key, object value)
    {
        MduPropertySchema schema = GetPropertySchema(key);

        switch (schema.ValueType)
        {
            case MduValueType.Int: _api.SetInt(key, (int)value); break;
            case MduValueType.Bool: _api.SetBool(key, (bool)value); break;
            case MduValueType.Double: _api.SetDouble(key, (double)value); break;
            case MduValueType.String: _api.SetString(key, (string)value); break;
            case MduValueType.Path: _api.SetPath(key, (string)value); break;
            case MduValueType.DateTime: _api.SetDateTime(key, (DateTime)value); break;
            case MduValueType.Enum: _api.SetEnum(key, (int)value); break;
            case MduValueType.DoubleList: _api.SetDoubleList(key, (IEnumerable<double>)value); break;
            case MduValueType.StringList: _api.SetStringList(key, (IEnumerable<string>)value); break;
            case MduValueType.PathList: _api.SetPathList(key, (IEnumerable<string>)value); break;
            default:
                throw new NotSupportedException(
                    $"Value type '{schema.ValueType}' is not supported.");
        }
    }

    /// <summary>
    /// Gets the schema for the property with the given fully-qualified key.
    /// </summary>
    /// <param name="key">The fully-qualified property key.</param>
    /// <returns>The property schema.</returns>
    /// <exception cref="ArgumentException">When <paramref name="key" /> is null or whitespace.</exception>
    /// <exception cref="KeyNotFoundException">When <paramref name="key" /> is not a known property.</exception>
    public static MduPropertySchema GetPropertySchema(string key)
    {
        if (string.IsNullOrWhiteSpace(key))
        {
            throw new ArgumentException("The property key must not be null or whitespace.", nameof(key));
        }

        if (!MduSchema.TryGetProperty(key, out MduPropertySchema? schema) || schema is null)
        {
            throw new KeyNotFoundException($"Unknown MDU property key '{key}'.");
        }

        return schema;
    }
}