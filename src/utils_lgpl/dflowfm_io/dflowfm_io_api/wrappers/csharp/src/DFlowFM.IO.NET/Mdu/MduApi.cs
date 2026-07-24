using DFlowFM.IO.Native;
using DFlowFM.IO.Reporting;

namespace DFlowFM.IO.Mdu;

/// <summary>
/// Provides low-level read and write access to D-Flow FM Model Definition Unstructured (MDU) files
/// via the native <c>dflowfm_io</c> library.
/// </summary>
/// <remarks>
/// Properties are identified by a fully-qualified, case-insensitive key in the format
/// <c>"&lt;section&gt;.&lt;property&gt;"</c>, e.g. <c>"geometry.netfile"</c> refers to
/// the <c>netFile</c> property in the <c>[geometry]</c> section.
/// </remarks>
internal sealed class MduApi : IDisposable
{
    private readonly SafeMduHandle _handle = new();

    /// <inheritdoc />
    public void Dispose()
    {
        _handle.Dispose();
    }

    /// <summary>
    /// Loads an MDU document from a file on disk.
    /// </summary>
    /// <param name="path">The path to the MDU file to load.</param>
    /// <exception cref="InvalidOperationException">When the file could not be loaded.</exception>
    public void LoadFromFile(string path)
    {
        byte[] pathBytes = NativeInterop.StringToUtf8(path);
        ThrowIfError(NativeMduApi.mdu_load_from_file(_handle, pathBytes));
    }

    /// <summary>
    /// Loads an MDU document from a string containing the file contents.
    /// </summary>
    /// <param name="content">The MDU file contents as a string.</param>
    /// <exception cref="InvalidOperationException">When the content could not be loaded.</exception>
    public void LoadFromString(string content)
    {
        byte[] contentBytes = NativeInterop.StringToUtf8(content);
        ThrowIfError(NativeMduApi.mdu_load_from_string(_handle, contentBytes, (ulong)contentBytes.Length));
    }

    /// <summary>
    /// Saves the MDU document to a file on disk.
    /// </summary>
    /// <param name="path">The path of the file to write.</param>
    /// <exception cref="InvalidOperationException">When the file could not be saved.</exception>
    public void SaveToFile(string path)
    {
        byte[] pathBytes = NativeInterop.StringToUtf8(path);
        ThrowIfError(NativeMduApi.mdu_save_to_file(_handle, pathBytes));
    }

    /// <summary>
    /// Saves the MDU document to a string and returns the contents.
    /// </summary>
    /// <returns>The MDU file contents as a string.</returns>
    /// <exception cref="InvalidOperationException">When the document could not be saved.</exception>
    public string SaveToString()
    {
        ThrowIfError(NativeMduApi.mdu_save_to_string(_handle, out IntPtr ptr));
        return NativeInterop.PtrToStringUtf8(ptr);
    }

    /// <summary>
    /// Gets an integer property value by its fully-qualified key.
    /// </summary>
    /// <param name="key">The fully-qualified property key.</param>
    /// <returns>The integer value of the property.</returns>
    /// <exception cref="InvalidOperationException">When the property could not be retrieved.</exception>
    public int GetInt(string key)
    {
        byte[] keyBytes = NativeInterop.StringToUtf8(key);
        ThrowIfError(NativeMduApi.mdu_get_int(_handle, keyBytes, out int value));
        return value;
    }

    /// <summary>
    /// Gets a boolean property value by its fully-qualified key.
    /// </summary>
    /// <param name="key">The fully-qualified property key.</param>
    /// <returns>The boolean value of the property.</returns>
    /// <exception cref="InvalidOperationException">When the property could not be retrieved.</exception>
    public bool GetBool(string key)
    {
        byte[] keyBytes = NativeInterop.StringToUtf8(key);
        ThrowIfError(NativeMduApi.mdu_get_bool(_handle, keyBytes, out int value));
        return value != 0;
    }

    /// <summary>
    /// Gets a double-precision floating-point property value by its fully-qualified key.
    /// </summary>
    /// <param name="key">The fully-qualified property key.</param>
    /// <returns>The double value of the property.</returns>
    /// <exception cref="InvalidOperationException">When the property could not be retrieved.</exception>
    public double GetDouble(string key)
    {
        byte[] keyBytes = NativeInterop.StringToUtf8(key);
        ThrowIfError(NativeMduApi.mdu_get_double(_handle, keyBytes, out double value));
        return value;
    }

    /// <summary>
    /// Gets a string property value by its fully-qualified key.
    /// </summary>
    /// <param name="key">The fully-qualified property key.</param>
    /// <returns>The string value of the property.</returns>
    /// <exception cref="InvalidOperationException">When the property could not be retrieved.</exception>
    public string GetString(string key)
    {
        byte[] keyBytes = NativeInterop.StringToUtf8(key);
        ThrowIfError(NativeMduApi.mdu_get_string(_handle, keyBytes, out IntPtr ptr));
        return NativeInterop.PtrToStringUtf8(ptr);
    }

    /// <summary>
    /// Gets a file path property value by its fully-qualified key.
    /// </summary>
    /// <param name="key">The fully-qualified property key.</param>
    /// <returns>The file path value of the property.</returns>
    /// <exception cref="InvalidOperationException">When the property could not be retrieved.</exception>
    public string GetPath(string key)
    {
        byte[] keyBytes = NativeInterop.StringToUtf8(key);
        ThrowIfError(NativeMduApi.mdu_get_path(_handle, keyBytes, out IntPtr ptr));
        return NativeInterop.PtrToStringUtf8(ptr);
    }

    /// <summary>
    /// Gets a date/time property value by its fully-qualified key.
    /// </summary>
    /// <param name="key">The fully-qualified property key.</param>
    /// <returns>The UTC <see cref="DateTime" /> value of the property.</returns>
    /// <exception cref="InvalidOperationException">When the property could not be retrieved.</exception>
    public DateTime GetDateTime(string key)
    {
        byte[] keyBytes = NativeInterop.StringToUtf8(key);
        ThrowIfError(NativeMduApi.mdu_get_datetime(_handle, keyBytes, out long epochSeconds));
        return DateTimeOffset.FromUnixTimeSeconds(epochSeconds).UtcDateTime;
    }

    /// <summary>
    /// Gets an enumeration property value by its fully-qualified key.
    /// </summary>
    /// <param name="key">The fully-qualified property key.</param>
    /// <returns>The integer representation of the enumeration value.</returns>
    /// <exception cref="InvalidOperationException">When the property could not be retrieved.</exception>
    public int GetEnum(string key)
    {
        byte[] keyBytes = NativeInterop.StringToUtf8(key);
        ThrowIfError(NativeMduApi.mdu_get_enum(_handle, keyBytes, out int value));
        return value;
    }

    public string GetEnumName(string key)
    {
        byte[] keyBytes = NativeInterop.StringToUtf8(key);
        ThrowIfError(NativeMduApi.mdu_get_enum_name(_handle, keyBytes, out IntPtr ptr));
        return NativeInterop.PtrToStringUtf8(ptr);
    }

    /// <summary>
    /// Gets a list of string property values by its fully-qualified key.
    /// </summary>
    /// <param name="key">The fully-qualified property key.</param>
    /// <returns>An enumerable of string values.</returns>
    /// <exception cref="InvalidOperationException">When the property could not be retrieved.</exception>
    public IEnumerable<string> GetStringList(string key)
    {
        byte[] keyBytes = NativeInterop.StringToUtf8(key);
        ThrowIfError(NativeMduApi.mdu_get_string_list(_handle, keyBytes, out IntPtr ptr, out ulong count));
        return NativeInterop.MarshalStringArray(ptr, count);
    }

    /// <summary>
    /// Gets a list of file path property values by its fully-qualified key.
    /// </summary>
    /// <param name="key">The fully-qualified property key.</param>
    /// <returns>An enumerable of file path values.</returns>
    /// <exception cref="InvalidOperationException">When the property could not be retrieved.</exception>
    public IEnumerable<string> GetPathList(string key)
    {
        byte[] keyBytes = NativeInterop.StringToUtf8(key);
        ThrowIfError(NativeMduApi.mdu_get_path_list(_handle, keyBytes, out IntPtr ptr, out ulong count));
        return NativeInterop.MarshalStringArray(ptr, count);
    }

    /// <summary>
    /// Gets a list of double-precision floating-point property values by its fully-qualified key.
    /// </summary>
    /// <param name="key">The fully-qualified property key.</param>
    /// <returns>An enumerable of double values.</returns>
    /// <exception cref="InvalidOperationException">When the property could not be retrieved.</exception>
    public IEnumerable<double> GetDoubleList(string key)
    {
        byte[] keyBytes = NativeInterop.StringToUtf8(key);
        ThrowIfError(NativeMduApi.mdu_get_double_list(_handle, keyBytes, out IntPtr ptr, out ulong count));
        return NativeInterop.MarshalDoubleArray(ptr, count);
    }

    /// <summary>
    /// Sets an integer property value by its fully-qualified key.
    /// </summary>
    /// <param name="key">The fully-qualified property key.</param>
    /// <param name="value">The integer value to assign.</param>
    /// <exception cref="InvalidOperationException">When the property could not be set.</exception>
    public void SetInt(string key, int value)
    {
        byte[] keyBytes = NativeInterop.StringToUtf8(key);
        ThrowIfError(NativeMduApi.mdu_set_int(_handle, keyBytes, value));
    }

    /// <summary>
    /// Sets a boolean property value by its fully-qualified key.
    /// </summary>
    /// <param name="key">The fully-qualified property key.</param>
    /// <param name="value">The boolean value to assign.</param>
    /// <exception cref="InvalidOperationException">When the property could not be set.</exception>
    public void SetBool(string key, bool value)
    {
        byte[] keyBytes = NativeInterop.StringToUtf8(key);
        ThrowIfError(NativeMduApi.mdu_set_bool(_handle, keyBytes, value ? 1 : 0));
    }

    /// <summary>
    /// Sets a double-precision floating-point property value by its fully-qualified key.
    /// </summary>
    /// <param name="key">The fully-qualified property key.</param>
    /// <param name="value">The double value to assign.</param>
    /// <exception cref="InvalidOperationException">When the property could not be set.</exception>
    public void SetDouble(string key, double value)
    {
        byte[] keyBytes = NativeInterop.StringToUtf8(key);
        ThrowIfError(NativeMduApi.mdu_set_double(_handle, keyBytes, value));
    }

    /// <summary>
    /// Sets a string property value by its fully-qualified key.
    /// </summary>
    /// <param name="key">The fully-qualified property key.</param>
    /// <param name="value">The string value to assign.</param>
    /// <exception cref="InvalidOperationException">When the property could not be set.</exception>
    public void SetString(string key, string value)
    {
        byte[] keyBytes = NativeInterop.StringToUtf8(key);
        byte[] valueBytes = NativeInterop.StringToUtf8(value);
        ThrowIfError(NativeMduApi.mdu_set_string(_handle, keyBytes, valueBytes));
    }

    /// <summary>
    /// Sets a file path property value by its fully-qualified key.
    /// </summary>
    /// <param name="key">The fully-qualified property key.</param>
    /// <param name="value">The file path value to assign.</param>
    /// <exception cref="InvalidOperationException">When the property could not be set.</exception>
    public void SetPath(string key, string value)
    {
        byte[] keyBytes = NativeInterop.StringToUtf8(key);
        byte[] valueBytes = NativeInterop.StringToUtf8(value);
        ThrowIfError(NativeMduApi.mdu_set_path(_handle, keyBytes, valueBytes));
    }

    /// <summary>
    /// Sets a date/time property value by its fully-qualified key.
    /// </summary>
    /// <param name="key">The fully-qualified property key.</param>
    /// <param name="value">The <see cref="DateTime" /> value to assign. Treated as UTC.</param>
    /// <exception cref="InvalidOperationException">When the property could not be set.</exception>
    public void SetDateTime(string key, DateTime value)
    {
        byte[] keyBytes = NativeInterop.StringToUtf8(key);
        ThrowIfError(NativeMduApi.mdu_set_datetime(
            _handle, keyBytes, new DateTimeOffset(value, TimeSpan.Zero).ToUnixTimeSeconds()));
    }

    /// <summary>
    /// Sets an enumeration property value by its fully-qualified key.
    /// </summary>
    /// <param name="key">The fully-qualified property key.</param>
    /// <param name="value">The integer representation of the enumeration value to assign.</param>
    /// <exception cref="InvalidOperationException">When the property could not be set.</exception>
    public void SetEnum(string key, int value)
    {
        byte[] keyBytes = NativeInterop.StringToUtf8(key);
        ThrowIfError(NativeMduApi.mdu_set_enum(_handle, keyBytes, value));
    }

    public void SetEnumName(string key, string value)
    {
        byte[] keyBytes = NativeInterop.StringToUtf8(key);
        byte[] valueBytes = NativeInterop.StringToUtf8(value);
        ThrowIfError(NativeMduApi.mdu_set_enum_name(_handle, keyBytes, valueBytes));
    }

    /// <summary>
    /// Sets a list of string property values by its fully-qualified key.
    /// </summary>
    /// <param name="key">The fully-qualified property key.</param>
    /// <param name="values">The string values to assign.</param>
    /// <exception cref="InvalidOperationException">When the property could not be set.</exception>
    public void SetStringList(string key, IEnumerable<string> values)
    {
        byte[] keyBytes = NativeInterop.StringToUtf8(key);
        NativeInterop.MarshalStringArray(values.ToArray(), (ptr, size) =>
            ThrowIfError(NativeMduApi.mdu_set_string_list(_handle, keyBytes, ptr, size)));
    }

    /// <summary>
    /// Sets a list of file path property values by its fully-qualified key.
    /// </summary>
    /// <param name="key">The fully-qualified property key.</param>
    /// <param name="values">The file path values to assign.</param>
    /// <exception cref="InvalidOperationException">When the property could not be set.</exception>
    public void SetPathList(string key, IEnumerable<string> values)
    {
        byte[] keyBytes = NativeInterop.StringToUtf8(key);
        NativeInterop.MarshalStringArray(values.ToArray(), (ptr, size) =>
            ThrowIfError(NativeMduApi.mdu_set_path_list(_handle, keyBytes, ptr, size)));
    }

    /// <summary>
    /// Sets a list of double-precision floating-point property values by its fully-qualified key.
    /// </summary>
    /// <param name="key">The fully-qualified property key.</param>
    /// <param name="values">The double values to assign.</param>
    /// <exception cref="InvalidOperationException">When the property could not be set.</exception>
    public void SetDoubleList(string key, IEnumerable<double> values)
    {
        byte[] keyBytes = NativeInterop.StringToUtf8(key);
        NativeInterop.MarshalDoubleArray(values.ToArray(), (ptr, size) =>
            ThrowIfError(NativeMduApi.mdu_set_double_list(_handle, keyBytes, ptr, size)));
    }

    /// <summary>
    /// Gets the issue report produced after the last load operation.
    /// </summary>
    /// <returns>An <see cref="IssueReport" /> containing any issues encountered during parsing.</returns>
    /// <exception cref="InvalidOperationException">When the issue report could not be retrieved.</exception>
    public IssueReport GetIssueReport()
    {
        ThrowIfError(NativeMduApi.mdu_get_issue_list(_handle, out IntPtr ptr, out ulong count));
        Issue[] issues = NativeInterop.MarshalIssueArray(ptr, count);
        return new IssueReport(issues);
    }

    private static void ThrowIfError(int result)
    {
        if (result == 0)
        {
            return;
        }

        string message = NativeInterop.PtrToStringUtf8(NativeMduApi.dflowfm_io_get_last_error());
        if (string.IsNullOrWhiteSpace(message))
        {
            message = "Unknown native error.";
        }

        throw new InvalidOperationException(message);
    }
}