using DFlowFM.IO.Native;
using DFlowFM.IO.Reporting;

namespace DFlowFM.IO.Mdu;

public sealed class MduApi : IDisposable
{
    private readonly SafeMduHandle _handle = new();

    public void Dispose()
    {
        _handle.Dispose();
    }

    public void LoadFromFile(string path)
    {
        byte[] pathBytes = NativeInterop.StringToUtf8(path);
        ThrowIfError(NativeMduApi.mdu_load_from_file(_handle, pathBytes));
    }

    public void LoadFromString(string content)
    {
        byte[] contentBytes = NativeInterop.StringToUtf8(content);
        ThrowIfError(NativeMduApi.mdu_load_from_string(_handle, contentBytes, (ulong)contentBytes.Length));
    }

    public void SaveToFile(string path)
    {
        byte[] pathBytes = NativeInterop.StringToUtf8(path);
        ThrowIfError(NativeMduApi.mdu_save_to_file(_handle, pathBytes));
    }

    public string SaveToString()
    {
        ThrowIfError(NativeMduApi.mdu_save_to_string(_handle, out IntPtr ptr));
        return NativeInterop.PtrToStringUtf8(ptr);
    }

    public int GetInt(string key)
    {
        byte[] keyBytes = NativeInterop.StringToUtf8(key);
        ThrowIfError(NativeMduApi.mdu_get_int(_handle, keyBytes, out int value));
        return value;
    }

    public bool GetBool(string key)
    {
        byte[] keyBytes = NativeInterop.StringToUtf8(key);
        ThrowIfError(NativeMduApi.mdu_get_bool(_handle, keyBytes, out int value));
        return value != 0;
    }

    public double GetDouble(string key)
    {
        byte[] keyBytes = NativeInterop.StringToUtf8(key);
        ThrowIfError(NativeMduApi.mdu_get_double(_handle, keyBytes, out double value));
        return value;
    }

    public string GetString(string key)
    {
        byte[] keyBytes = NativeInterop.StringToUtf8(key);
        ThrowIfError(NativeMduApi.mdu_get_string(_handle, keyBytes, out IntPtr ptr));
        return NativeInterop.PtrToStringUtf8(ptr);
    }

    public string GetPath(string key)
    {
        byte[] keyBytes = NativeInterop.StringToUtf8(key);
        ThrowIfError(NativeMduApi.mdu_get_path(_handle, keyBytes, out IntPtr ptr));
        return NativeInterop.PtrToStringUtf8(ptr);
    }

    public DateTime GetDateTime(string key)
    {
        byte[] keyBytes = NativeInterop.StringToUtf8(key);
        ThrowIfError(NativeMduApi.mdu_get_datetime(_handle, keyBytes, out long epochSeconds));
        return DateTimeOffset.FromUnixTimeSeconds(epochSeconds).UtcDateTime;
    }

    public int GetEnum(string key)
    {
        byte[] keyBytes = NativeInterop.StringToUtf8(key);
        ThrowIfError(NativeMduApi.mdu_get_enum(_handle, keyBytes, out int value));
        return value;
    }

    public IEnumerable<string> GetStringList(string key)
    {
        byte[] keyBytes = NativeInterop.StringToUtf8(key);
        ThrowIfError(NativeMduApi.mdu_get_string_list(_handle, keyBytes, out IntPtr ptr, out ulong count));
        return NativeInterop.MarshalStringArray(ptr, count);
    }

    public IEnumerable<string> GetPathList(string key)
    {
        byte[] keyBytes = NativeInterop.StringToUtf8(key);
        ThrowIfError(NativeMduApi.mdu_get_path_list(_handle, keyBytes, out IntPtr ptr, out ulong count));
        return NativeInterop.MarshalStringArray(ptr, count);
    }

    public IEnumerable<double> GetDoubleList(string key)
    {
        byte[] keyBytes = NativeInterop.StringToUtf8(key);
        ThrowIfError(NativeMduApi.mdu_get_double_list(_handle, keyBytes, out IntPtr ptr, out ulong count));
        return NativeInterop.MarshalDoubleArray(ptr, count);
    }

    public void SetInt(string key, int value)
    {
        byte[] keyBytes = NativeInterop.StringToUtf8(key);
        ThrowIfError(NativeMduApi.mdu_set_int(_handle, keyBytes, value));
    }

    public void SetBool(string key, bool value)
    {
        byte[] keyBytes = NativeInterop.StringToUtf8(key);
        ThrowIfError(NativeMduApi.mdu_set_bool(_handle, keyBytes, value ? 1 : 0));
    }

    public void SetDouble(string key, double value)
    {
        byte[] keyBytes = NativeInterop.StringToUtf8(key);
        ThrowIfError(NativeMduApi.mdu_set_double(_handle, keyBytes, value));
    }

    public void SetString(string key, string value)
    {
        byte[] keyBytes = NativeInterop.StringToUtf8(key);
        byte[] valueBytes = NativeInterop.StringToUtf8(value);
        ThrowIfError(NativeMduApi.mdu_set_string(_handle, keyBytes, valueBytes));
    }

    public void SetPath(string key, string value)
    {
        byte[] keyBytes = NativeInterop.StringToUtf8(key);
        byte[] valueBytes = NativeInterop.StringToUtf8(value);
        ThrowIfError(NativeMduApi.mdu_set_path(_handle, keyBytes, valueBytes));
    }

    public void SetDateTime(string key, DateTime value)
    {
        byte[] keyBytes = NativeInterop.StringToUtf8(key);
        ThrowIfError(NativeMduApi.mdu_set_datetime(
            _handle, keyBytes, new DateTimeOffset(value, TimeSpan.Zero).ToUnixTimeSeconds()));
    }

    public void SetEnum(string key, int value)
    {
        byte[] keyBytes = NativeInterop.StringToUtf8(key);
        ThrowIfError(NativeMduApi.mdu_set_enum(_handle, keyBytes, value));
    }

    public void SetStringList(string key, IEnumerable<string> values)
    {
        byte[] keyBytes = NativeInterop.StringToUtf8(key);
        NativeInterop.MarshalStringArray(values.ToArray(), (ptr, size) =>
            ThrowIfError(NativeMduApi.mdu_set_string_list(_handle, keyBytes, ptr, size)));
    }

    public void SetPathList(string key, IEnumerable<string> values)
    {
        byte[] keyBytes = NativeInterop.StringToUtf8(key);
        NativeInterop.MarshalStringArray(values.ToArray(), (ptr, size) =>
            ThrowIfError(NativeMduApi.mdu_set_path_list(_handle, keyBytes, ptr, size)));
    }

    public void SetDoubleList(string key, IEnumerable<double> values)
    {
        byte[] keyBytes = NativeInterop.StringToUtf8(key);
        NativeInterop.MarshalDoubleArray(values.ToArray(), (ptr, size) =>
            ThrowIfError(NativeMduApi.mdu_set_double_list(_handle, keyBytes, ptr, size)));
    }

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