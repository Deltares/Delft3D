using System.Runtime.InteropServices;
using System.Text;
using DFlowFM.IO.Native;
using DFlowFM.IO.Reporting;

namespace DFlowFM.IO.Mdu;

public sealed class MduApi : IDisposable
{
    private readonly SafeMduHandle handle = new();

    public void Dispose()
    {
        handle.Dispose();
    }

    public void LoadFromFile(string path)
    {
        ThrowIfError(NativeMduApi.mdu_load_from_file(handle, path));
    }

    public void LoadFromString(string content)
    {
        byte[] bytes = Encoding.Default.GetBytes(content);
        ThrowIfError(NativeMduApi.mdu_load_from_string(handle, bytes, (ulong)bytes.Length));
    }

    public void SaveToFile(string path)
    {
        ThrowIfError(NativeMduApi.mdu_save_to_file(handle, path));
    }

    public string SaveToString()
    {
        ThrowIfError(NativeMduApi.mdu_save_to_string(handle, out IntPtr ptr));
        return Marshal.PtrToStringAnsi(ptr) ?? string.Empty;
    }

    public int GetInt(string key)
    {
        ThrowIfError(NativeMduApi.mdu_get_int(handle, key, out int value));
        return value;
    }

    public bool GetBool(string key)
    {
        ThrowIfError(NativeMduApi.mdu_get_bool(handle, key, out int value));
        return value != 0;
    }

    public double GetDouble(string key)
    {
        ThrowIfError(NativeMduApi.mdu_get_double(handle, key, out double value));
        return value;
    }

    public string GetString(string key)
    {
        ThrowIfError(NativeMduApi.mdu_get_string(handle, key, out IntPtr ptr));
        return Marshal.PtrToStringAnsi(ptr) ?? string.Empty;
    }

    public string GetPath(string key)
    {
        ThrowIfError(NativeMduApi.mdu_get_path(handle, key, out IntPtr ptr));
        return Marshal.PtrToStringAnsi(ptr) ?? string.Empty;
    }

    public DateTime GetDateTime(string key)
    {
        ThrowIfError(NativeMduApi.mdu_get_datetime(handle, key, out long epochSeconds));
        return DateTimeOffset.FromUnixTimeSeconds(epochSeconds).UtcDateTime;
    }

    public int GetEnum(string key)
    {
        ThrowIfError(NativeMduApi.mdu_get_enum(handle, key, out int value));
        return value;
    }

    public IEnumerable<string> GetStringList(string key)
    {
        ThrowIfError(NativeMduApi.mdu_get_string_list(handle, key, out IntPtr ptr, out ulong count));
        return NativeInterop.MarshalStringArray(ptr, count);
    }

    public IEnumerable<string> GetPathList(string key)
    {
        ThrowIfError(NativeMduApi.mdu_get_path_list(handle, key, out IntPtr ptr, out ulong count));
        return NativeInterop.MarshalStringArray(ptr, count);
    }

    public IEnumerable<double> GetDoubleList(string key)
    {
        ThrowIfError(NativeMduApi.mdu_get_double_list(handle, key, out IntPtr ptr, out ulong count));
        return NativeInterop.MarshalDoubleArray(ptr, count);
    }

    public void SetInt(string key, int value)
    {
        ThrowIfError(NativeMduApi.mdu_set_int(handle, key, value));
    }

    public void SetBool(string key, bool value)
    {
        ThrowIfError(NativeMduApi.mdu_set_bool(handle, key, value ? 1 : 0));
    }

    public void SetDouble(string key, double value)
    {
        ThrowIfError(NativeMduApi.mdu_set_double(handle, key, value));
    }

    public void SetString(string key, string value)
    {
        ThrowIfError(NativeMduApi.mdu_set_string(handle, key, value));
    }

    public void SetPath(string key, string value)
    {
        ThrowIfError(NativeMduApi.mdu_set_path(handle, key, value));
    }

    public void SetDateTime(string key, DateTime value)
    {
        ThrowIfError(NativeMduApi.mdu_set_datetime(
            handle, key, new DateTimeOffset(value, TimeSpan.Zero).ToUnixTimeSeconds()));
    }

    public void SetEnum(string key, int value)
    {
        ThrowIfError(NativeMduApi.mdu_set_enum(handle, key, value));
    }

    public void SetStringList(string key, IEnumerable<string> values)
    {
        NativeInterop.MarshalStringArray(values.ToArray(), (ptr, size) =>
            ThrowIfError(NativeMduApi.mdu_set_string_list(handle, key, ptr, size)));
    }

    public void SetPathList(string key, IEnumerable<string> values)
    {
        NativeInterop.MarshalStringArray(values.ToArray(), (ptr, size) =>
            ThrowIfError(NativeMduApi.mdu_set_path_list(handle, key, ptr, size)));
    }

    public void SetDoubleList(string key, IEnumerable<double> values)
    {
        NativeInterop.MarshalDoubleArray(values.ToArray(), (ptr, size) =>
            ThrowIfError(NativeMduApi.mdu_set_double_list(handle, key, ptr, size)));
    }

    public IssueReport GetIssueReport()
    {
        ThrowIfError(NativeMduApi.mdu_get_issue_list(handle, out IntPtr ptr, out ulong count));
        Issue[] issues = NativeInterop.MarshalIssueArray(ptr, count);
        return new IssueReport(issues);
    }

    private static void ThrowIfError(int result)
    {
        if (result == 0)
        {
            return;
        }

        string message = Marshal.PtrToStringAnsi(NativeMduApi.dflowfm_io_get_last_error())
                         ?? "Unknown native error.";
        throw new InvalidOperationException(message);
    }
}