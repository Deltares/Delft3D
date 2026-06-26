using System.Runtime.InteropServices;

namespace DFlowFM.IO.Native;

internal static class NativeMduApi
{
    private const string dllName = "dflowfm_io_api";

    static NativeMduApi()
    {
        string? directoryName = Path.GetDirectoryName(typeof(NativeMduApi).Assembly.Location);
        NativeLibrary.LoadNativeDll(dllName, Path.Combine(directoryName!, @"win-x64\native"));
    }

    [DllImport(dllName, CallingConvention = CallingConvention.Cdecl)]
    public static extern IntPtr dflowfm_io_get_last_error();

    [DllImport(dllName, CallingConvention = CallingConvention.Cdecl)]
    public static extern int mdu_document_create(out IntPtr handleOut);

    [DllImport(dllName, CallingConvention = CallingConvention.Cdecl)]
    public static extern int mdu_document_destroy(ref IntPtr handle);

    [DllImport(dllName, CallingConvention = CallingConvention.Cdecl)]
    public static extern int mdu_document_load_from_file(SafeMduHandle handle, [MarshalAs(UnmanagedType.LPStr)] string filename);

    [DllImport(dllName, CallingConvention = CallingConvention.Cdecl)]
    internal static extern int mdu_document_load_from_string(SafeMduHandle handle, byte[] data, ulong length);

    [DllImport(dllName, CallingConvention = CallingConvention.Cdecl)]
    public static extern int mdu_document_save_to_file(SafeMduHandle handle, [MarshalAs(UnmanagedType.LPStr)] string filename);

    [DllImport(dllName, CallingConvention = CallingConvention.Cdecl)]
    public static extern int mdu_document_save_to_string(SafeMduHandle handle, out IntPtr dataOut);

    [DllImport(dllName, CallingConvention = CallingConvention.Cdecl)]
    public static extern int mdu_model_get_int(SafeMduHandle handle, [MarshalAs(UnmanagedType.LPStr)] string key, out int intOut);

    [DllImport(dllName, CallingConvention = CallingConvention.Cdecl)]
    public static extern int mdu_model_get_bool(SafeMduHandle handle, [MarshalAs(UnmanagedType.LPStr)] string key, out int boolOut);

    [DllImport(dllName, CallingConvention = CallingConvention.Cdecl)]
    public static extern int mdu_model_get_double(SafeMduHandle handle, [MarshalAs(UnmanagedType.LPStr)] string key, out double doubleOut);

    [DllImport(dllName, CallingConvention = CallingConvention.Cdecl)]
    public static extern int mdu_model_get_string(SafeMduHandle handle, [MarshalAs(UnmanagedType.LPStr)] string key, out IntPtr stringOut);

    [DllImport(dllName, CallingConvention = CallingConvention.Cdecl)]
    public static extern int mdu_model_get_path(SafeMduHandle handle, [MarshalAs(UnmanagedType.LPStr)] string key, out IntPtr pathOut);

    [DllImport(dllName, CallingConvention = CallingConvention.Cdecl)]
    public static extern int mdu_model_get_datetime(SafeMduHandle handle, [MarshalAs(UnmanagedType.LPStr)] string key, out long epochOut);

    [DllImport(dllName, CallingConvention = CallingConvention.Cdecl)]
    public static extern int mdu_model_get_enum(SafeMduHandle handle, [MarshalAs(UnmanagedType.LPStr)] string key, out int enumOut);

    [DllImport(dllName, CallingConvention = CallingConvention.Cdecl)]
    public static extern int mdu_model_get_string_list(SafeMduHandle handle, [MarshalAs(UnmanagedType.LPStr)] string key, out IntPtr stringListOut, out ulong sizeOut);

    [DllImport(dllName, CallingConvention = CallingConvention.Cdecl)]
    public static extern int mdu_model_get_path_list(SafeMduHandle handle, [MarshalAs(UnmanagedType.LPStr)] string key, out IntPtr pathListOut, out ulong sizeOut);

    [DllImport(dllName, CallingConvention = CallingConvention.Cdecl)]
    public static extern int mdu_model_get_double_list(SafeMduHandle handle, [MarshalAs(UnmanagedType.LPStr)] string key, out IntPtr doubleListOut, out ulong sizeOut);

    [DllImport(dllName, CallingConvention = CallingConvention.Cdecl)]
    public static extern int mdu_model_set_int(SafeMduHandle handle, [MarshalAs(UnmanagedType.LPStr)] string key, int value);

    [DllImport(dllName, CallingConvention = CallingConvention.Cdecl)]
    public static extern int mdu_model_set_bool(SafeMduHandle handle, [MarshalAs(UnmanagedType.LPStr)] string key, int value);

    [DllImport(dllName, CallingConvention = CallingConvention.Cdecl)]
    public static extern int mdu_model_set_double(SafeMduHandle handle, [MarshalAs(UnmanagedType.LPStr)] string key, double value);

    [DllImport(dllName, CallingConvention = CallingConvention.Cdecl)]
    public static extern int mdu_model_set_string(SafeMduHandle handle, [MarshalAs(UnmanagedType.LPStr)] string key, [MarshalAs(UnmanagedType.LPStr)] string value);

    [DllImport(dllName, CallingConvention = CallingConvention.Cdecl)]
    public static extern int mdu_model_set_path(SafeMduHandle handle, [MarshalAs(UnmanagedType.LPStr)] string key, [MarshalAs(UnmanagedType.LPStr)] string value);

    [DllImport(dllName, CallingConvention = CallingConvention.Cdecl)]
    public static extern int mdu_model_set_datetime(SafeMduHandle handle, [MarshalAs(UnmanagedType.LPStr)] string key, long epoch);

    [DllImport(dllName, CallingConvention = CallingConvention.Cdecl)]
    public static extern int mdu_model_set_enum(SafeMduHandle handle, [MarshalAs(UnmanagedType.LPStr)] string key, int enumValue);

    [DllImport(dllName, CallingConvention = CallingConvention.Cdecl)]
    public static extern int mdu_model_set_string_list(SafeMduHandle handle, [MarshalAs(UnmanagedType.LPStr)] string key, IntPtr stringList, ulong size);

    [DllImport(dllName, CallingConvention = CallingConvention.Cdecl)]
    public static extern int mdu_model_set_path_list(SafeMduHandle handle, [MarshalAs(UnmanagedType.LPStr)] string key, IntPtr pathList, ulong size);

    [DllImport(dllName, CallingConvention = CallingConvention.Cdecl)]
    public static extern int mdu_model_set_double_list(SafeMduHandle handle, [MarshalAs(UnmanagedType.LPStr)] string key, IntPtr doubleList, ulong size);

    [DllImport(dllName, CallingConvention = CallingConvention.Cdecl)]
    public static extern int mdu_report_get_issue_list(SafeMduHandle handle, out IntPtr issueListOut, out ulong sizeOut);
}