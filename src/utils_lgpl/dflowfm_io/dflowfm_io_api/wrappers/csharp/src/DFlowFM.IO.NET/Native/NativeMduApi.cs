using System.Runtime.InteropServices;

namespace DFlowFM.IO.Native;

internal static class NativeMduApi
{
    private const string DllName = "dflowfm_io_api";

    static NativeMduApi()
    {
        string? directoryName = Path.GetDirectoryName(typeof(NativeMduApi).Assembly.Location);
        NativeLibrary.LoadNativeDll(DllName, Path.Combine(directoryName!, @"win-x64\native"));
    }

    [DllImport(DllName, CallingConvention = CallingConvention.Cdecl)]
    public static extern IntPtr dflowfm_io_get_last_error();

    [DllImport(DllName, CallingConvention = CallingConvention.Cdecl)]
    public static extern int mdu_create(out IntPtr handleOut);

    [DllImport(DllName, CallingConvention = CallingConvention.Cdecl)]
    public static extern int mdu_destroy(ref IntPtr handle);

    [DllImport(DllName, CallingConvention = CallingConvention.Cdecl)]
    public static extern int mdu_load_from_file(SafeMduHandle handle, byte[] filename);

    [DllImport(DllName, CallingConvention = CallingConvention.Cdecl)]
    internal static extern int mdu_load_from_string(SafeMduHandle handle, byte[] data, ulong length);

    [DllImport(DllName, CallingConvention = CallingConvention.Cdecl)]
    public static extern int mdu_save_to_file(SafeMduHandle handle, byte[] filename);

    [DllImport(DllName, CallingConvention = CallingConvention.Cdecl)]
    public static extern int mdu_save_to_string(SafeMduHandle handle, out IntPtr dataOut);

    [DllImport(DllName, CallingConvention = CallingConvention.Cdecl)]
    public static extern int mdu_get_int(SafeMduHandle handle, byte[] key, out int intOut);

    [DllImport(DllName, CallingConvention = CallingConvention.Cdecl)]
    public static extern int mdu_get_bool(SafeMduHandle handle, byte[] key, out int boolOut);

    [DllImport(DllName, CallingConvention = CallingConvention.Cdecl)]
    public static extern int mdu_get_double(SafeMduHandle handle, byte[] key, out double doubleOut);

    [DllImport(DllName, CallingConvention = CallingConvention.Cdecl)]
    public static extern int mdu_get_string(SafeMduHandle handle, byte[] key, out IntPtr stringOut);

    [DllImport(DllName, CallingConvention = CallingConvention.Cdecl)]
    public static extern int mdu_get_path(SafeMduHandle handle, byte[] key, out IntPtr pathOut);

    [DllImport(DllName, CallingConvention = CallingConvention.Cdecl)]
    public static extern int mdu_get_datetime(SafeMduHandle handle, byte[] key, out long epochOut);

    [DllImport(DllName, CallingConvention = CallingConvention.Cdecl)]
    public static extern int mdu_get_enum(SafeMduHandle handle, byte[] key, out int enumOut);

    [DllImport(DllName, CallingConvention = CallingConvention.Cdecl)]
    public static extern int mdu_get_enum_name(SafeMduHandle handle, byte[] key, out IntPtr nameOut);

    [DllImport(DllName, CallingConvention = CallingConvention.Cdecl)]
    public static extern int mdu_get_string_list(SafeMduHandle handle, byte[] key, out IntPtr stringListOut, out ulong sizeOut);

    [DllImport(DllName, CallingConvention = CallingConvention.Cdecl)]
    public static extern int mdu_get_path_list(SafeMduHandle handle, byte[] key, out IntPtr pathListOut, out ulong sizeOut);

    [DllImport(DllName, CallingConvention = CallingConvention.Cdecl)]
    public static extern int mdu_get_double_list(SafeMduHandle handle, byte[] key, out IntPtr doubleListOut, out ulong sizeOut);

    [DllImport(DllName, CallingConvention = CallingConvention.Cdecl)]
    public static extern int mdu_set_int(SafeMduHandle handle, byte[] key, int value);

    [DllImport(DllName, CallingConvention = CallingConvention.Cdecl)]
    public static extern int mdu_set_bool(SafeMduHandle handle, byte[] key, int value);

    [DllImport(DllName, CallingConvention = CallingConvention.Cdecl)]
    public static extern int mdu_set_double(SafeMduHandle handle, byte[] key, double value);

    [DllImport(DllName, CallingConvention = CallingConvention.Cdecl)]
    public static extern int mdu_set_string(SafeMduHandle handle, byte[] key, byte[] value);

    [DllImport(DllName, CallingConvention = CallingConvention.Cdecl)]
    public static extern int mdu_set_path(SafeMduHandle handle, byte[] key, byte[] value);

    [DllImport(DllName, CallingConvention = CallingConvention.Cdecl)]
    public static extern int mdu_set_datetime(SafeMduHandle handle, byte[] key, long epoch);

    [DllImport(DllName, CallingConvention = CallingConvention.Cdecl)]
    public static extern int mdu_set_enum(SafeMduHandle handle, byte[] key, int enumValue);

    [DllImport(DllName, CallingConvention = CallingConvention.Cdecl)]
    public static extern int mdu_set_enum_name(SafeMduHandle handle, byte[] key, byte[] name);

    [DllImport(DllName, CallingConvention = CallingConvention.Cdecl)]
    public static extern int mdu_set_string_list(SafeMduHandle handle, byte[] key, IntPtr stringList, ulong size);

    [DllImport(DllName, CallingConvention = CallingConvention.Cdecl)]
    public static extern int mdu_set_path_list(SafeMduHandle handle, byte[] key, IntPtr pathList, ulong size);

    [DllImport(DllName, CallingConvention = CallingConvention.Cdecl)]
    public static extern int mdu_set_double_list(SafeMduHandle handle, byte[] key, IntPtr doubleList, ulong size);

    [DllImport(DllName, CallingConvention = CallingConvention.Cdecl)]
    public static extern int mdu_get_issue_list(SafeMduHandle handle, out IntPtr issueListOut, out ulong sizeOut);
}