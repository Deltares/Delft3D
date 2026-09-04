using System.Runtime.InteropServices;
using System.Text;

using DFlowFM.IO.Reporting;

namespace DFlowFM.IO.Native;

internal static class NativeInterop
{
    private static readonly UTF8Encoding Utf8NoBom = new(false);

    public static string PtrToStringUtf8(IntPtr ptr)
    {
        if (ptr == IntPtr.Zero)
        {
            return string.Empty;
        }

        int length = 0;
        while (Marshal.ReadByte(ptr, length) != 0)
        {
            length++;
        }

        byte[] bytes = new byte[length];
        Marshal.Copy(ptr, bytes, 0, length);

        return Utf8NoBom.GetString(bytes);
    }

    public static byte[] StringToUtf8(string value)
    {
        return Utf8NoBom.GetBytes(value);
    }

    public static double[] MarshalDoubleArray(IntPtr ptr, ulong count)
    {
        double[] result = new double[(int)count];
        if (ptr != IntPtr.Zero)
        {
            Marshal.Copy(ptr, result, 0, (int)count);
        }

        return result;
    }

    public static string[] MarshalStringArray(IntPtr ptr, ulong count)
    {
        string[] result = new string[(int)count];
        for (int i = 0; i < (int)count; i++)
        {
            IntPtr strPtr = Marshal.ReadIntPtr(ptr, i * IntPtr.Size);
            result[i] = PtrToStringUtf8(strPtr);
        }

        return result;
    }

    public static void MarshalDoubleArray(double[] values, Action<IntPtr, ulong> action)
    {
        GCHandle pin = GCHandle.Alloc(values, GCHandleType.Pinned);
        try
        {
            action(pin.AddrOfPinnedObject(), (ulong)values.Length);
        }
        finally
        {
            pin.Free();
        }
    }

    public static void MarshalStringArray(string[] values, Action<IntPtr, ulong> action)
    {
        IntPtr[] ptrs = Array.ConvertAll(values, v =>
        {
            byte[] bytes = StringToUtf8(v);
            IntPtr ptr = Marshal.AllocHGlobal(bytes.Length + 1);
            
            Marshal.Copy(bytes, 0, ptr, bytes.Length);
            Marshal.WriteByte(ptr, bytes.Length, 0);
            
            return ptr;
        });

        GCHandle pin = GCHandle.Alloc(ptrs, GCHandleType.Pinned);
        try
        {
            action(pin.AddrOfPinnedObject(), (ulong)ptrs.Length);
        }
        finally
        {
            pin.Free();
            foreach (IntPtr ptr in ptrs)
            {
                Marshal.FreeHGlobal(ptr);
            }
        }
    }

    public static Issue[] MarshalIssueArray(IntPtr ptr, ulong count)
    {
        return MarshalStructArray<IssueNative, Issue>(ptr, count, n => n.ToIssue());
    }

    private static Issue ToIssue(this IssueNative native)
    {
        return new Issue(
            (IssueSeverity)native.Severity,
            PtrToStringUtf8(native.Message),
            native.LineNumber > 0 ? native.LineNumber : null);
    }

    private static TManaged[] MarshalStructArray<TNative, TManaged>(IntPtr ptr, ulong count,
        Func<TNative, TManaged> convert)
        where TNative : struct
    {
        TManaged[] result = new TManaged[(int)count];
        int structSize = Marshal.SizeOf(typeof(TNative));

        for (int i = 0; i < (int)count; i++)
        {
            IntPtr elementPtr = new(ptr.ToInt64() + (i * structSize));
            TNative native = (TNative)Marshal.PtrToStructure(elementPtr, typeof(TNative))!;
            result[i] = convert(native);
        }

        return result;
    }
}