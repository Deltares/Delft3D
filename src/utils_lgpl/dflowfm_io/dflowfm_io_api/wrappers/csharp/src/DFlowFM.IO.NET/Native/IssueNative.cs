using System.Runtime.InteropServices;

namespace DFlowFM.IO.Native;

[StructLayout(LayoutKind.Sequential)]
internal struct IssueNative
{
    public int LineNumber;
    public int Severity;
    public IntPtr Message;
}