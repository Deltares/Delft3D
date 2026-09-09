using System.Runtime.InteropServices;

namespace DFlowFM.IO.Native;

internal sealed class SafeMduHandle : SafeHandle
{
    public SafeMduHandle()
        : base(IntPtr.Zero, true)
    {
        int result = NativeMduApi.mdu_create(out IntPtr ptr);

        if (result != 0)
        {
            throw new InvalidOperationException("Failed to create MDU handle.");
        }

        SetHandle(ptr);
    }

    public override bool IsInvalid => handle == IntPtr.Zero;

    protected override bool ReleaseHandle()
    {
        int result = NativeMduApi.mdu_destroy(ref handle);
        return result == 0;
    }
}