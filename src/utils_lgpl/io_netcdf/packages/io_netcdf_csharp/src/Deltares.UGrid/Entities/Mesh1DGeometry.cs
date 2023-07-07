using System;
using System.Runtime.InteropServices;

namespace Deltares.UGrid.Entities
{
    [StructLayout(LayoutKind.Sequential)]
    internal struct Mesh1DGeometry
    {
        public IntPtr BranchIds;
        public IntPtr BranchOffsets;
        public IntPtr NodeX;
        public IntPtr NodeY;
        public IntPtr NodeIds;
        public IntPtr NodeLongNames;

        public IntPtr EdgeBranchIds;
        public IntPtr EdgeNodes;
        public IntPtr EdgeCenterPointOffset;
        public IntPtr EdgeCenterPointX;
        public IntPtr EdgeCenterPointY;

        public int startIndex;
    }
}