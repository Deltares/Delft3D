using System;
using System.Runtime.InteropServices;

namespace Deltares.UGrid.Entities
{
    [StructLayout(LayoutKind.Sequential)]
    internal struct Network1DGeometry
    {
        public IntPtr NodeX;
        public IntPtr NodeY;
        public IntPtr NodeIds;
        public IntPtr NodeLongNames;

        public IntPtr SourceNodes;
        public IntPtr TargetNodes;
        public IntPtr BranchLengths;
        public IntPtr BranchGeometryCount;
        public IntPtr BranchIds;
        public IntPtr BranchLongNames;
        public IntPtr BranchOrder;
        public IntPtr BranchTypes;

        public IntPtr BranchGeometryX;
        public IntPtr BranchGeometryY;

        public int startIndex;
    }
}