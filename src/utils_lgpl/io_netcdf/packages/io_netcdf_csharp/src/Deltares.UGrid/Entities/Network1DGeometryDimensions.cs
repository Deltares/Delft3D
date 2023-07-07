using System;
using System.Runtime.InteropServices;

namespace Deltares.UGrid.Entities
{
    [StructLayout(LayoutKind.Sequential)]
    internal struct Network1DGeometryDimensions
    {
        public int NumberOfNodes;

        public int NumberOfBranches;

        public int NumberOfBranchGeometryPoints;
    }
}