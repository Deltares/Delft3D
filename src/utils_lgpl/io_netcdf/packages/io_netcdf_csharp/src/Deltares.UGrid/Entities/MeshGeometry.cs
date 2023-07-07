using System;
using System.Runtime.InteropServices;

namespace Deltares.UGrid.Entities
{
    [StructLayout(LayoutKind.Sequential)]
    internal struct MeshGeometry
    {
        public IntPtr edge_nodes;
        public IntPtr face_nodes;
        public IntPtr edge_faces;
        public IntPtr face_edges;
        public IntPtr face_links;

        public IntPtr nnodex;
        public IntPtr nnodey;
        public IntPtr nedge_nodes;
        public IntPtr nbranchlengths;
        public IntPtr nbranchgeometrynodes;

        public IntPtr ngeopointx;
        public IntPtr ngeopointy;
        public IntPtr nbranchorder;
        public IntPtr branchidx;
        public IntPtr branchoffsets;

        public IntPtr nodex;
        public IntPtr nodey;
        public IntPtr nodez;
        public IntPtr edgex;
        public IntPtr edgey;
        public IntPtr edgez;
        public IntPtr facex;
        public IntPtr facey;
        public IntPtr facez;

        public IntPtr layer_zs;
        public IntPtr interface_zs;

        public IntPtr nodeids;
        public IntPtr nodelongnames;
        public IntPtr nbranchids;
        public IntPtr nbranchlongnames;
        public IntPtr nnodeids;
        public IntPtr nnodelongnames;
        public int startIndex;
    }
}