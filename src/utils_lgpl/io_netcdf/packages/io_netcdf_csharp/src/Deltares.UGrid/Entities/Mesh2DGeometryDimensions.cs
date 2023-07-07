using System.Runtime.InteropServices;

namespace Deltares.UGrid.Entities
{
    [StructLayout(LayoutKind.Sequential)]
    internal struct Mesh2DGeometryDimensions
    {
        [MarshalAs(UnmanagedType.ByValArray, SizeConst = 255)]
        public char[] name;
        public int dim;
        public int numnode;
        public int numedge;
        public int numface;
        public int maxnumfacenodes;
        public int numlayer;
        public int layertype;
        public int nnodes;
        public int nbranches;
        public int ngeometry;
        public int epgs;
        public int numlinks;
    }
}