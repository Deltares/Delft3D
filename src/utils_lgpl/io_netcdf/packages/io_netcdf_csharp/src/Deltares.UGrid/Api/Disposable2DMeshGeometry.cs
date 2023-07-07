using Deltares.UGrid.Entities;
using ProtoBuf;

namespace Deltares.UGrid.Api
{
    /// <summary>
    /// Class describing a 2D mesh (vertices, edges and cells) 
    /// </summary>
    [ProtoContract(AsReferenceDefault = true)]
    public class Disposable2DMeshGeometry : DisposableMeshObject
    {
        /// <summary>
        /// Name of the mesh
        /// </summary>
        [ProtoMember(1)]
        public string Name;

        /// <summary>
        /// X position of the nodes
        /// </summary>
        [ProtoMember(2)]
        public double[] NodesX;

        /// <summary>
        /// Y position of the nodes
        /// </summary>
        [ProtoMember(3)]
        public double[] NodesY;

        /// <summary>
        /// Edge node connections {[from, to], [from, to] ... }
        /// </summary>
        [ProtoMember(4)]
        public int[] EdgeNodes;

        /// <summary>
        /// Nodes for each face (using <see cref="MaxNumberOfFaceNodes"/>)
        /// </summary>
        [ProtoMember(5)]
        public int[] FaceNodes;

        /// <summary>
        /// X position of face the center
        /// </summary>
        [ProtoMember(6)]
        public double[] FaceX;

        /// <summary>
        /// Y position of face the center
        /// </summary>
        [ProtoMember(7)]
        public double[] FaceY;

        /// <summary>
        /// Maximum number of nodes for a face (used in <see cref="FaceNodes"/>)
        /// </summary>
        [ProtoMember(8)]
        public int MaxNumberOfFaceNodes;

        internal Mesh2DGeometryDimensions CreateMeshDimensions()
        {
            return new Mesh2DGeometryDimensions
            {
                dim = 2, //-> Type of grid 1d (=1)/2d (=2)
                numnode = NodesX?.Length ?? 0,
                numedge = EdgeNodes?.Length /2 ?? 0,
                numface = FaceX?.Length ?? 0,
                maxnumfacenodes = MaxNumberOfFaceNodes,
                numlayer = 1,
                layertype = 1,
                nnodes = 0,
                nbranches = 0,
                ngeometry = 0
            };
        }

        internal Mesh2DGeometry CreateMeshGeometry()
        {
            if (!IsMemoryPinned)
            {
                PinMemory();
            }

            return new Mesh2DGeometry
            {
                nodex = GetPinnedObjectPointer(NodesX),
                nodey = GetPinnedObjectPointer(NodesY),
                
                edge_nodes = GetPinnedObjectPointer(EdgeNodes),

                facex = GetPinnedObjectPointer(FaceX),
                facey = GetPinnedObjectPointer(FaceY),
                face_nodes = GetPinnedObjectPointer(FaceNodes),

                startIndex = 0
            };
        }

        internal void InitializeWithEmptyData(Mesh2DGeometryDimensions mesh2dDimensions)
        {
            var numberOfNodes = mesh2dDimensions.numnode;
            var numberOfEdges = mesh2dDimensions.numedge;
            var numberOfFaces = mesh2dDimensions.numface;

            NodesX = new double[numberOfNodes];
            NodesY = new double[numberOfNodes];
            
            EdgeNodes = new int[numberOfEdges * 2];
            FaceNodes = new int[numberOfFaces * mesh2dDimensions.maxnumfacenodes];

            FaceX = new double[numberOfFaces];
            FaceY = new double[numberOfFaces];
        }
    }
}