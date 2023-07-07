using System.Runtime.Serialization.Formatters;
using Deltares.UGrid.Entities;
using Deltares.UGrid.Helpers;
using ProtoBuf;

namespace Deltares.UGrid.Api
{
    /// <summary>
    /// Class describing 1D mesh geometry (computation points) based on a 1D network
    /// (see: <see cref="Network1DGeometry"/>)
    /// </summary>
    [ProtoContract(AsReferenceDefault = true)]
    public class Disposable1DMeshGeometry : DisposableMeshObject
    {
        /// <summary>
        /// The name of the 1D mesh (always 255 characters)
        /// </summary>
        [ProtoMember(1)]
        [StringBufferSize(BufferSize = 255)]
        public string Name;

        /// <summary>
        /// X values of the 1D mesh nodes
        /// </summary>
        [ProtoMember(2)]
        public double[] NodesX;

        /// <summary>
        /// Y values of the 1D mesh nodes
        /// </summary>
        [ProtoMember(3)]
        public double[] NodesY;

        /// <summary>
        /// Branch ids of the Nodes
        /// </summary>
        [ProtoMember(4)]
        public int[] BranchIDs;

        /// <summary>
        /// Branch offsets of the Nodes
        /// </summary>
        [ProtoMember(5)]
        public double[] BranchOffsets;

        /// <summary>
        /// Ids of the nodes
        /// </summary>
        [ProtoMember(6)]
        [StringBufferSize(BufferSize = 40)]
        public string[] NodeIds;

        /// <summary>
        /// Long names of the nodes
        /// </summary>
        [ProtoMember(7)]
        [StringBufferSize(BufferSize = 80)]
        public string[] NodeLongNames;

        /// <summary>
        /// Branch ids for each edge
        /// </summary>
        [ProtoMember(8)]
        public int[] EdgeBranchIds;

        /// <summary>
        /// Nodes for every edge {[from, to], [from, to] ... }
        /// </summary>
        [ProtoMember(9)]
        public int[] EdgeNodes;

        /// <summary>
        /// Offset values of the center point of each edge
        /// </summary>
        [ProtoMember(10)]
        public double[] EdgeCenterPointOffset;

        /// <summary>
        /// X values of the center point of each edge
        /// </summary>
        [ProtoMember(11)]
        public double[] EdgeCenterPointX;

        /// <summary>
        /// Y values of the center point of each edge
        /// </summary>
        [ProtoMember(12)]
        public double[] EdgeCenterPointY;

        internal void InitializeWithEmptyData(Mesh1DGeometryDimensions dimensions)
        {
            Name = Name.ToFixedLengthString(GetType().GetBufferSize(nameof(Name)));
            NodesX = new double[dimensions.NumberOfNodes];
            NodesY = new double[dimensions.NumberOfNodes];
            NodeIds = new string[dimensions.NumberOfNodes].GetFixedLengthStringArray(GetType().GetBufferSize(nameof(NodeIds)));
            NodeLongNames = new string[dimensions.NumberOfNodes].GetFixedLengthStringArray(GetType().GetBufferSize(nameof(NodeLongNames)));

            BranchIDs = new int[dimensions.NumberOfNodes];
            BranchOffsets = new double[dimensions.NumberOfNodes];
            
            EdgeBranchIds = new int[dimensions.NumberOfEdges];
            EdgeNodes = new int[dimensions.NumberOfEdges * 2];
            EdgeCenterPointOffset = new double[dimensions.NumberOfEdges];
            EdgeCenterPointX = new double[dimensions.NumberOfEdges];
            EdgeCenterPointY = new double[dimensions.NumberOfEdges];
        }

        internal Mesh1DGeometryDimensions CreateMesh1DGeometryDimensions()
        {
            return new Mesh1DGeometryDimensions
            {
                name = Name.Replace(' ', '_').ToCharArray(),
                NumberOfNodes = NodesX?.Length ?? 0,
                NumberOfEdges = EdgeBranchIds?.Length ?? 0
            };
        }

        internal Mesh1DGeometry CreateMesh1DGeometry()
        {
            if (!IsMemoryPinned)
            {
                PinMemory();
            }

            return new Mesh1DGeometry
            {
                startIndex = 0,
                NodeX = GetPinnedObjectPointer(NodesX),
                NodeY = GetPinnedObjectPointer(NodesY),
                NodeIds = GetPinnedObjectPointer(NodeIds),
                NodeLongNames = GetPinnedObjectPointer(NodeLongNames),
                BranchIds = GetPinnedObjectPointer(BranchIDs),
                BranchOffsets = GetPinnedObjectPointer(BranchOffsets),

                EdgeBranchIds = GetPinnedObjectPointer(EdgeBranchIds),
                EdgeNodes = GetPinnedObjectPointer(EdgeNodes),
                EdgeCenterPointOffset = GetPinnedObjectPointer(EdgeCenterPointOffset),
                EdgeCenterPointX = GetPinnedObjectPointer(EdgeCenterPointX),
                EdgeCenterPointY = GetPinnedObjectPointer(EdgeCenterPointY)
            };
        }
    }
}