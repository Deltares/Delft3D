using Deltares.UGrid.Entities;
using Deltares.UGrid.Helpers;
using ProtoBuf;

namespace Deltares.UGrid.Api
{
    /// <summary>
    /// Class describing a 1D network (nodes, branches etc.)
    /// </summary>
    [ProtoContract(AsReferenceDefault = true)]
    public class DisposableNetworkGeometry : DisposableMeshObject
    {
        /// <summary>
        /// Name of the 1D network
        /// </summary>
        [ProtoMember(1)]
        [StringBufferSize(BufferSize = 255)]
        public string NetworkName = "network";

        /// <summary>
        /// X values of the 1D network nodes
        /// </summary>
        [ProtoMember(2)]
        public double[] NodesX;

        /// <summary>
        /// Y values of the 1D network nodes
        /// </summary>
        [ProtoMember(3)]
        public double[] NodesY;

        /// <summary>
        /// Ids for all the nodes
        /// </summary>
        [ProtoMember(4)]
        [StringBufferSize(BufferSize = 40)]
        public string[] NodeIds;

        /// <summary>
        /// Long name for all the nodes
        /// </summary>
        [ProtoMember(5)]
        [StringBufferSize(BufferSize = 80)]
        public string[] NodeLongNames;

        /// <summary>
        /// Length for each branch
        /// </summary>
        [ProtoMember(6)]
        public double[] BranchLengths;

        /// <summary>
        /// End node id for each branch
        /// </summary>
        [ProtoMember(7)]
        public int[] NodesTo;

        /// <summary>
        /// Begin node id for each branch
        /// </summary>
        [ProtoMember(8)]
        public int[] NodesFrom;
        
        /// <summary>
        /// Branch order for each branch
        /// </summary>
        [ProtoMember(9)]
        public int[] BranchOrder;

        /// <summary>
        /// Ids for all the branches
        /// </summary>
        [ProtoMember(10)]
        [StringBufferSize(BufferSize = 40)]
        public string[] BranchIds;

        /// <summary>
        /// Long name for all the branches
        /// </summary>
        [ProtoMember(11)]
        [StringBufferSize(BufferSize = 80)]
        public string[] BranchLongNames;

        /// <summary>
        /// Type for all the branches
        /// </summary>
        [ProtoMember(12)]
        public int[] BranchTypes;

        /// <summary>
        /// Number of nodes(coordinates) for each branch geometry
        /// </summary>
        [ProtoMember(13)]
        public int[] BranchGeometryNodesCount;

        /// <summary>
        /// All x values of all branch geometry points
        /// </summary>
        [ProtoMember(14)]
        public double[] BranchGeometryX;

        /// <summary>
        /// All y values of all branch geometry points
        /// </summary>
        [ProtoMember(15)]
        public double[] BranchGeometryY;

        internal void InitializeWithEmptyData(Network1DGeometryDimensions dimensions)
        {
            var numberOfNodes = dimensions.NumberOfNodes;
            var numberOfBranches = dimensions.NumberOfBranches;
            var totalBranchGeometryPoints = dimensions.NumberOfBranchGeometryPoints;

            NodesX = new double[numberOfNodes];
            NodesY = new double[numberOfNodes];
            NodeIds = new string[numberOfNodes].GetFixedLengthStringArray(GetType().GetBufferSize(nameof(NodeIds)));
            NodeLongNames = new string[numberOfNodes].GetFixedLengthStringArray(GetType().GetBufferSize(nameof(NodeLongNames)));

            BranchLengths = new double[numberOfBranches];
            BranchTypes = new int[numberOfBranches];
            BranchGeometryNodesCount = new int[numberOfBranches];
            BranchOrder = new int[numberOfBranches];
            BranchIds = new string[numberOfBranches].GetFixedLengthStringArray(GetType().GetBufferSize(nameof(BranchIds)));
            BranchLongNames = new string[numberOfBranches].GetFixedLengthStringArray(GetType().GetBufferSize(nameof(BranchLongNames)));

            NodesTo = new int[numberOfBranches];
            NodesFrom = new int[numberOfBranches];

            BranchGeometryX = new double[totalBranchGeometryPoints];
            BranchGeometryY = new double[totalBranchGeometryPoints];
        }

        internal Network1DGeometryDimensions CreateNetwork1DGeometryDimensions()
        {
            return new Network1DGeometryDimensions
            {
                NumberOfNodes = NodesX.Length, 
                NumberOfBranches = BranchIds.Length,
                NumberOfBranchGeometryPoints = BranchGeometryX.Length
            };
        }

        internal Network1DGeometry CreateNetwork1DGeometry()
        {
            if (!IsMemoryPinned)
            {
                PinMemory();
            }

            return new Network1DGeometry
            {
                NodeX =  GetPinnedObjectPointer(NodesX),
                NodeY = GetPinnedObjectPointer(NodesY),
                NodeIds = GetPinnedObjectPointer(NodeIds),
                NodeLongNames = GetPinnedObjectPointer(NodeLongNames),

                BranchGeometryCount = GetPinnedObjectPointer(BranchGeometryNodesCount),
                BranchTypes = GetPinnedObjectPointer(BranchTypes),
                BranchOrder = GetPinnedObjectPointer(BranchOrder),
                BranchLengths = GetPinnedObjectPointer(BranchLengths),
                BranchIds = GetPinnedObjectPointer(BranchIds),
                BranchLongNames = GetPinnedObjectPointer(BranchLongNames),
                SourceNodes = GetPinnedObjectPointer(NodesFrom),
                TargetNodes = GetPinnedObjectPointer(NodesTo),

                BranchGeometryX = GetPinnedObjectPointer(BranchGeometryX),
                BranchGeometryY = GetPinnedObjectPointer(BranchGeometryY),
            };
        }
    }
}