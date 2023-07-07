using System.ComponentModel;

namespace Deltares.UGrid.Api
{
    /// <summary>
    /// Type of location on the mesh (node, edge etc.)
    /// </summary>
    public enum GridLocationType
    {
        /// <summary>
        /// No location on grid
        /// </summary>
        [Description("Unknown")]
        None = 0,

        /// <summary>
        /// Located on the nodes
        /// </summary>
        [Description("Node")]
        Node = 1,

        /// <summary>
        /// Located on the edges
        /// </summary>
        [Description("Edge")]
        Edge = 2,

        /// <summary>
        /// Located on the cells
        /// </summary>
        [Description("Cell")]
        Face = 4,

        /// <summary>
        /// Located on the volumes
        /// </summary>
        [Description("Volume")]
        Volume = 8,
        
        /// <summary>
        /// Located on all 2D elements (node, edge, face)
        /// </summary>
        [Description("All")]
        All2D = Node + Edge + Face
    }
}