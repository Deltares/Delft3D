namespace Deltares.UGrid.Api
{
    /// <summary>
    /// Type of UGrid mesh
    /// </summary>
    public enum UGridMeshType
    {
        /// <summary>
        /// Combined mesh (1D/2D/3D)
        /// </summary>
        Combined = 0,
        
        /// <summary>
        /// A 1D mesh
        /// </summary>
        Mesh1D = 1,

        /// <summary>
        /// A 2D mesh
        /// </summary>
        Mesh2D = 2,

        /// <summary>
        /// A 3D mesh
        /// </summary>
        Mesh3D = 3
    }
}