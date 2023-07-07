using System;
using System.Runtime.InteropServices;
using System.Text;

namespace Deltares.UGrid.Entities
{
    /// <summary>
    /// Class containing all static imports for IONetCdf library
    /// </summary>
    /// <remarks>
    /// This class contains unused functions and functions that are
    /// not documented properly. This is because the native dll
    /// will be rewritten to use structures instead of separate function
    /// calls to get/set meshes, networks, links etc.</remarks>
    internal static class IoNetCfdImports
    {
        internal const int NoErrorCode = 0;
        internal static int VariableNotFoundErrorCode = -49;

        internal const int NF90_INT = 4;
        internal const int NF90_DOUBLE = 6;
        internal const double DEFAULT_FILL_VALUE = -999.0;
        internal const int DEFAULT_FILL_VALUE_INT = -999;

        internal const string GRIDDLL_NAME = "io_netcdf.dll";

        #region Generic

        #region IO

        /// <summary>
        /// Tries to open a NetCDF file and initialize based on its specified conventions.
        /// </summary>
        /// <param name="c_path">File name for netCDF dataset to be opened.</param>
        /// <param name="mode">NetCDF open mode, e.g. NF90_NOWRITE.</param>
        /// <param name="ioncid">The io_netcdf dataset id (this is not the NetCDF ncid, which is stored in datasets(ioncid)%ncid.</param>
        /// <param name="iconvtype">The detected conventions in the file.</param>
        /// <param name="convversion"></param>
        /// <returns>Result status (NoErrorCode if successful).</returns>
        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_open", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_open_dll([In] string c_path, [In] ref int mode, [In, Out] ref int ioncid, [In, Out] ref int iconvtype, ref double convversion);

        /// <summary>
        /// Tries to close an open io_netcdf data set.
        /// </summary>
        /// <param name="ioncid">The io_netcdf dataset id (this is not the NetCDF ncid, which is stored in datasets(ioncid)%ncid.</param>
        /// <returns>Result status (NoErrorCode if successful).</returns>
        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_close", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_close_dll([In] ref int ioncid);


        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_write_geom_ugrid", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_write_geom_ugrid_dll(string filename);

        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_write_map_ugrid", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_write_map_ugrid_dll(string filename);

        /// <summary>
        /// Adds meta data about modelName, version etc.
        /// </summary>
        /// <param name="ioncid"></param>
        /// <param name="metadata"></param>
        /// <returns></returns>
        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_add_global_attributes", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_add_global_attributes_dll([In] ref int ioncid, ref InteropMetadata metadata);

        /// <summary>
        /// This function creates a new netCDF file for writing
        /// </summary>
        /// <param name="c_path">The path where the file will be created (in)</param>
        /// <param name="mode"> The netCDF opening mode (in)</param>
        /// <param name="ioncid">The netCDF file id (out)</param>
        /// <returns></returns>
        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_create", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_create_dll([In] string c_path, [In] ref int mode, [In, Out] ref int ioncid);

        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_initialize", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_initialize_dll(IO_NetCDF_Message_Callback c_message_callback, IO_NetCDF_Progress_Callback c_progress_callback);

        #endregion

        #region Convention checks

        /// <summary>
        /// Checks whether the specified data set adheres to a specific set of conventions.
        /// Datasets may adhere to multiple conventions at the same time, so use this method
        /// to check for individual conventions.
        /// </summary>
        /// <param name="ioncid">File Id</param>
        /// <param name="iconvtype">The NetCDF conventions type to check for.</param>
        /// <returns>Whether or not the file adheres to the specified conventions.</returns>
        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_adheresto_conventions", CallingConvention = CallingConvention.Cdecl)]
        public static extern bool ionc_adheresto_conventions_dll(ref int ioncid, ref int iconvtype);

        /// <summary>
        /// Inquire the NetCDF conventions used in the dataset.
        /// </summary>
        /// <param name="ioncid">The IONC data set id.</param>
        /// <param name="iconvtype">The NetCDF conventions type of the dataset.</param>
        /// <param name="convversion"></param>
        /// <returns>Result status, ionc_noerr if successful.</returns>
        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_inq_conventions", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_inq_conventions_dll(ref int ioncid, ref int iconvtype, ref double convversion);

        #endregion

        #region Mesh

        /// <summary>
        /// Clone the definitions specific mesh from one netCDF file to another netCDF. 
        /// Clones all related attributes of the mesh, but it can not clone mesh contacts yet!
        /// </summary>
        /// <param name="ncidin">The input netCDF file id containing the mesh to clone (in)</param>
        /// <param name="ncidout">The output netCDF file id, can be empty/not empty (in)</param>
        /// <param name="meshidin">The mesh id to copy (in)</param>
        /// <param name="meshidout">The id of the cloned mesh in the output file (out)</param>
        /// <returns></returns>
        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_clone_mesh_definition", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_clone_mesh_definition_dll([In] ref int ncidin, [In] ref int ncidout, [In] ref int meshidin, [In, Out] ref int meshidout);

        /// <summary>
        /// Clone the data of a specific mesh from one netCDF file to another netCDF
        /// </summary>
        /// <param name="ncidin">The input netCDF file id containing the mesh to clone (in)</param>
        /// <param name="ncidout">The output netCDF file id, can be empty/not empty (in)</param>
        /// <param name="meshidin">The mesh id to copy (in)</param>
        /// <param name="meshidout">The id of the cloned mesh in the output file (out)</param>
        /// <returns></returns>
        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_clone_mesh_data", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_clone_mesh_data_dll([In] ref int ncidin, [In] ref int ncidout, [In] ref int meshidin, [In] ref int meshidout);

        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_def_mesh_ids", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_def_mesh_ids_dll([In] ref int ioncid, [In] ref int meshid, [In] ref int iloctype);

        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_put_var_chars", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_put_var_chars_dll([In] ref int ioncid, [In] ref int meshid, [MarshalAs(UnmanagedType.LPStr)][In, Out] StringBuilder varname, [In] ref IntPtr ids, [In] ref IntPtr longNames, [In] ref int nvalues);

        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_get_var_chars", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_get_var_chars_dll([In] ref int ioncid, [In] ref int meshid, [MarshalAs(UnmanagedType.LPStr)][In, Out] StringBuilder varname, [In, Out] ref IntPtr ids, [In, Out] ref IntPtr longNames, [In] ref int nvalues);

        /// <summary>
        /// Gets the number of mesh from a data set.
        /// </summary>
        /// <param name="ioncid">The IONC data set id.</param>
        /// <param name="nmesh">Number of meshes.</param>
        /// <returns>Result status (NoErrorCode if successful).</returns>
        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_get_mesh_count", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_get_mesh_count_dll([In, Out] ref int ioncid, [In, Out] ref int nmesh);

        /// <summary>
        /// Gets the number of meshes
        /// </summary>
        /// <param name="ioncid"></param>
        /// <param name="meshType"> Mesh type: 0 = any type, 1 = 1D mesh, 2 = 2D mesh, 3 = 3D mesh </param>
        /// <param name="numMeshes"></param>
        /// <returns></returns>
        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_get_number_of_meshes", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_get_number_of_meshes_dll([In] ref int ioncid, [In] ref int meshType, [In, Out] ref int numMeshes);

        /// <summary>
        /// Gets the mesh ids 
        /// </summary>
        /// <param name="ioncid"></param>
        /// <param name="meshType"></param>
        /// <param name="pointerToMeshIds"></param>
        /// <param name="nnumNetworks"></param>
        /// <returns></returns>
        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_ug_get_mesh_ids", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_ug_get_mesh_ids_dll([In] ref int ioncid, [In] ref int meshType, [In, Out] ref IntPtr pointerToMeshIds, [In] ref int nnumNetworks);

        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_get_coordinate_system", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_get_coordinate_system_dll([In] ref int ioncid, [In, Out] ref int epsgcode);

        /// <summary>
        /// Gets the name of mesh from a data set.
        /// </summary>
        /// <param name="ioncid">The IONC data set id.</param>
        /// <param name="meshId">Mesh id.</param>
        /// <param name="meshName">The mesh name.</param>
        /// <returns>Result status (NoErrorCode if successful).</returns>
        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_get_mesh_name", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_get_mesh_name_dll([In, Out] ref int ioncid, [In, Out] ref int meshId, [MarshalAs(UnmanagedType.LPStr)][In, Out] StringBuilder meshName);

        /// <summary>
        /// Gets the number of nodes in a single mesh from a data set.
        /// </summary>
        /// <param name="ioncid">The IONC data set id.</param>
        /// <param name="meshId">The mesh id in the specified data set.</param>
        /// <param name="nnode">Number of nodes.</param>
        /// <returns>Result status (NoErrorCode if successful).</returns>
        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_get_node_count", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_get_node_count_dll(ref int ioncid, ref int meshId, ref int nnode);

        /// <summary>
        /// Gets the number of edges in a single mesh from a data set.
        /// </summary>
        /// <param name="ioncid">The IONC data set id.</param>
        /// <param name="meshId">The mesh id in the specified data set.</param>
        /// <param name="nedge">Number of edges.</param>
        /// <returns>Result status (NoErrorCode if successful).</returns>
        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_get_edge_count", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_get_edge_count_dll(ref int ioncid, ref int meshId, ref int nedge);

        /// <summary>
        /// Gets the number of faces in a single mesh from a data set.
        /// </summary>
        /// <param name="ioncid">The IONC data set id.</param>
        /// <param name="meshId">The mesh id in the specified data set.</param>
        /// <param name="nface">Number of faces.</param>
        /// <returns>Result status (NoErrorCode if successful).</returns>
        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_get_face_count", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_get_face_count_dll(ref int ioncid, ref int meshId, ref int nface);

        /// <summary>
        /// Gets the maximum number of nodes for any face in a single mesh from a data set.
        /// </summary>
        /// <param name="ioncid">The IONC data set id.</param>
        /// <param name="meshId">The mesh id in the specified data set.</param>
        /// <param name="nmaxfacenodes">The maximum number of nodes per face in the mesh.Number of faces.</param>
        /// <returns>Result status (NoErrorCode if successful).</returns>
        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_get_max_face_nodes", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_get_max_face_nodes_dll(ref int ioncid, ref int meshId, ref int nmaxfacenodes);

        /// <summary>
        /// Gets the x,y coordinates for all nodes in a single mesh from a data set.
        /// </summary>
        /// <param name="ioncid">The IONC data set id.</param>
        /// <param name="meshId">The mesh id in the specified data set.</param>
        /// <param name="c_xptr">Pointer to array for x-coordinates</param>
        /// <param name="c_yptr">Pointer to array for y-coordinates</param>
        /// <param name="nnode">The number of nodes in the mesh.</param>
        /// <returns>Result status (NoErrorCode if successful).</returns>
        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_get_node_coordinates", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_get_node_coordinates_dll([In] ref int ioncid, [In, Out] ref int meshId, [In, Out] ref IntPtr c_xptr, [In, Out]ref IntPtr c_yptr, [In, Out] ref int nnode);

        /// <summary>
        /// Gets the edge-node connectvit table for all edges in the specified mesh.
        /// The output edge_nodes array is supposed to be of exact correct size already.
        /// </summary>
        /// <param name="ioncid">The IONC data set id.</param>
        /// <param name="meshId">The mesh id in the specified data set.</param>
        /// <param name="c_edge_nodes_ptr">Pointer to array for the edge-node connectivity table.</param>
        /// <param name="nedge">The number of edges in the mesh.</param>
        /// <param name="startIndex"></param>
        /// <returns>Result status (NoErrorCode if successful).</returns>
        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_get_edge_nodes", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_get_edge_nodes_dll(ref int ioncid, ref int meshId, ref IntPtr c_edge_nodes_ptr, ref int nedge, ref int startIndex);

        /// <summary>
        /// Gets the face-node connectvit table for all faces in the specified mesh.
        /// The output face_nodes array is supposed to be of exact correct size already.
        /// </summary>
        /// <param name="ioncid">The IONC data set id.</param>
        /// <param name="meshId">The mesh id in the specified data set.</param>
        /// <param name="c_face_nodes_ptr">Pointer to array for the face-node connectivity table.</param>
        /// <param name="nface">The number of faces in the mesh.</param>
        /// <param name="nmaxfacenodes">The maximum number of nodes per face in the mesh.</param>
        /// <param name="fillvalue"></param>
        /// <param name="startIndex"></param>
        /// <returns>Result status (NoErrorCode if successful).</returns>
        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_get_face_nodes", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_get_face_nodes_dll(ref int ioncid, ref int meshId, ref IntPtr c_face_nodes_ptr, ref int nface, ref int nmaxfacenodes, ref int fillvalue, ref int startIndex);

        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_put_node_coordinates", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_put_node_coordinates_dll(ref int ioncid, ref int meshId, ref IntPtr c_xvalues_ptr, ref IntPtr c_yvalues_ptr, ref int nNode);

        #endregion

        #region Variables

        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_get_var_count", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_get_var_count_dll([In] ref int ioncid, [In] ref int mesh, [In] ref int location, [In, Out] ref int nCount);

        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_inq_varid", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_inq_varid_dll(ref int ioncid, ref int meshId, string varName, ref int varId);

        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_inq_varid_by_standard_name", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_inq_varid_by_standard_name_dll(ref int ioncid, ref int meshId, ref int location, string standardName, ref int varId);

        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_inq_varids", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_inq_varids_dll(ref int ioncid, ref int meshId, ref int location, ref IntPtr ptr, ref int nVar);

        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_get_var", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_get_var_dll(ref int ioncid, ref int meshId, ref int location, string varname, ref IntPtr c_zptr, ref int nNode, ref double c_fillvalue);

        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_put_var", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_put_var_dll(ref int ioncid, ref int meshId, ref int iloctype, string c_varname, ref IntPtr c_values_ptr, ref int nVal);

        #endregion
        
        #region Other

        /// <summary>
        /// Get the id of the 3d computational mesh
        /// </summary>
        /// <param name="ioncid">The IONC data set id.</param>
        /// <param name="meshId">The 3d computational mesh id (out)</param>
        /// <returns></returns>
        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_get_3d_mesh_id", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_get_3d_mesh_id_dll([In] ref int ioncid, [In, Out] ref int meshId);

        #endregion

        #endregion

        #region 1D calls

        #region Network

        //-Get the network id for a specified mesh

        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_get_contact_id", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_get_contact_id_dll([In] ref int ioncid, [In] ref int contactId);

        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_get_network_id_from_mesh_id", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_get_network_id_from_mesh_id_dll([In] ref int ioncid, [In] ref int meshId, [In, Out] ref int networkid);

        /// <summary>
        /// Gets the number of networks
        /// </summary>
        /// <param name="ioncid"></param>
        /// <param name="nnumNetworks"></param>
        /// <returns></returns>
        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_get_number_of_networks", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_get_number_of_networks_dll([In] ref int ioncid, [In, Out] ref int nnumNetworks);

        /// <summary>
        /// Get the network ids
        /// </summary>
        /// <param name="ncidin"></param>
        /// <param name="c_networkids"></param>
        /// <param name="nnumNetworks"></param>
        /// <returns></returns>
        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_get_network_ids", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_get_network_ids_dll([In] ref int ncidin, [In, Out] ref IntPtr c_networkids, [In] ref int nnumNetworks);

        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_get_1d_network_name", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_get_network_name_dll([In] ref int ncidin, [In] ref int networkId, [MarshalAs(UnmanagedType.LPStr)][In, Out] StringBuilder networkName);

        /// <summary>
        /// Defines a new variable on a 1D network in an IONetCDF data set and sets up proper meta-attributes.
        /// </summary>
        /// <remarks>
        /// File should still be in define mode. Does not write the actual data yet.
        /// </remarks>
        /// <param name="ioncid">The IONetCDF data set id (in).</param>
        /// <param name="meshId">Id of the mesh</param>
        /// <param name="networkId">The network id in the specified data set (in).</param>
        /// <param name="varId">The id of the NetCDF variable to define.</param>
        /// <param name="type">The variable type expressed in one of the basic nf90_* types (in). Should match with int fillValueInt (i.e., nf90_integer).</param>
        /// <param name="locType">Specifies at which unique mesh location data will be specified (in).</param>
        /// <param name="varName">The name of the new variable (in).</param>
        /// <param name="standardName">Standard name (CF-compliant) for 'standard_name' attribute in this variable (in).</param>
        /// <param name="longName">Long name for 'long_name' attribute in this variable (use empty string if not applicable) (in).</param>
        /// <param name="unit">Unit of this variable (CF-compliant) (use empty string for dimensionless quantities) (in).</param>
        /// <param name="fillValueInt">Int precision fill value (in).</param>
        /// <param name="fillValue">Double precision fill value (in).</param>
        /// <returns>Result status (UG_NOERR==NF90_NOERR if successful).</returns>
        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_def_var", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_def_var_dll(ref int ioncid, ref int meshId, ref int networkId, ref int varId, ref int type, ref int locType, string varName, string standardName, string longName, string unit, ref int fillValueInt, ref double fillValue);
        
        /// <summary>
        /// Get the id of the geometry network.
        /// </summary>
        /// <param name="ioncid">The IONC data set id (in)</param>
        /// <param name="networkid">The geometry mesh (out)</param>
        /// <returns></returns>
        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_get_1d_network_id", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_get_1d_network_id_dll([In] ref int ioncid, [In, Out] ref int networkid);

        /// <summary>
        /// Create a 1d network in an opened netCDF file  
        /// </summary>
        /// <param name="ioncid">The netCDF file id (in)</param>
        /// <param name="networkid">The network id (out)</param>
        /// <param name="networkName">The network name (in) </param>
        /// <param name="nNodes">The number of network nodes (in) </param>
        /// <param name="nBranches">The number of network branches (in)</param>
        /// <param name="nGeometry">The number of geometry points (in)</param>
        /// <returns></returns>
        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_create_1d_network", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_create_1d_network_dll([In] ref int ioncid, [In, Out] ref int networkid, [In] string networkName, [In] ref int nNodes, [In] ref int nBranches, [In] ref int nGeometry);

        /// <summary>
        /// Write the coordinates of the network nodes
        /// </summary>
        /// <param name="ioncid">The netCDF file id (in)</param>
        /// <param name="networkid">The network id (in)</param>
        /// <param name="c_nodesX">The x coordinates of the network nodes (in)</param>
        /// <param name="c_nodesY">The y coordinates of the network nodes (in)</param>
        /// <param name="nodesinfo">The network infos (in)</param>
        /// <param name="nNodes">The number of network nodes (in)</param>
        /// <returns></returns>
        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_write_1d_network_nodes", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_write_1d_network_nodes_dll([In] ref int ioncid, [In] ref int networkid, [In] ref IntPtr c_nodesX, [In] ref IntPtr c_nodesY, [In] ref IntPtr ids, [In] ref IntPtr longNames, [In] ref int nNodes);

        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_write_1d_network_nodes_v1", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_write_1d_network_nodes_v1_dll([In] ref int ioncid, [In] ref int networkid, [In] ref IntPtr c_nodesX, [In] ref IntPtr c_nodesY, [In] ref IntPtr ids, [In] ref IntPtr longNames, [In] ref int nNodes);

        /// <summary>
        /// Write the coordinates of the network branches
        /// </summary>
        /// <param name="ioncid">The netCDF file id (in)</param>
        /// <param name="networkid">The network id (in)</param>
        /// <param name="c_sourcenodeid">The source node id (in)</param>
        /// <param name="c_targetnodeid">The target node id (in)</param>
        /// <param name="branchinfo">The branch info (in)</param>
        /// <param name="c_branchlengths">The branch lengths (in)</param>
        /// <param name="c_nbranchgeometrypoints">The number of geometry points in each branch (in)</param>
        /// <param name="nBranches">The number of branches (in)</param>
        /// <param name="startIndex"></param>
        /// <returns></returns>
        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_put_1d_network_branches", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_put_1d_network_branches_dll([In] ref int ioncid, [In] ref int networkid, [In] ref IntPtr c_sourcenodeid, [In] ref IntPtr c_targetnodeid, [In] ref IntPtr ids, [In] ref IntPtr longNames, [In] ref IntPtr c_branchlengths, [In] ref IntPtr c_nbranchgeometrypoints, [In] ref int nBranches, [In] ref int startIndex);

        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_put_1d_network_branches_v1", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_put_1d_network_branches_v1_dll([In] ref int ioncid, [In] ref int networkid, [In] ref IntPtr c_sourcenodeid, [In] ref IntPtr c_targetnodeid, [In] ref IntPtr ids, [In] ref IntPtr longNames, [In] ref IntPtr c_branchlengths, [In] ref IntPtr c_nbranchgeometrypoints, [In] ref int nBranches, [In] ref int startIndex);

        /// <summary>
        /// Writes the branch geometry (the geometry points)  
        /// </summary>
        /// <param name="ioncid">The netCDF file id (in)</param>
        /// <param name="networkid">The network id (in)</param>
        /// <param name="c_geopointsX">The x coordinates of the geometry points (in)</param>
        /// <param name="c_geopointsY">The y coordinates of the geometry points (in)</param>
        /// <param name="nGeometry">The number of geometry points (in)</param>
        /// <returns></returns>
        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_write_1d_network_branches_geometry", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_write_1d_network_branches_geometry_dll([In] ref int ioncid, [In] ref int networkid, [In] ref IntPtr c_geopointsX, [In] ref IntPtr c_geopointsY, [In] ref int nGeometry);

        /// <summary>
        /// Get the number of network nodes
        /// </summary>
        /// <param name="ioncid">The netCDF file id (in)</param>
        /// <param name="networkid">The network id (in)</param>
        /// <param name="nNodes">The number of nodes(out)</param>
        /// <returns></returns>
        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_get_1d_network_nodes_count", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_get_1d_network_nodes_count_dll([In] ref int ioncid, [In] ref int networkid, [In, Out] ref int nNodes);

        /// <summary>
        /// Get the number of branches
        /// </summary>
        /// <param name="ioncid">The netCDF file id (in)</param>
        /// <param name="networkid">The network id (in)</param>
        /// <param name="nBranches">The number of branches (out)</param>
        /// <returns></returns>
        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_get_1d_network_branches_count", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_get_1d_network_branches_count_dll([In] ref int ioncid, [In] ref int networkid, [In, Out] ref int nBranches);

        /// <summary>
        /// Get the number of geometry points for all branches
        /// </summary>
        /// <param name="ioncid">The netCDF file id (in)</param>
        /// <param name="networkid">The network id (in)</param>
        /// <param name="ngeometrypoints">The number of geometry points for all branches (out)</param>
        /// <returns></returns>
        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_get_1d_network_branches_geometry_coordinate_count", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_get_1d_network_branches_geometry_coordinate_count_dll([In] ref int ioncid, [In] ref int networkid, [In, Out] ref int ngeometrypoints);

        /// <summary>
        /// Read the node coordinates and the charinfo
        /// </summary>
        /// <param name="ioncid">The netCDF file id</param>
        /// <param name="networkid">The network id (in)</param>
        /// <param name="c_nodesX">The x coordinates of the network nodes (out)</param>
        /// <param name="c_nodesY">The y coordinates of the network nodes (out)</param>
        /// <param name="nodesinfo">The network infos (out)</param>
        /// <param name="nNodes">The number of network nodes (in)</param>
        /// <returns></returns>
        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_read_1d_network_nodes", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_read_1d_network_nodes_dll([In] ref int ioncid, [In] ref int networkid, [In, Out] ref IntPtr c_nodesX, [In, Out] ref IntPtr c_nodesY, [In] ref IntPtr ids, [In] ref IntPtr longNames, [In] ref int nNodes);

        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_read_1d_network_nodes_v1", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_read_1d_network_nodes_v1_dll([In] ref int ioncid, [In] ref int networkid, [In, Out] ref IntPtr c_nodesX, [In, Out] ref IntPtr c_nodesY, [In] ref IntPtr ids, [In] ref IntPtr longNames, [In] ref int nNodes);

        /// <summary>
        /// Read the coordinates of the network branches
        /// </summary>
        /// <param name="ioncid">The netCDF file id</param>
        /// <param name="networkid">The network id (in)</param>
        /// <param name="c_sourcenodeid">The source node id (out)</param>
        /// <param name="c_targetnodeid">The target node id (out)</param>
        /// <param name="c_branchlengths">The branch lengths (out)</param>
        /// <param name="branchinfo">The branch info (out)</param>
        /// <param name="c_nbranchgeometrypoints">he number of geometry points in each branch (out)</param>
        /// <param name="nBranches">The number of branches (in)</param>
        /// <param name="startIndex"></param>
        /// <returns></returns>
        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_get_1d_network_branches", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_get_1d_network_branches_dll([In] ref int ioncid, [In] ref int networkid, [In, Out] ref IntPtr c_sourcenodeid, [In, Out] ref IntPtr c_targetnodeid, [In, Out] ref IntPtr c_branchlengths, [In] ref IntPtr ids, [In] ref IntPtr longNames, [In, Out] ref IntPtr c_nbranchgeometrypoints, [In] ref int nBranches, [In] ref int startIndex);

        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_get_1d_network_branches_v1", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_get_1d_network_branches_v1_dll([In] ref int ioncid, [In] ref int networkid, [In, Out] ref IntPtr c_sourcenodeid, [In, Out] ref IntPtr c_targetnodeid, [In, Out] ref IntPtr c_branchlengths, [In, Out] ref IntPtr ids, [In, Out] ref IntPtr longNames, [In, Out] ref IntPtr c_nbranchgeometrypoints, [In] ref int nBranches, [In] ref int startIndex);

        /// <summary>
        /// Reads the branch geometry
        /// </summary>
        /// <param name="ioncid">The netCDF file id</param>
        /// <param name="networkid">The network id (in)</param>
        /// <param name="c_geopointsX">The x coordinates of the geometry points (out)</param>
        /// <param name="c_geopointsY">The y coordinates of the geometry points (out)</param>
        /// <param name="nGeometrypoints">The number of nodes (in)</param>
        /// <returns></returns>
        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_read_1d_network_branches_geometry", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_read_1d_network_branches_geometry_dll([In] ref int ioncid, [In] ref int networkid, [In, Out] ref IntPtr c_geopointsX, [In, Out] ref IntPtr c_geopointsY, [In] ref int nGeometrypoints);

        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_put_1d_network_branchorder", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_put_1d_network_branchorder_dll([In] ref int ioncId, [In] ref int networkId, [In] ref IntPtr pointerToBranchOrder, [In] ref int numberOfBranches);

        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_put_1d_network_branchtype", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_put_1d_network_branchtype_dll([In] ref int ioncId, [In] ref int networkId, [In] ref IntPtr pointerTobranchtype, [In] ref int numberOfBranches);

        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_get_1d_network_branchorder", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_get_1d_network_branchorder_dll([In] ref int ioncId, [In] ref int networkId, [In, Out] ref IntPtr pointerToBranchOrder, [In] ref int numberOfBranches);

        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_get_1d_network_branchtype", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_get_1d_network_branchtype_dll([In] ref int ioncId, [In] ref int networkId, [In, Out] ref IntPtr pointerToBranchOrder, [In] ref int numberOfBranches);

        #endregion

        #region Mesh1D

        /// <summary>
        /// Get the id of the 1d computational mesh
        /// </summary>
        /// <param name="ioncid">The IONC data set id.</param>
        /// <param name="meshId">The 1d computational mesh id (out)</param>
        /// <returns></returns>
        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_get_1d_mesh_id", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_get_1d_mesh_id_dll([In] ref int ioncid, [In, Out] ref int meshId);

        /// <summary>
        /// Writes a 1d mesh. The geometrical features (e.g. the branches and geometry points) are described in the network above
        /// </summary>
        /// <param name="ioncid">The netCDF file id (in)</param>
        /// <param name="networkname">The network name</param>
        /// <param name="meshId">The mesh id (out)</param>
        /// <param name="meshname">The mesh name (in)</param>
        /// <param name="nmeshpoints">The number of mesh points (in)</param>
        /// <returns></returns>
        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_create_1d_mesh", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_create_1d_mesh_dll([In] ref int ioncid, [In] string networkname, [In, Out] ref int meshId, [In] string meshname, [In] ref int nmeshpoints);

        /// <summary>
        /// Writes a 1d mesh. The geometrical features (e.g. the branches and geometry points) are described in the network above
        /// </summary>
        /// <param name="ioncid">The netCDF file id (in)</param>
        /// <param name="networkname">The network name</param>
        /// <param name="meshId">The mesh id (out)</param>
        /// <param name="meshname">The mesh name (in)</param>
        /// <param name="nmeshpoints">The number of mesh points (in)</param>
        /// <param name="writexy"></param>
        /// <returns></returns>
        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_create_1d_mesh_v1", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_create_1d_mesh_v1_dll([In] ref int ioncid, [In] string networkname, [In, Out] ref int meshId, [In] string meshname, [In] ref int nmeshpoints, [In] ref int nmeshedges, [In, Out] ref int writexy);

        /// <summary>
        /// Writes the mesh coordinates points 
        /// </summary>
        /// <param name="ioncid">The netCDF file id (in)</param>
        /// <param name="meshid">dataset mesh 1d id</param>
        /// <param name="c_branchidx">The branch id for each mesh point (in)</param>
        /// <param name="c_offset">The offset along the branch from the starting point (in)</param>
        /// <param name="nodeinfo">The node info (in)</param>
        /// <param name="nmeshpoints">The number of mesh points (in)</param>
        /// <param name="startIndex">array start index</param>
        /// <returns></returns>
        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_put_1d_mesh_discretisation_points", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_put_1d_mesh_discretisation_points_dll([In] ref int ioncid, [In] ref int meshid, [In] ref IntPtr c_branchidx, [In] ref IntPtr c_offset, [In] ref IntPtr ids, [In] ref IntPtr longNames, [In] ref int nmeshpoints, [In] ref int startIndex);

        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_put_1d_mesh_edges", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_put_1d_mesh_edges_dll([In] ref int ioncid, [In] ref int meshid, [In] ref IntPtr c_edgebranchidx, [In] ref IntPtr c_edgeoffset, [In] ref int nmeshedges, [In] ref int startIndex);

        /// <summary>
        /// Writes the mesh coordinates points 
        /// </summary>
        /// <param name="ioncid">The netCDF file id (in)</param>
        /// <param name="meshid">dataset mesh 1d id</param>
        /// <param name="c_branchidx">The branch id for each mesh point (in)</param>
        /// <param name="c_offset">The offset along the branch from the starting point (in)</param>
        /// <param name="nodeinfo">The node info (in)</param>
        /// <param name="nmeshpoints">The number of mesh points (in)</param>
        /// <param name="startIndex">array start index</param>
        /// <param name="c_coordx"></param>
        /// <param name="c_coordy"></param>
        /// <returns></returns>
        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_put_1d_mesh_discretisation_points_v1", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_put_1d_mesh_discretisation_points_v1_dll([In] ref int ioncid, [In] ref int meshid, [In] ref IntPtr c_branchidx, [In] ref IntPtr c_offset, [In] ref IntPtr ids, [In] ref IntPtr longNames, [In] ref int nmeshpoints, [In] ref int startIndex, [In] ref IntPtr c_coordx, [In] ref IntPtr c_coordy);

        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_put_1d_mesh_discretisation_points_v2", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_put_1d_mesh_discretisation_points_v2_dll([In] ref int ioncid, [In] ref int meshid, [In] ref IntPtr c_branchidx, [In] ref IntPtr c_offset, [In] ref IntPtr ids, [In] ref IntPtr longNames, [In] ref int nmeshpoints, [In] ref int startIndex, [In] ref IntPtr c_coordx, [In] ref IntPtr c_coordy);

        /// <summary>
        /// Get the number of mesh discretization points 
        /// </summary>
        /// <param name="ioncid">The netCDF file id (in)</param>
        /// <param name="meshId">The mesh id (in)</param>
        /// <param name="nmeshpoints">The number of mesh points (out)</param>
        /// <returns></returns>
        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_get_1d_mesh_discretisation_points_count", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_get_1d_mesh_discretisation_points_count_dll([In] ref int ioncid, [In] ref int meshId, [In, Out] ref int nmeshpoints);

        /// <summary>
        /// Read the coordinates of the mesh points  
        /// </summary>
        /// <param name="ioncid">The netCDF file id (in)</param>
        /// <param name="meshId">The mesh id (in)</param>
        /// <param name="c_branchidx">The branch id for each mesh point (out)</param>
        /// <param name="c_offset">The offset along the branch from the starting point (out)</param>
        /// <param name="nmeshpoints">The number of mesh points (in)</param>
        /// <param name="startIndex"></param>
        /// <returns></returns>
        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_get_1d_mesh_discretisation_points", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_get_1d_mesh_discretisation_points_dll([In] ref int ioncid, [In] ref int networkid, [In, Out] ref IntPtr c_branchidx, [In, Out] ref IntPtr c_offset, [In, Out] ref IntPtr ids, [In, Out] ref IntPtr longNames, [In] ref int nmeshpoints, [In] ref int startIndex);

        /// <summary>
        /// Read the coordinates of the mesh points  
        /// </summary>
        /// <param name="ioncid">The netCDF file id (in)</param>
        /// <param name="meshId">The mesh id (in)</param>
        /// <param name="c_branchidx">The branch id for each mesh point (out)</param>
        /// <param name="c_offset">The offset along the branch from the starting point (out)</param>
        /// <param name="nmeshpoints">The number of mesh points (in)</param>
        /// <param name="startIndex"></param>
        /// <param name="c_coordx"></param>
        /// <param name="c_coordy"></param>
        /// <returns></returns>
        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_get_1d_mesh_discretisation_points_v1", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_get_1d_mesh_discretisation_points_v1_dll([In] ref int ioncid, [In] ref int meshId, [In, Out] ref IntPtr c_branchidx, [In, Out] ref IntPtr c_offset, [In, Out] ref IntPtr ids, [In, Out] ref IntPtr longNames, [In] ref int nmeshpoints, [In] ref int startIndex, [In, Out] ref IntPtr c_coordx, [In, Out] ref IntPtr c_coordy);

        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_get_1d_mesh_discretisation_points_v2", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_get_1d_mesh_discretisation_points_v2_dll([In] ref int ioncid, [In] ref int meshId, [In, Out] ref IntPtr c_branchidx, [In, Out] ref IntPtr c_offset, [In, Out] ref IntPtr ids, [In, Out] ref IntPtr longNames, [In] ref int nmeshpoints, [In] ref int startIndex, [In, Out] ref IntPtr c_coordx, [In, Out] ref IntPtr c_coordy);

        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_write_mesh_1d_edge_nodes", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_write_mesh_1d_edge_nodes_dll([In] ref int ioncid, [In, Out] ref int meshid, [In, Out] ref int numEdge, [In] ref IntPtr c_mesh_1d_edge_nodes, [In] ref int start_index);

        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_put_1d_mesh_edges", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_put_1d_mesh_edges_dll([In] ref int ioncid, [In, Out] ref int meshid, [In] ref IntPtr c_edgebranchidx, [In] ref IntPtr c_edgeoffset, [In, Out] ref int numEdge, [In] ref int start_index, [In] ref IntPtr c_coordx, [In] ref IntPtr c_coordy);

        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_get_1d_mesh_edges", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_get_1d_mesh_edges_dll([In] ref int ioncid, [In, Out] ref int meshid, [In,Out] ref IntPtr c_edgebranchidx, [In,Out] ref IntPtr c_edgeoffset, [In] ref int numEdge, [In] ref int start_index, [In,Out] ref IntPtr c_coordx, [In,Out] ref IntPtr c_coordy);

        #endregion

        #endregion

        #region Mesh2D

        /// <summary>
        /// Get the id of the 2d computational mesh
        /// </summary>
        /// <param name="ioncid">The IONC data set id.</param>
        /// <param name="meshId">The 2d computational mesh id (out)</param>
        /// <returns></returns>
        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_get_2d_mesh_id", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_get_2d_mesh_id_dll([In] ref int ioncid, [In, Out] ref int meshId);

        /// <summary>
        /// Gets the 1d2d grid.
        /// </summary>
        /// <param name="ioncid"></param>
        /// <param name="meshId"></param>
        /// <param name="mesh2DGeometry"></param>
        /// <param name="startIndex"></param>
        /// <param name="includeArrays"></param>
        /// <returns></returns>
        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_get_meshgeom", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_get_meshgeom_dll(ref int ioncid, ref int meshid, ref int networkId, [In, Out] ref Mesh2DGeometry mesh2DGeometry, ref int startIndex, ref bool includeArrays);

        /// <summary>
        /// Gets the dimension of the 1d2d grid.
        /// </summary>
        /// <param name="ioncid"></param>
        /// <param name="meshId"></param>
        /// <param name="mesh2DGeometryDimensions"></param>
        /// <returns></returns>
        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_get_meshgeom_dim", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_get_meshgeom_dim_dll([In] ref int ioncid, [In] ref int meshid, [In] ref int networkId, [In, Out] ref Mesh2DGeometryDimensions mesh2DGeometryDimensions);

        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_put_meshgeom", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_put_meshgeom_dll([In] ref int ioncid, [In, Out] ref int meshid, [In, Out] ref int networkid, [In] ref Mesh2DGeometry mesh2DGeometry, [In] ref Mesh2DGeometryDimensions mesh2DGeometryDimensions, [In] string c_meshname, [In] string c_networkName, [In] ref int start_index);


        #endregion

        #region Links1D2D

        /// <summary>
        /// Defines the contacts structure.
        /// </summary>
        /// <param name="ioncid">The netCDF file id (in)</param>
        /// <param name="linkmesh">The id of the linksmesh (out)</param>
        /// <param name="linkmeshname">The name of the link (in)</param>
        /// <param name="ncontacts">The number of contactss (in)</param>
        /// <param name="mesh1">The id of the first connecting mesh (in)</param>
        /// <param name="mesh2">The id of the second connecting mesh (in)</param>
        /// <param name="locationType1Id">The location type for the first mesh: 0, 1, 2 for node, edge, face respectively (in)</param>
        /// <param name="locationType2Id">The location type for the second mesh (in)</param>
        /// <returns></returns>
        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_def_mesh_contact", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_def_mesh_contact_dll([In] ref int ioncid, [In, Out] ref int linkmesh, string linkmeshname, [In] ref int ncontacts, [In] ref int mesh1, [In] ref int mesh2, [In] ref int locationType1Id, [In] ref int locationType2Id);

        /// <summary>
        /// Puts the contacts structure.
        /// </summary>
        /// <param name="ioncid">The netCDF file id (in)</param>
        /// <param name="linkmesh">The id of the linkmesh (in)</param>
        /// <param name="c_mesh1indexes">The mesh1 indexes (in)</param>
        /// <param name="c_mesh2indexes">The mesh2 indexes (in)</param>
        /// <param name="c_contacttype">type of link</param>
        /// <param name="contactsinfo">The contacts info containing the ids and longnames (in)</param>
        /// <param name="ncontacts">The number of contactss (in)</param>
        /// <param name="startIndex"></param>
        /// <returns></returns>
        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_put_mesh_contact", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_put_mesh_contact_dll([In] ref int ioncid, [In] ref int contactsmesh, [In] ref IntPtr c_mesh1indexes, [In] ref IntPtr c_mesh2indexes, [In] ref IntPtr c_contacttype, [In, Out] ref IntPtr ids, [In, Out] ref IntPtr longNames, [In] ref int ncontacts, [In] ref int startIndex);

        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_put_mesh_contact_v1", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_put_mesh_contact_v1_dll([In] ref int ioncid, [In] ref int contactsmesh, [In] ref IntPtr c_mesh1indexes, [In] ref IntPtr c_mesh2indexes, [In] ref IntPtr c_contacttype, [In, Out] ref IntPtr ids, [In, Out] ref IntPtr longNames, [In] ref int ncontacts, [In] ref int startIndex);

        /// <summary>
        /// Get the number of contacts from a specific linkmesh
        /// </summary>
        /// <param name="ioncid">The netCDF file id (in)</param>
        /// <param name="linkmesh">The id of the linkmesh (in)</param>
        /// <param name="nlinks">The number of contactss (out)</param>
        /// <returns></returns>
        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_get_contacts_count", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_get_contacts_count_dll([In] ref int ioncid, [In] ref int linkmesh, [In, Out] ref int nlinks);

        /// <summary>
        /// Get the the mesh contacts ids from a specific linkmesh 
        /// </summary>
        /// <param name="ioncid">The netCDF file id (in)</param>
        /// <param name="linkmesh">The id of the linkmesh (in)</param>
        /// <param name="c_mesh1indexes">The mesh1 indexes (out)</param>
        /// <param name="c_mesh2indexes">The mesh2 indexes (out)</param>
        /// <param name="c_contacttype">link type</param>
        /// <param name="contactsinfo">The contacts info containing the ids and longnames (out)</param>
        /// <param name="nlinks">The number of contactss (in)</param>
        /// <param name="startIndex"></param>
        /// <returns></returns>
        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_get_mesh_contact", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_get_mesh_contact_dll([In] ref int ioncid, [In] ref int contactsmesh, [In, Out] ref IntPtr c_mesh1indexes, [In, Out] ref IntPtr c_mesh2indexes, [In, Out] ref IntPtr c_contacttype, [In, Out] ref IntPtr ids, [In, Out] ref IntPtr longNames, [In] ref int ncontacts, [In] ref int startIndex);

        [DllImport(GRIDDLL_NAME, EntryPoint = "ionc_get_mesh_contact_v1", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_get_mesh_contact_v1_dll([In] ref int ioncid, [In] ref int contactsmesh, [In, Out] ref IntPtr c_mesh1indexes, [In, Out] ref IntPtr c_mesh2indexes, [In, Out] ref IntPtr c_contacttype, [In, Out] ref IntPtr ids, [In, Out] ref IntPtr longNames, [In] ref int ncontacts, [In] ref int startIndex);
        
        #endregion

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate void IO_NetCDF_Message_Callback(int level, [MarshalAs(UnmanagedType.LPStr)]string message);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate void IO_NetCDF_Progress_Callback([MarshalAs(UnmanagedType.LPStr)]string message, ref double progress);
    }
}