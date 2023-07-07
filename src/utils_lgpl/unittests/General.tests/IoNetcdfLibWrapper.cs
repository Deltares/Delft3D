using System;
using System.Runtime.InteropServices;
using System.Text;

namespace General.tests
{
    public class UGridAttributeConstants
    {
        public class LocationValues
        {
            public const string Face = "face";
            public const string Edge = "edge";
            public const string Node = "node";
            public const string Volume = "volume";
        }

        public class Names
        {
            public const string Location = "location";
        }
    }

    public enum NetcdfOpenMode
    {
        nf90_nowrite = 0,
        nf90_write = 1,
        nf90_clobber = 0,
        nf90_noclobber = 4,
        nf90_fill = 0,
        nf90_nofill = 256,
        nf90_64bit_offset = 512,
        nf90_lock = 1024,
        nf90_share = 2048
    }

    public enum NetcdfDataType
    {
        nf90_int = 4,
        nf90_double = 6
    }

    public enum Locations
    {
        UG_LOC_NONE = 0,
        UG_LOC_NODE = 1,
        UG_LOC_EDGE = 2,
        UG_LOC_FACE = 4,
        UG_LOC_VOL = 8,
        UG_LOC_ALL2D = UG_LOC_NODE + UG_LOC_EDGE + UG_LOC_FACE
    }
    public class IoNetcdfLibWrapper
    {
        /// <summary>
        /// IoNetcdf specific constants
        /// </summary>
        public static class LibDetails
        {
            public const int MAXDIMS = 6;
            public const int MAXSTRLEN = 255;
            public const string LIB_NAME = "io_netcdf";
            public const string LIB_DEP = "netcdf";
            public const string LIB_DLL_NAME = "io_netcdf.dll";
        }

        /// <summary>
        /// Checks whether the specified data set adheres to a specific set of conventions.
        /// Datasets may adhere to multiple conventions at the same time, so use this method
        /// to check for individual conventions.
        /// </summary>
        /// <param name="ioncid"></param>
        /// <param name="iconvtype">The NetCDF conventions type to check for.</param>
        /// <returns>Whether or not the file adheres to the specified conventions.</returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_adheresto_conventions", CallingConvention = CallingConvention.Cdecl)]
        private static extern bool ionc_adheresto_conventions_dll(ref int ioncid, ref int iconvtype);

        /// <summary>
        /// Inquire the NetCDF conventions used in the dataset.
        /// </summary>
        /// <param name="ioncid">The IONC data set id.</param>
        /// <param name="iconvtype">The NetCDF conventions type of the dataset.</param>
        /// <param name="convversion"></param>
        /// <returns>Result status, ionc_noerr if successful.</returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_inq_conventions", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_inq_conventions_dll(ref int ioncid, ref int iconvtype, ref double convversion);

        /// <summary>
        /// Tries to open a NetCDF file and initialize based on its specified conventions.
        /// </summary>
        /// <param name="c_path">File name for netCDF dataset to be opened.</param>
        /// <param name="mode">NetCDF open mode, e.g. NF90_NOWRITE.</param>
        /// <param name="ioncid">The io_netcdf dataset id (this is not the NetCDF ncid, which is stored in datasets(ioncid)%ncid.</param>
        /// <param name="iconvtype">The detected conventions in the file.</param>
        /// <param name="convversion"></param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_open", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_open_dll([In] string c_path, [In, Out] ref int mode, [In, Out] ref int ioncid, [In, Out] ref int iconvtype, ref double convversion);

        /// <summary>
        /// Tries to close an open io_netcdf data set.
        /// </summary>
        /// <param name="ioncid">The io_netcdf dataset id (this is not the NetCDF ncid, which is stored in datasets(ioncid)%ncid.</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_close", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_close_dll([In] ref int ioncid);

        #region UGRID specifics

        /// <summary>
        /// Get the id of the geometry network.
        /// </summary>
        /// <param name="ioncid">The IONC data set id (in)</param>
        /// <param name="networkid">The geometry mesh (out)</param>
        /// <returns></returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_get_1d_network_id", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_get_1d_network_id_dll([In] ref int ioncid, [In, Out] ref int networkid);


        /// <summary>
        /// Get the id of the 1d computational mesh
        /// </summary>
        /// <param name="ioncid">The IONC data set id.</param>
        /// <param name="meshid">The 1d computational mesh id (out)</param>
        /// <returns></returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_get_1d_mesh_id", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_get_1d_mesh_id_dll([In] ref int ioncid, [In, Out] ref int meshid);

        /// <summary>
        /// Get the id of the 2d computational mesh
        /// </summary>
        /// <param name="ioncid">The IONC data set id.</param>
        /// <param name="meshid">The 2d computational mesh id (out)</param>
        /// <returns></returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_get_2d_mesh_id", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_get_2d_mesh_id_dll([In] ref int ioncid, [In, Out] ref int meshid);

        /// <summary>
        /// Get the id of the 3d computational mesh
        /// </summary>
        /// <param name="ioncid">The IONC data set id.</param>
        /// <param name="meshid">The 3d computational mesh id (out)</param>
        /// <returns></returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_get_3d_mesh_id", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_get_3d_mesh_id_dll([In] ref int ioncid, [In, Out] ref int meshid);

        /// <summary>
        /// Gets the number of mesh from a data set.
        /// </summary>
        /// <param name="ioncid">The IONC data set id.</param>
        /// <param name="nmesh">Number of meshes.</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_get_mesh_count", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_get_mesh_count_dll([In, Out] ref int ioncid, [In, Out] ref int nmesh);

        /// <summary>
        /// Gets the name of mesh from a data set.
        /// </summary>
        /// <param name="ioncid">The IONC data set id.</param>
        /// <param name="meshid">Mesh id.</param>
        /// <param name="meshName">The mesh name.</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_get_mesh_name", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_get_mesh_name_dll([In, Out] ref int ioncid, [In, Out] ref int meshid, [MarshalAs(UnmanagedType.LPStr)][In, Out] StringBuilder meshName);

        /// <summary>
        /// Gets the number of nodes in a single mesh from a data set.
        /// </summary>
        /// <param name="ioncid">The IONC data set id.</param>
        /// <param name="meshid">The mesh id in the specified data set.</param>
        /// <param name="nnode">Number of nodes.</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_get_node_count", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_get_node_count_dll(ref int ioncid, ref int meshid, ref int nnode);

        /// <summary>
        /// Gets the number of edges in a single mesh from a data set.
        /// </summary>
        /// <param name="ioncid">The IONC data set id.</param>
        /// <param name="meshid">The mesh id in the specified data set.</param>
        /// <param name="nedge">Number of edges.</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_get_edge_count", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_get_edge_count_dll(ref int ioncid, ref int meshid, ref int nedge);

        /// <summary>
        /// Gets the number of faces in a single mesh from a data set.
        /// </summary>
        /// <param name="ioncid">The IONC data set id.</param>
        /// <param name="meshid">The mesh id in the specified data set.</param>
        /// <param name="nface">Number of faces.</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_get_face_count", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_get_face_count_dll(ref int ioncid, ref int meshid, ref int nface);

        /// <summary>
        /// Gets the maximum number of nodes for any face in a single mesh from a data set.
        /// </summary>
        /// <param name="ioncid">The IONC data set id.</param>
        /// <param name="meshid">The mesh id in the specified data set.</param>
        /// <param name="nmaxfacenodes">The maximum number of nodes per face in the mesh.Number of faces.</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_get_max_face_nodes", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_get_max_face_nodes_dll(ref int ioncid, ref int meshid, ref int nmaxfacenodes);

        /// <summary>
        /// Gets the x,y coordinates for all nodes in a single mesh from a data set.
        /// </summary>
        /// <param name="ioncid">The IONC data set id.</param>
        /// <param name="meshid">The mesh id in the specified data set.</param>
        /// <param name="c_xptr">Pointer to array for x-coordinates</param>
        /// <param name="c_yptr">Pointer to array for y-coordinates</param>
        /// <param name="nnode">The number of nodes in the mesh.</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_get_node_coordinates", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_get_node_coordinates_dll([In, Out] ref int ioncid, [In, Out] ref int meshid, [In, Out] ref IntPtr c_xptr, [In, Out]ref IntPtr c_yptr, [In, Out] ref int nnode);

        /// <summary>
        /// Gets the edge-node connectvit table for all edges in the specified mesh.
        /// The output edge_nodes array is supposed to be of exact correct size already.
        /// </summary>
        /// <param name="ioncid">The IONC data set id.</param>
        /// <param name="meshid">The mesh id in the specified data set.</param>
        /// <param name="c_edge_nodes_ptr">Pointer to array for the edge-node connectivity table.</param>
        /// <param name="nedge">The number of edges in the mesh.</param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_get_edge_nodes", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_get_edge_nodes_dll(ref int ioncid, ref int meshid, ref IntPtr c_edge_nodes_ptr, ref int nedge, ref int start_index);

        /// <summary>
        /// Gets the face-node connectvit table for all faces in the specified mesh.
        /// The output face_nodes array is supposed to be of exact correct size already.
        /// </summary>
        /// <param name="ioncid">The IONC data set id.</param>
        /// <param name="meshid">The mesh id in the specified data set.</param>
        /// <param name="c_face_nodes_ptr">Pointer to array for the face-node connectivity table.</param>
        /// <param name="nface">The number of faces in the mesh.</param>
        /// <param name="nmaxfacenodes">The maximum number of nodes per face in the mesh.</param>
        /// <param name="fillvalue"></param>
        /// <returns>Result status (IONC_NOERR if successful).</returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_get_face_nodes", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_get_face_nodes_dll(ref int ioncid, ref int meshid, ref IntPtr c_face_nodes_ptr, ref int nface, ref int nmaxfacenodes, ref int fillvalue, ref int startIndex);

        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_write_geom_ugrid", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_write_geom_ugrid_dll(string filename);

        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_write_map_ugrid", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_write_map_ugrid_dll(string filename);

        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_get_coordinate_system", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_get_coordinate_system_dll([In] ref int ioncid, [In, Out] ref int nmesh);

        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_get_var_count", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_get_var_count_dll([In] ref int ioncid, [In] ref int mesh, [In] ref int location, [In, Out] ref int nCount);

        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_inq_varids", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_inq_varids_dll(ref int ioncid, ref int meshId, ref int location, ref IntPtr ptr, ref int nVar);

        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_def_var", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_def_var_dll(ref int ioncid, ref int meshId, ref int varId, ref int type, ref int locType, string varName, string standardName, string longName, string unit, ref double fillValue);

        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_inq_varid", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_inq_varid_dll(ref int ioncid, ref int meshId, string varName, ref int varId);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate void IO_NetCDF_Message_Callback(int level, [MarshalAs(UnmanagedType.LPStr)]string message);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate void IO_NetCDF_Progress_Callback([MarshalAs(UnmanagedType.LPStr)]string message, ref double progress);

        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_initialize", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_initialize_dll(IO_NetCDF_Message_Callback c_message_callback, IO_NetCDF_Progress_Callback c_progress_callback);

        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_get_var", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_get_var_dll(ref int ioncid, ref int meshId, ref int location, string varname, ref IntPtr c_zptr, ref int nNode, ref double c_fillvalue);

        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_put_var", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_put_var_dll(ref int ioncid, ref int meshid, ref int iloctype, string c_varname, ref IntPtr c_values_ptr, ref int nVal);

        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_put_node_coordinates", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_put_node_coordinates_dll(ref int ioncid, ref int meshid, ref IntPtr c_xvalues_ptr, ref IntPtr c_yvalues_ptr, ref int nNode);

        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_inq_varid_by_standard_name",
            CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_inq_varid_by_standard_name_dll(ref int ioncid, ref int meshId, ref int location,
            string standardName, ref int varId);

        #endregion
        #region UGRID 1D Specifics

        /// <summary>
        /// This is a structure to pass arrays of chars arrays from c# to fortran.
        /// </summary>
        public const int idssize = 40;
        public const int longnamessize = 80;
        public const int namesize = 255;
        [StructLayout(LayoutKind.Sequential)]
        public struct interop_charinfo
        {
            [MarshalAs(UnmanagedType.ByValArray, SizeConst = idssize)]
            public char[] ids;
            [MarshalAs(UnmanagedType.ByValArray, SizeConst = longnamessize)]
            public char[] longnames;
        }

        public const int metadatasize = 100;
        [StructLayout(LayoutKind.Sequential)]
        public struct interop_metadata
        {
            [MarshalAs(UnmanagedType.ByValArray, SizeConst = metadatasize)]
            public char[] institution;
            [MarshalAs(UnmanagedType.ByValArray, SizeConst = metadatasize)]
            public char[] source;
            [MarshalAs(UnmanagedType.ByValArray, SizeConst = metadatasize)]
            public char[] references;
            [MarshalAs(UnmanagedType.ByValArray, SizeConst = metadatasize)]
            public char[] version;
            [MarshalAs(UnmanagedType.ByValArray, SizeConst = metadatasize)]
            public char[] modelname;
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="ioncid"></param>
        /// <param name="metadata"></param>
        /// <returns></returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_add_global_attributes", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_add_global_attributes_dll([In] ref int ioncid, ref interop_metadata metadata);

        /// <summary>
        /// This function creates a new netCDF file
        /// </summary>
        /// <param name="c_path">The path where the file will be created (in)</param>
        /// <param name="mode"> The netCDF opening mode (in)</param>
        /// <param name="ioncid">The netCDF file id (out)</param>
        /// <returns></returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_create", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_create_dll([In] string c_path, [In] ref int mode, [In, Out] ref int ioncid);

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
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_create_1d_network", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_create_1d_network_dll([In] ref int ioncid, [In, Out] ref int networkid, [In] string networkName, [In] ref int nNodes, [In] ref int nBranches, [In] ref int nGeometry);

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
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_write_1d_network_nodes", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_write_1d_network_nodes_dll([In] ref int ioncid, [In] ref int networkid, [In] ref IntPtr c_nodesX, [In] ref IntPtr c_nodesY, interop_charinfo[] nodesinfo, [In] ref int nNodes);

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
        /// <returns></returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_put_1d_network_branches", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_put_1d_network_branches_dll([In] ref int ioncid, [In] ref int networkid, [In] ref IntPtr c_sourcenodeid, [In] ref IntPtr c_targetnodeid, interop_charinfo[] branchinfo, [In] ref IntPtr c_branchlengths, [In] ref IntPtr c_nbranchgeometrypoints, [In] ref int nBranches, [In] ref int startIndex);

        /// <summary>
        /// Writes the branch geometry (the geometry points)  
        /// </summary>
        /// <param name="ioncid">The netCDF file id (in)</param>
        /// <param name="networkid">The network id (in)</param>
        /// <param name="c_geopointsX">The x coordinates of the geometry points (in)</param>
        /// <param name="c_geopointsY">The y coordinates of the geometry points (in)</param>
        /// <param name="nGeometry">The number of geometry points (in)</param>
        /// <returns></returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_write_1d_network_branches_geometry", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_write_1d_network_branches_geometry_dll([In] ref int ioncid, [In] ref int networkid, [In] ref IntPtr c_geopointsX, [In] ref IntPtr c_geopointsY, [In] ref int nGeometry);

        /// <summary>
        /// Writes a 1d mesh. The geometrical features (e.g. the branches and geometry points) are described in the network above
        /// </summary>
        /// <param name="ioncid">The netCDF file id (in)</param>
        /// <param name="networkid">The network id (in)</param>
        /// <param name="meshid">The mesh id (out)</param>
        /// <param name="meshname">The mesh name (in)</param>
        /// <param name="nmeshpoints">The number of mesh points (in)</param>
        /// <param name="nmeshedges">The number of mesh edges (in)</param>
        /// <returns></returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_create_1d_mesh", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_create_1d_mesh_dll([In] ref int ioncid, [In] string networkname, [In, Out] ref int meshid, string meshname, [In] ref int nmeshpoints);

        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_create_1d_mesh_v1", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_create_1d_mesh_v1_dll([In] ref int ioncid, [In] string networkname, [In, Out] ref int meshid, string meshname, [In] ref int nmeshpoints,[In] ref int nedges, [In] ref int writexy);

        /// <summary>
        /// Writes the mesh coordinates points 
        /// </summary>
        /// <param name="ioncid">The netCDF file id (in)</param>
        /// <param name="networkid">The network id (in)</param>
        /// <param name="c_branchidx">The branch id for each mesh point (in)</param>
        /// <param name="c_offset">The offset along the branch from the starting point (in)</param>
        /// <param name="nodeinfo">The node info (in)</param>
        /// <param name="nmeshpoints">The number of mesh points (in)</param>
        /// <returns></returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_put_1d_mesh_discretisation_points", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_put_1d_mesh_discretisation_points_dll([In] ref int ioncid, [In] ref int meshid, [In] ref IntPtr c_branchidx, [In] ref IntPtr c_offset, interop_charinfo[] nodeinfo, [In] ref int nmeshpoints, [In] ref int startIndex);


        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_put_1d_mesh_discretisation_points_v1", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_put_1d_mesh_discretisation_points_v1_dll([In] ref int ioncid, [In] ref int meshid, [In] ref IntPtr c_branchidx, [In] ref IntPtr c_offset, interop_charinfo[] nodeinfo, [In] ref int nmeshpoints, [In] ref int startIndex, [In] ref IntPtr c_coordx, [In] ref IntPtr c_coordy);


        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_put_1d_mesh_discretisation_points_v2", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_put_1d_mesh_discretisation_points_v2_dll([In] ref int ioncid, [In] ref int meshid, [In] ref IntPtr c_branchidx, [In] ref IntPtr c_offset, [In] ref IntPtr c_ids, [In] ref IntPtr c_long_names, [In] ref int nmeshpoints, [In] ref int startIndex, [In] ref IntPtr c_coordx, [In] ref IntPtr c_coordy);


        /// <summary>
        /// Get the number of network nodes
        /// </summary>
        /// <param name="ioncid">The netCDF file id (in)</param>
        /// <param name="networkid">The network id (in)</param>
        /// <param name="nNodes">The number of nodes(out)</param>
        /// <returns></returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_get_1d_network_nodes_count", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_get_1d_network_nodes_count_dll([In] ref int ioncid, [In] ref int networkid, [In, Out] ref int nNodes);

        /// <summary>
        /// Get the number of branches
        /// </summary>
        /// <param name="ioncid">The netCDF file id (in)</param>
        /// <param name="networkid">The network id (in)</param>
        /// <param name="nBranches">The number of branches (out)</param>
        /// <returns></returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_get_1d_network_branches_count", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_get_1d_network_branches_count_dll([In] ref int ioncid, [In] ref int networkid, [In, Out] ref int nBranches);

        /// <summary>
        /// Get the number of geometry points for all branches
        /// </summary>
        /// <param name="ioncid">The netCDF file id (in)</param>
        /// <param name="networkid">The network id (in)</param>
        /// <param name="ngeometrypoints">The number of geometry points for all branches (out)</param>
        /// <returns></returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_get_1d_network_branches_geometry_coordinate_count", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_get_1d_network_branches_geometry_coordinate_count_dll([In] ref int ioncid, [In] ref int networkid, [In, Out] ref int ngeometrypoints);

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
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_read_1d_network_nodes", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_read_1d_network_nodes_dll([In] ref int ioncid, [In] ref int networkid, [In, Out] ref IntPtr c_nodesX, [In, Out] ref IntPtr c_nodesY, [In, Out]  interop_charinfo[] nodesinfo, [In] ref int nNodes);

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
        /// <returns></returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_get_1d_network_branches", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_get_1d_network_branches_dll([In] ref int ioncid, [In] ref int networkid, [In, Out] ref IntPtr c_sourcenodeid, [In, Out] ref IntPtr c_targetnodeid, [In, Out] ref IntPtr c_branchlengths, [In, Out]  interop_charinfo[] branchinfo, [In, Out] ref IntPtr c_nbranchgeometrypoints, [In] ref int nBranches, [In] ref int startIndex);

        /// <summary>
        /// Reads the branch geometry
        /// </summary>
        /// <param name="ioncid">The netCDF file id</param>
        /// <param name="networkid">The network id (in)</param>
        /// <param name="c_geopointsX">The x coordinates of the geometry points (out)</param>
        /// <param name="c_geopointsY">The y coordinates of the geometry points (out)</param>
        /// <param name="nGeometrypoints">The number of nodes (in)</param>
        /// <returns></returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_read_1d_network_branches_geometry", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_read_1d_network_branches_geometry_dll([In] ref int ioncid, [In] ref int networkid, [In, Out] ref IntPtr c_geopointsX, [In, Out] ref IntPtr c_geopointsY, [In] ref int nGeometrypoints);

        /// <summary>
        /// Get the number of mesh discretization points 
        /// </summary>
        /// <param name="ioncid">The netCDF file id (in)</param>
        /// <param name="networkid">The network id (in)</param>
        /// <param name="nmeshpoints">The number of mesh points (out)</param>
        /// <returns></returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_get_1d_mesh_discretisation_points_count", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_get_1d_mesh_discretisation_points_count_dll([In] ref int ioncid, [In] ref int networkid, [In, Out] ref int nmeshpoints);

        /// <summary>
        /// Read the coordinates of the mesh points  
        /// </summary>
        /// <param name="ioncid">The netCDF file id (in)</param>
        /// <param name="networkid">The network id (in)</param>
        /// <param name="c_branchidx">The branch id for each mesh point (out)</param>
        /// <param name="c_offset">The offset along the branch from the starting point (out)</param>
        /// <param name="nmeshpoints">The number of mesh points (in)</param>
        /// <returns></returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_get_1d_mesh_discretisation_points", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_get_1d_mesh_discretisation_points_dll([In] ref int ioncid, [In] ref int networkid, [In, Out] ref IntPtr c_branchidx, [In, Out] ref IntPtr c_offset, [In, Out]  interop_charinfo[] nodeinfo, [In] ref int nmeshpoints, [In] ref int startIndex);

        /// <summary>
        /// Defines the contacts structure.
        /// </summary>
        /// <param name="ioncid">The netCDF file id (in)</param>
        /// <param name="contactsmesh">The id of the contactsmesh (out)</param>
        /// <param name="contactsmeshname">The name of the contacts structure (in)</param>
        /// <param name="ncontacts">The number of contactss (in)</param>
        /// <param name="mesh1">The id of the first connecting mesh (in)</param>
        /// <param name="mesh2">The id of the second connecting mesh (in)</param>
        /// <param name="locationType1Id">The location type for the first mesh: 0, 1, 2 for node, edge, face respectively (in)</param>
        /// <param name="locationType2Id">The location type for the second mesh (in)</param>
        /// <returns></returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_def_mesh_contact", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_def_mesh_contact_dll([In] ref int ioncid, [In, Out] ref int contactsmesh, string contactsmeshname, [In] ref int ncontacts, [In] ref int mesh1, [In] ref int mesh2, [In] ref int locationType1Id, [In] ref int locationType2Id);

        /// <summary>
        /// Puts the contacts structure.
        /// </summary>
        /// <param name="ioncid">The netCDF file id (in)</param>
        /// <param name="contactsmesh">The id of the contactsmesh (in)</param>
        /// <param name="c_mesh1indexes">The mesh1 indexes (in)</param>
        /// <param name="c_mesh2indexes">The mesh2 indexes (in)</param>
        /// <param name="contactsinfo">The contacts info containing the ids and longnames (in)</param>
        /// <param name="ncontacts">The number of contactss (in)</param>
        /// <returns></returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_put_mesh_contact", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_put_mesh_contact_dll([In] ref int ioncid, [In] ref int contactsmesh, [In] ref IntPtr c_mesh1indexes, [In] ref IntPtr c_mesh2indexes, [In] ref IntPtr c_contacttype, [In, Out]  interop_charinfo[] contactsinfo, [In] ref int ncontacts, [In] ref int startIndex);

        /// <summary>
        /// Get the number of contacts from a specific contactsmesh
        /// </summary>
        /// <param name="ioncid">The netCDF file id (in)</param>
        /// <param name="contactsmesh">The id of the contactsmesh (in)</param>
        /// <param name="ncontacts">The number of contactss (out)</param>
        /// <returns></returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_get_contacts_count", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_get_contacts_count_dll([In] ref int ioncid, [In] ref int contactsmesh, [In, Out] ref int ncontacts);

        /// <summary>
        /// Get the the mesh contacts ids from a specific contactsmesh 
        /// </summary>
        /// <param name="ioncid">The netCDF file id (in)</param>
        /// <param name="contactsmesh">The id of the contactsmesh (in)</param>
        /// <param name="c_mesh1indexes">The mesh1 indexes (out)</param>
        /// <param name="c_mesh2indexes">The mesh2 indexes (out)</param>
        /// <param name="contactsinfo">The contacts info containing the ids and longnames (out)</param>
        /// <param name="ncontacts">The number of contactss (in)</param>
        /// <returns></returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_get_mesh_contact", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_get_mesh_contact_dll([In] ref int ioncid, [In] ref int contactsmesh, [In, Out] ref IntPtr c_mesh1indexes, [In, Out] ref IntPtr c_mesh2indexes, [In, Out] ref IntPtr c_contacttype, [In, Out]  interop_charinfo[] contactsinfo, [In] ref int ncontacts, [In] ref int startIndex);

        /// <summary>
        /// Clone the definitions specific mesh from one netCDF file to another netCDF. 
        /// Clones all related attributes of the mesh, but it can not clone mesh contacts yet!
        /// </summary>
        /// <param name="ncidin">The input netCDF file id containing the mesh to clone (in)</param>
        /// <param name="ncidout">The output netCDF file id, can be empty/not empty (in)</param>
        /// <param name="meshidin">The mesh id to copy (in)</param>
        /// <param name="meshidout">The id of the cloned mesh in the output file (out)</param>
        /// <returns></returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_clone_mesh_definition", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_clone_mesh_definition_dll([In] ref int ncidin, [In] ref int ncidout, [In] ref int meshidin, [In, Out] ref int meshidout);

        /// <summary>
        /// Clone the data of a specific mesh from one netCDF file to another netCDF
        /// </summary>
        /// <param name="ncidin">The input netCDF file id containing the mesh to clone (in)</param>
        /// <param name="ncidout">The output netCDF file id, can be empty/not empty (in)</param>
        /// <param name="meshidin">The mesh id to copy (in)</param>
        /// <param name="meshidout">The id of the cloned mesh in the output file (out)</param>
        /// <returns></returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_clone_mesh_data", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_clone_mesh_data_dll([In] ref int ncidin, [In] ref int ncidout, [In] ref int meshidin, [In] ref int meshidout);

        /// <summary>
        /// Gets the number of networks
        /// </summary>
        /// <param name="ncidin"></param>
        /// <param name="nnumNetworks"></param>
        /// <returns></returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_get_number_of_networks", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_get_number_of_networks_dll([In] ref int ncidin, [In, Out] ref int nnumNetworks);
        
        /// <summary>
        /// Gets the number of meshes
        /// </summary>
        /// <param name="ncidin"></param>
        /// <param name="meshType"> Mesh type: 0 = any type, 1 = 1D mesh, 2 = 2D mesh, 3 = 3D mesh </param>
        /// <param name="numMeshes"></param>
        /// <returns></returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_get_number_of_meshes", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_get_number_of_meshes_dll([In] ref int ncidin, [In] ref int meshType, [In, Out] ref int numMeshes);

        /// <summary>
        /// Get the network ids
        /// </summary>
        /// <param name="ncidin"></param>
        /// <param name="c_networkids"></param>
        /// <param name="nnumNetworks"></param>
        /// <returns></returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_get_network_ids", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_get_network_ids_dll([In] ref int ncidin, [In, Out] ref IntPtr c_networkids,[In] ref int nnumNetworks);

        /// <summary>
        /// Gets the mesh ids
        /// </summary>
        /// <param name="ncidin"></param>
        /// <param name="meshType"></param>
        /// <param name="c_meshids"></param>
        /// <param name="nnumNetworks"></param>
        /// <returns></returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_ug_get_mesh_ids", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_ug_get_mesh_ids_dll([In] ref int ncidin, [In] ref int meshType, [In, Out] ref IntPtr c_meshids, [In] ref int nnumNetworks);

        //-Branch order functions -------------------//

        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_put_1d_network_branchorder", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_put_1d_network_branchorder_dll([In] ref int ncidin, [In] ref int networkid, [In] ref IntPtr c_branchorder, [In] ref int nbranches);


        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_get_1d_network_branchorder", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_get_1d_network_branchorder_dll([In] ref int ncidin, [In] ref int networkid, [In, Out] ref IntPtr c_branchorder, [In] ref int nnumNetworks);

        //-Get the network names------///
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_get_1d_network_name", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_get_1d_network_name_dll([In] ref int ncidin, [In] ref int networkid, [MarshalAs(UnmanagedType.LPStr)][In, Out] StringBuilder networkName);

        //-Get the meshids from network ids ---//
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_count_mesh_ids_from_network_id", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_count_mesh_ids_from_network_id_dll([In] ref int ncidin, [In] ref int networkid, [In, Out] ref int nmeshids);

        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_get_mesh_ids_from_network_id", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_get_mesh_ids_from_network_id_dll([In] ref int ncidin, [In] ref int networkid, [In] ref int nmeshids, [In, Out] ref IntPtr c_meshids);

        //-Get the network id for a specified mesh
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_get_network_id_from_mesh_id", CallingConvention = CallingConvention.Cdecl)]
        private static extern int ionc_get_network_id_from_mesh_id_dll([In] ref int ncidin, [In] ref int meshid, [In, Out] ref int networkid);

        #endregion

        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_get_meshgeom", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_get_meshgeom_dll([In] ref int ioncid, [In] ref int meshid, [In] ref int networkid, [In, Out] ref meshgeom meshgeom, [In] ref int start_index, [In] ref bool includeArrays);


        /// <summary>
        /// Get meshgeom without start_index and includeArrays parameters
        /// </summary>
        /// <param name="ioncid"> The input file id</param>
        /// <param name="meshid"> The input meshid</param>
        /// <param name="networkid">The input network id. If set to -1 no network arrays will be returned </param>
        /// <param name="meshgeom"> The output structure containing the pointers to the arrays</param>
        /// <param name="meshgeomdim">The output structure containing the arrays dimensions</param>
        /// <returns></returns>

        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_get_meshgeom_v1", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_get_meshgeom_v1_dll([In] ref int ioncid, [In] ref int meshid, [In] ref int networkid, [In, Out] ref meshgeom meshgeom);
        

        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_get_meshgeom_dim", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_get_meshgeom_dim_dll([In] ref int ioncid, [In] ref int meshid, [In] ref int networkid, [In, Out] ref meshgeomdim meshgeomdim);

        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_def_mesh_ids", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_def_mesh_ids_dll([In] ref int ioncid, [In] ref int meshid, [In] ref int iloctype);

        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_put_var_chars", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_put_var_chars_dll([In] ref int ioncid, [In] ref int meshid, [MarshalAs(UnmanagedType.LPStr)][In, Out] StringBuilder varname, [In] interop_charinfo[] values, [In] ref int nvalues);

        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_get_var_chars", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_get_var_chars_dll([In] ref int ioncid, [In] ref int meshid, [MarshalAs(UnmanagedType.LPStr)][In, Out] StringBuilder varname, [In, Out] interop_charinfo[] values, [In] ref int nvalues);

        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_put_meshgeom", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_put_meshgeom_dll([In] ref int ioncid, [In,Out] ref int meshid, [In, Out] ref int networkid, [In] ref meshgeom meshgeom, [In] ref meshgeomdim meshgeomdim, [In] string c_meshname, [In] string c_networkName, [In] ref int start_index);


        /// <summary>
        /// Put meshgeom without start_index and network name parameters
        /// </summary>
        /// <param name="ioncid"> The input file id</param>
        /// <param name="meshid"> The returned meshid</param>
        /// <param name="networkid">The returned network id. If set to -1 no network will be written </param>
        /// <param name="meshgeom"> The input structure containing the arrays to write</param>
        /// <param name="meshgeomdim">The input structure containing the dimensions of the arrays</param>
        /// <returns></returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_put_meshgeom_v1", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_put_meshgeom_v1_dll([In] ref int ioncid, [In, Out] ref int meshid, [In, Out] ref int networkid, [In] ref meshgeom meshgeom, [In] ref meshgeomdim meshgeomdim);

        /// <summary>
        /// Put a network to file
        /// </summary>
        /// <param name="ioncid"></param>
        /// <param name="networkid"></param>
        /// <param name="networkgeom"></param>
        /// <param name="networkgeomdim"></param>
        /// <returns></returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ionc_put_network", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ionc_put_network_dll([In] ref int ioncid, [In, Out] ref int networkid, [In] ref meshgeom networkgeom, [In] ref meshgeomdim networkgeomdim);


        #region Implementation of LibWrapper

        public bool ionc_adheresto_conventions(ref int ioncid, ref int iconvtype)
        {
            return ionc_adheresto_conventions_dll(ref ioncid, ref iconvtype);
        }

        public int ionc_inq_conventions(ref int ioncid, ref int iconvtype, ref double convversion)
        {
            return ionc_inq_conventions_dll(ref ioncid, ref iconvtype, ref convversion);
        }

        public int ionc_open(string c_path, ref int mode, ref int ioncid, ref int iconvtype, ref double convversion)
        {
            return ionc_open_dll(c_path, ref mode, ref ioncid, ref iconvtype, ref convversion);
        }

        public int ionc_close(ref int ioncid)
        {
            return ionc_close_dll(ref ioncid);
        }

        public int ionc_get_1d_network_id(ref int ioncid, ref int networkid)
        {
            return ionc_get_1d_network_id_dll(ref ioncid, ref networkid);
        }

        public int ionc_get_1d_mesh_id(ref int ioncid, ref int meshid)
        {
            return ionc_get_1d_mesh_id_dll(ref ioncid, ref meshid);
        }

        public int ionc_get_2d_mesh_id(ref int ioncid, ref int meshid)
        {
            return ionc_get_2d_mesh_id_dll(ref ioncid, ref meshid);
        }

        public int ionc_get_3d_mesh_id(ref int ioncid, ref int meshid)
        {
            return ionc_get_3d_mesh_id_dll(ref ioncid, ref meshid);
        }

        public int ionc_get_mesh_count(ref int ioncid, ref int nmesh)
        {
            return ionc_get_mesh_count_dll(ref ioncid, ref nmesh);
        }

        public int ionc_get_mesh_name(ref int ioncid, ref int meshid, StringBuilder meshName)
        {
            return ionc_get_mesh_name_dll(ref ioncid, ref meshid, meshName);
        }

        public int ionc_get_node_count(ref int ioncid, ref int meshid, ref int nnode)
        {
            return ionc_get_node_count_dll(ref ioncid, ref meshid, ref nnode);
        }

        public int ionc_get_edge_count(ref int ioncid, ref int meshid, ref int nedge)
        {
            return ionc_get_edge_count_dll(ref ioncid, ref meshid, ref nedge);
        }

        public int ionc_get_face_count(ref int ioncid, ref int meshid, ref int nface)
        {
            return ionc_get_face_count_dll(ref ioncid, ref meshid, ref nface);
        }

        public int ionc_get_max_face_nodes(ref int ioncid, ref int meshid, ref int nmaxfacenodes)
        {
            return ionc_get_max_face_nodes_dll(ref ioncid, ref meshid, ref nmaxfacenodes);
        }

        public int ionc_get_node_coordinates(ref int ioncid, ref int meshid, ref IntPtr c_xptr, ref IntPtr c_yptr, ref int nnode)
        {
            return ionc_get_node_coordinates_dll(ref ioncid, ref meshid, ref c_xptr, ref c_yptr, ref nnode);
        }

        public int ionc_get_edge_nodes(ref int ioncid, ref int meshid, ref IntPtr c_edge_nodes_ptr, ref int nedge, ref int startIndex)
        {
            return ionc_get_edge_nodes_dll(ref ioncid, ref meshid, ref c_edge_nodes_ptr, ref nedge, ref startIndex);
        }

        public int ionc_get_face_nodes(ref int ioncid, ref int meshid, ref IntPtr c_face_nodes_ptr, ref int nface, ref int nmaxfacenodes, ref int fillvalue, ref int startIndex)
        {
            return ionc_get_face_nodes_dll(ref ioncid, ref meshid, ref c_face_nodes_ptr, ref nface, ref nmaxfacenodes, ref fillvalue, ref startIndex);
        }

        public int ionc_write_geom_ugrid(string filename)
        {
            return ionc_write_geom_ugrid_dll(filename);
        }

        public int ionc_write_map_ugrid(string filename)
        {
            return ionc_write_map_ugrid_dll(filename);
        }

        public int ionc_get_coordinate_system(ref int ioncid, ref int nmesh)
        {
            return ionc_get_coordinate_system_dll(ref ioncid, ref nmesh);
        }

        public int ionc_get_var_count(ref int ioncid, ref int mesh, ref int location, ref int nCount)
        {
            return ionc_get_var_count_dll(ref ioncid, ref mesh, ref location, ref nCount);
        }

        public int ionc_inq_varids(ref int ioncid, ref int meshId, ref int location, ref IntPtr ptr, ref int nVar)
        {
            return ionc_inq_varids_dll(ref ioncid, ref meshId, ref location, ref ptr, ref nVar);
        }

        public int ionc_initialize(IO_NetCDF_Message_Callback c_message_callback, IO_NetCDF_Progress_Callback c_progress_callback)
        {
            return ionc_initialize_dll(c_message_callback, c_progress_callback);
        }

        public int ionc_get_var(ref int ioncid, ref int meshId, ref int location, string varname, ref IntPtr c_zptr, ref int nNode, ref double c_fillvalue)
        {
            return ionc_get_var_dll(ref ioncid, ref meshId, ref location, varname, ref c_zptr, ref nNode, ref c_fillvalue);
        }

        public int ionc_put_var(ref int ioncid, ref int meshid, ref int iloctype, string c_varname, ref IntPtr c_values_ptr, ref int nVal)
        {
            return ionc_put_var_dll(ref ioncid, ref meshid, ref iloctype, c_varname, ref c_values_ptr, ref nVal);
        }

        public int ionc_put_node_coordinates(ref int ioncid, ref int meshid, ref IntPtr c_xvalues_ptr, ref IntPtr c_yvalues_ptr, ref int nNode)
        {
            return ionc_put_node_coordinates_dll(ref ioncid, ref meshid, ref c_xvalues_ptr, ref c_yvalues_ptr, ref nNode);
        }

        public int ionc_add_global_attributes(ref int ioncid, ref interop_metadata metadata)
        {
            return ionc_add_global_attributes_dll(ref ioncid, ref metadata);
        }

        public int ionc_create(string c_path, ref int mode, ref int ioncid)
        {
            return ionc_create_dll(c_path, ref mode, ref ioncid);
        }

        public int ionc_create_1d_network(ref int ioncid, ref int networkid, string networkName, ref int nNodes, ref int nBranches, ref int nGeometry)
        {
            return ionc_create_1d_network_dll(ref ioncid, ref networkid, networkName, ref nNodes, ref nBranches, ref nGeometry);
        }

        public int ionc_write_1d_network_nodes(ref int ioncid, ref int networkid, ref IntPtr c_nodesX, ref IntPtr c_nodesY, interop_charinfo[] nodesinfo, ref int nNodes)
        {
            return ionc_write_1d_network_nodes_dll(ref ioncid, ref networkid, ref c_nodesX, ref c_nodesY, nodesinfo, ref nNodes);
        }

        public int ionc_put_1d_network_branches(ref int ioncid, ref int networkid, ref IntPtr c_sourcenodeid,
            ref IntPtr c_targetnodeid, interop_charinfo[] branchinfo, ref IntPtr c_branchlengths,
            ref IntPtr c_nbranchgeometrypoints, ref int nBranches, ref int startIndex)
        {
            return ionc_put_1d_network_branches_dll(ref ioncid, ref networkid, ref c_sourcenodeid, ref c_targetnodeid, branchinfo, ref c_branchlengths, ref c_nbranchgeometrypoints, ref nBranches, ref startIndex);
        }

        public int ionc_write_1d_network_branches_geometry(ref int ioncid, ref int networkid, ref IntPtr c_geopointsX,
            ref IntPtr c_geopointsY, ref int nGeometry)
        {
            return ionc_write_1d_network_branches_geometry_dll(ref ioncid, ref networkid, ref c_geopointsX, ref c_geopointsY, ref nGeometry);
        }

        public int ionc_create_1d_mesh(ref int ioncid, string networkname, ref int meshid, string meshname, ref int nmeshpoints)
        {
            return ionc_create_1d_mesh_dll(ref ioncid, networkname, ref meshid, meshname, ref nmeshpoints);
        }

        public int ionc_create_1d_mesh_v1(ref int ioncid, string networkname, ref int meshid, string meshname, ref int nmeshpoints, ref int nedges, ref int writexy)
        {
            return ionc_create_1d_mesh_v1_dll(ref ioncid, networkname, ref meshid, meshname, ref nmeshpoints, ref nedges, ref writexy);
        }

        public int ionc_put_1d_mesh_discretisation_points(ref int ioncid, ref int networkid, ref IntPtr c_branchidx,
            ref IntPtr c_offset, interop_charinfo[]  nodeinfo, ref int nmeshpoints, ref int startIndex)
        {
            return ionc_put_1d_mesh_discretisation_points_dll(ref ioncid, ref networkid, ref c_branchidx, ref c_offset, nodeinfo, ref nmeshpoints, ref startIndex);
        }

        public int ionc_put_1d_mesh_discretisation_points_v1(ref int ioncid, ref int networkid, ref IntPtr c_branchidx,
            ref IntPtr c_offset, interop_charinfo[] nodeinfo, ref int nmeshpoints, ref int startIndex, ref IntPtr c_coordx, ref IntPtr c_coordy)
        {
            return ionc_put_1d_mesh_discretisation_points_v1_dll(ref ioncid, ref networkid, ref c_branchidx, ref c_offset, nodeinfo, ref nmeshpoints, ref startIndex, ref c_coordx, ref c_coordy);
        }


        public virtual int ionc_put_1d_mesh_discretisation_points_v2(ref int ioncid, ref int networkid, ref IntPtr c_branchidx,
            ref IntPtr c_offset, ref IntPtr nodeids, ref IntPtr nodelongnames, ref int nmeshpoints, ref int startIndex, ref IntPtr c_coordx, ref IntPtr c_coordy)
        {
            return  ionc_put_1d_mesh_discretisation_points_v2_dll(ref ioncid, ref networkid, ref c_branchidx, ref c_offset, ref nodeids,  ref nodelongnames,ref nmeshpoints, ref startIndex, ref c_coordx, ref c_coordy);
        }

        public int ionc_get_1d_network_nodes_count(ref int ioncid, ref int networkid, ref int nNodes)
        {
            return ionc_get_1d_network_nodes_count_dll(ref ioncid, ref networkid, ref nNodes);
        }

        public int ionc_get_1d_network_branches_count(ref int ioncid, ref int networkid, ref int nBranches)
        {
            return ionc_get_1d_network_branches_count_dll(ref ioncid, ref networkid, ref nBranches);
        }

        public int ionc_get_1d_network_branches_geometry_coordinate_count(ref int ioncid, ref int networkid, ref int ngeometrypoints)
        {
            return ionc_get_1d_network_branches_geometry_coordinate_count_dll(ref ioncid, ref networkid, ref ngeometrypoints);
        }

        public int ionc_read_1d_network_nodes(ref int ioncid, ref int networkid, ref IntPtr c_nodesX, ref IntPtr c_nodesY,
            interop_charinfo[] nodesinfo, ref int nNodes)
        {
            return ionc_read_1d_network_nodes_dll(ref ioncid, ref networkid, ref c_nodesX, ref c_nodesY,
                nodesinfo, ref nNodes);
        }

        public int ionc_get_1d_network_branches(ref int ioncid, ref int networkid, ref IntPtr c_sourcenodeid,
            ref IntPtr c_targetnodeid, ref IntPtr c_branchlengths, interop_charinfo[] branchinfo,
            ref IntPtr c_nbranchgeometrypoints, ref int nBranches, ref int startIndex)
        {
            return ionc_get_1d_network_branches_dll(ref ioncid, ref  networkid, ref c_sourcenodeid,
                ref c_targetnodeid, ref c_branchlengths, branchinfo,
                ref c_nbranchgeometrypoints, ref nBranches, ref startIndex);
        }

        public int ionc_read_1d_network_branches_geometry(ref int ioncid, ref int networkid, ref IntPtr c_geopointsX,
            ref IntPtr c_geopointsY, ref int nNodes)
        {
            return ionc_read_1d_network_branches_geometry_dll(ref ioncid, ref networkid, ref c_geopointsX,
                ref c_geopointsY, ref nNodes);
        }

        public int ionc_get_1d_mesh_discretisation_points_count(ref int ioncid, ref int networkid, ref int nmeshpoints)
        {
            return ionc_get_1d_mesh_discretisation_points_count_dll(ref ioncid, ref networkid, ref nmeshpoints);
        }

        public int ionc_get_1d_mesh_discretisation_points(ref int ioncid, ref int networkid, ref IntPtr c_branchidx,
            ref IntPtr c_offset, interop_charinfo[] nodeinfo, ref int nmeshpoints, ref int startIndex)
        {
            return ionc_get_1d_mesh_discretisation_points_dll(ref ioncid, ref networkid, ref c_branchidx,
                ref c_offset, nodeinfo, ref nmeshpoints, ref startIndex);
        }

        public int ionc_def_mesh_contact(ref int ioncid, ref int contactsmesh, string contactsmeshname, ref int ncontacts, ref int mesh1, ref int mesh2, ref int locationType1Id, ref int locationType2Id)
        {
            return ionc_def_mesh_contact_dll(ref ioncid, ref contactsmesh, contactsmeshname, ref ncontacts, ref mesh1, ref mesh2, ref locationType1Id, ref locationType2Id);
        }

        public int ionc_put_mesh_contact(ref int ioncid, ref int contactsmesh, ref IntPtr c_mesh1indexes, ref IntPtr c_mesh2indexes, ref IntPtr c_contacttype, interop_charinfo[] contactsinfo, ref int ncontacts, ref int startIndex)
        {
            return ionc_put_mesh_contact_dll(ref ioncid, ref contactsmesh, ref c_mesh1indexes, ref c_mesh2indexes, ref c_contacttype, contactsinfo, ref ncontacts, ref startIndex);
        }

        public int ionc_get_contacts_count(ref int ioncid, ref int contactsmesh, ref int ncontacts)
        {
            return ionc_get_contacts_count_dll(ref ioncid, ref contactsmesh, ref ncontacts);
        }

        public int ionc_get_mesh_contact(ref int ioncid, ref int contactsmesh, ref IntPtr c_mesh1indexes, ref IntPtr c_mesh2indexes, ref IntPtr c_contacttype, interop_charinfo[] contactsinfo, ref int ncontacts, ref int startIndex)
        {
            return ionc_get_mesh_contact_dll(ref ioncid, ref contactsmesh, ref c_mesh1indexes, ref c_mesh2indexes, ref c_contacttype, contactsinfo, ref ncontacts, ref startIndex);
        }

        public int ionc_clone_mesh_definition(ref int ncidin, ref int ncidout, ref int meshidin, ref int meshidout)
        {
            return ionc_clone_mesh_definition_dll(ref ncidin, ref ncidout, ref meshidin, ref meshidout);
        }

        public int ionc_clone_mesh_data(ref int ncidin, ref int ncidout, ref int meshidin, ref int meshidout)
        {
            return ionc_clone_mesh_data_dll(ref ncidin, ref ncidout, ref meshidin, ref meshidout);
        }

        public int ionc_get_number_of_networks(ref int ncidin, ref int nnumNetworks)
        {
            return ionc_get_number_of_networks_dll(ref ncidin, ref nnumNetworks);
        }

        public int ionc_get_number_of_meshes(ref int ncidin, ref int meshType, ref int numMeshes)
        {
            return ionc_get_number_of_meshes_dll(ref ncidin, ref meshType, ref numMeshes);
        }

        public int ionc_get_network_ids(ref int ncidin, ref IntPtr c_networkids, ref int nnumNetworks)
        {
            return ionc_get_network_ids_dll(ref ncidin, ref c_networkids, ref nnumNetworks);
        }

        public int ionc_ug_get_mesh_ids(ref int ncidin, ref int meshType, ref IntPtr c_meshids, ref int nnumNetworks)
        {
            return ionc_ug_get_mesh_ids_dll(ref ncidin, ref meshType, ref c_meshids, ref nnumNetworks);
        }

        public int ionc_put_1d_network_branchorder(ref int ncidin, ref int networkid, ref IntPtr c_branchorder, ref int nbranches)
        {
            return ionc_put_1d_network_branchorder_dll(ref ncidin, ref networkid, ref c_branchorder, ref  nbranches);
        }

        public int ionc_get_1d_network_branchorder(ref int ncidin, ref int networkid, ref IntPtr c_branchorder, ref int nbranches)
        {
            return ionc_get_1d_network_branchorder_dll(ref ncidin, ref networkid, ref c_branchorder, ref nbranches);
        }

        /// Gets the network name
        public int ionc_get_1d_network_name(ref int ncidin, ref int networkid, StringBuilder networkName)
        {
            return ionc_get_1d_network_name_dll(ref ncidin, ref networkid, networkName);
        }

        //- Counts the mesh ids associated with a network id
        public int ionc_count_mesh_ids_from_network_id(ref int ncidin, ref int networkid, ref int nmeshids)
        {
            return ionc_count_mesh_ids_from_network_id_dll(ref ncidin, ref networkid, ref nmeshids);
        }
        //- Gets the mesh ids associated with a network id
        public int ionc_get_mesh_ids_from_network_id(ref int ncidin, ref int networkid, ref int nmeshids, ref IntPtr c_meshids)
        {
            return ionc_get_mesh_ids_from_network_id_dll(ref ncidin, ref networkid, ref nmeshids, ref c_meshids);
        }

        public int ionc_get_network_id_from_mesh_id(ref int ncidin, ref int meshid, ref int networkid)
        {
            return ionc_get_network_id_from_mesh_id_dll(ref ncidin, ref meshid, ref networkid);
        }
        #endregion

        public int ionc_get_meshgeom(ref int ioncid, ref int meshid, ref int networkid, ref meshgeom meshgeom, ref int start_index, ref bool includeArrays)
        {
            return ionc_get_meshgeom_dll(ref ioncid, ref meshid, ref networkid, ref meshgeom, ref start_index, ref includeArrays);
        }

        public int ionc_get_meshgeom(ref int ioncid, ref int meshid, ref int networkid, ref meshgeom meshgeom)
        {
            return ionc_get_meshgeom_v1_dll(ref ioncid, ref meshid, ref networkid, ref meshgeom);
        }

        public int ionc_put_network(ref int ioncid, ref int networkid, ref meshgeom networkgeom, ref meshgeomdim networkgeomdim)
        {
            return ionc_put_network_dll(ref  ioncid, ref networkid, ref  networkgeom, ref networkgeomdim);
        }

        public int ionc_get_meshgeom_dim(ref int ioncid, ref int meshid, ref int networkid, ref meshgeomdim meshgeomdim)
        {
            return ionc_get_meshgeom_dim_dll(ref  ioncid, ref  meshid, ref networkid, ref meshgeomdim);
        }

        public int ionc_def_mesh_ids(ref int ioncid, ref int meshid, ref int iconvtype)
        {
            return ionc_def_mesh_ids_dll(ref ioncid, ref meshid, ref iconvtype);
        }

        public int ionc_put_var_chars(ref int ioncid, ref int meshid, StringBuilder varname, interop_charinfo[] values, ref int nvalues)
        {
            return ionc_put_var_chars_dll(ref ioncid, ref meshid, varname, values, ref nvalues);
        }

        public int ionc_get_var_chars(ref int ioncid, ref int meshid, StringBuilder varname, interop_charinfo[] values, ref int nvalues)
        {
            return ionc_get_var_chars_dll(ref ioncid, ref meshid, varname, values, ref nvalues);
        }

        public int ionc_put_meshgeom(ref int ioncid, ref int meshid, ref int networkid, ref meshgeom meshgeom, ref meshgeomdim meshgeomdim, string c_meshname, string c_networkName, ref int start_index)
        {
            return ionc_put_meshgeom_dll(ref ioncid, ref  meshid, ref  networkid, ref  meshgeom, ref  meshgeomdim,  c_meshname,  c_networkName, ref  start_index);
        }

        public int ionc_put_meshgeom(ref int ioncid, ref int meshid, ref int networkid, ref meshgeom meshgeom, ref meshgeomdim meshgeomdim)
        {
            return ionc_put_meshgeom_v1_dll(ref ioncid, ref meshid, ref networkid, ref meshgeom, ref meshgeomdim);
        }

        public int ionc_inq_varid_by_standard_name(ref int ioncid, ref int meshId, ref int location, string standardName, ref int varId)
        {
            return ionc_inq_varid_by_standard_name_dll(ref ioncid, ref meshId, ref location, standardName, ref varId);
        }
        public virtual int InqueryVariableId(int ioncId, int meshId, string varName, ref int varId)
        {
            return ionc_inq_varid_dll(ref ioncId, ref meshId, varName, ref varId);
        }
        public virtual int DefineVariable(int ioncId, int meshId, int varId, int type, int locationType, string varName, string standardName, string longName, string unit, double fillValue)
        {
            return ionc_def_var_dll(ref ioncId, ref meshId, ref varId, ref type, ref locationType, varName, standardName,
             longName, unit, ref fillValue);
        }
        public virtual int PutVariable(int ioncId, int meshId, int locationType, string varname, IntPtr valuesPtr, int numberOfValues)
        {
            return ionc_put_var_dll(ref ioncId, ref meshId, ref locationType, varname, ref valuesPtr, ref numberOfValues);
        }
    }
}
