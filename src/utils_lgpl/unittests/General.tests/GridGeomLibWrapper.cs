using System;
using System.Runtime.InteropServices;

namespace General.tests
{
    //this class wraps the single library functions
    public class GridGeomLibWrapper
    {
        public static class LibDetails
        {
            public const int MAXDIMS = 6;
            public const int MAXSTRLEN = 255;
            public const string LIB_NAME = "gridgeom";
            public const string LIB_DLL_NAME = "gridgeom.dll";

            public const string NETCDF_DEP = "netcdf";
            public const string NETCDF_LIB_NAME = "io_netcdf";
            public const string NETCDF_DLL_NAME = "io_netcdf.dll";
        }
        #region ggeo_functions
        /// <summary>
        /// Converts 1d ugrid coordinates to x-y coordinates
        /// </summary>
        /// <param name="c_branchids">The branch ids</param>
        /// <param name="c_branchoffsets">The branch offsets</param>
        /// <param name="c_geopointsX">The x coordiantes of the geometrical points</param>
        /// <param name="c_geopointsY">The y coordiantes of the geometrical points</param>
        /// <param name="c_nbranchgeometrynodes">The number of geometrical points per branch</param>
        /// <param name="c_branchlengths">The branch lengths</param>
        /// <param name="c_meshXCoords">The x coordinates of the mesh points</param>
        /// <param name="c_meshYCoords">The y coordinates of the mesh points</param>
        /// <param name="nbranches">The number of branches</param>
        /// <param name="ngeopoints">The number of geometrical points</param>
        /// <param name="nmeshnodes">The number of mesh nodes</param>
        /// <returns></returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ggeo_get_xy_coordinates", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ggeo_get_xy_coordinates_dll(
            [In] ref IntPtr c_branchids,
            [In] ref IntPtr c_branchoffsets,
            [In] ref IntPtr c_geopointsX,
            [In] ref IntPtr c_geopointsY,
            [In] ref IntPtr c_nbranchgeometrynodes,
            [In] ref IntPtr c_branchlengths,
            [In] ref int jsferic,
            [In, Out] ref IntPtr c_meshXCoords,
            [In, Out] ref IntPtr c_meshYCoords,
            [In] ref int nbranches,
            [In] ref int ngeopoints,
            [In] ref int nmeshnodes);

        /// <summary>
        /// Use meshgeom to fill kn matrix
        /// </summary>
        /// <param name="meshgeom"> The data structure containing the mesh information</param>
        /// <param name="meshgeomdim">The data structure containing the mesh dimensions</param>
        /// <returns></returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ggeo_convert", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ggeo_convert_dll([In, Out] ref meshgeom meshgeom, [In] ref meshgeomdim meshgeomdim, [In] ref int startIndex);

        /// <summary>
        /// Makes the 1d/2d links (results are stored in memory)
        /// </summary>
        /// <param name="c_nin"></param>
        /// <param name="c_xpl"></param>
        /// <param name="c_ypl"></param>
        /// <param name="c_zpl"></param>
        /// <param name="c_nOneDMask"></param>
        /// <param name="c_oneDmask"></param>
        /// <param name="c_jsferic"></param>
        /// <param name="c_jasfer3D"></param>
        /// <param name="c_jglobe"></param>
        /// <returns></returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ggeo_make1D2Dinternalnetlinks", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ggeo_make1D2Dinternalnetlinks_dll(ref int c_npl, [In] ref IntPtr c_xpl, [In] ref IntPtr c_ypl, [In] ref IntPtr c_zpl, [In] ref int c_nOneDMask, [In] ref IntPtr c_oneDmask,  ref int c_jsferic, ref int c_jasfer3D, ref int c_jglobe);


        /// <summary>
        /// Make 1D2D embedded links
        /// </summary>
        /// <param name="c_jsferic"></param>
        /// <param name="c_jasfer3D"></param>
        /// <param name="c_nOneDMask"></param>
        /// <param name="c_oneDmask"></param>
        /// <returns></returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ggeo_make1D2Dembeddedlinks", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ggeo_make1D2Dembeddedlinks_dll(ref int c_jsferic, ref int c_jasfer3D, [In] ref int c_nOneDMask, [In] ref IntPtr c_oneDmask);


        /// <summary>
        /// Makes lateral 1d2d links. 1d-2d river connections connections.With this function multiple 2d boundary cells can be connected to 1d mesh points. 
        ///Please note that the gridgeom library has to be initialized before this function can be called.
        /// </summeray>
        /// <param name="c_npl">The number of polygon nodes.</param>
        /// <param name="c_xpl">The x coordinates of the polygon nodes.</param>
        /// <param name="c_ypl">The y coordinates of the polygon nodes.</param>
        /// <param name="c_zpl">The z coordinates of the polygon nodes.</param>
        /// <param name="c_nOneDMask">The size of the 1d mask, should be equal to the number of 1d computational nodes</param>
        /// <param name="c_oneDmask">The size of the 1d mask.</param>
        /// <param name="c_jsferic">Cartisian (0) or spheric (1)</param>
        /// <param name="c_jasfer3D"></param>
        /// <param name="c_searchRadius">The radius where to search for boundary cells from the 1d poind</param>
        /// <returns></returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ggeo_make1D2DRiverLinks",
            CallingConvention = CallingConvention.Cdecl)]
        public static extern int ggeo_make1D2DRiverLinks_dll(ref int c_npl, [In] ref IntPtr c_xpl, [In] ref IntPtr c_ypl, [In] ref IntPtr c_zpl, ref int c_nOneDmask, [In] ref IntPtr c_OneDmask,
            ref int c_jsferic, ref int c_jasfer3D, ref double c_searchRadius);


        /// <summary>
        /// Use 1d array to fill kn matrix
        /// </summary>
        /// <param name="c_meshXCoords">The x coordinates of the mesh points</param>
        /// <param name="c_meshYCoords">The y coordinates of the mesh points</param>
        /// <param name="c_branchids">The branch ids</param>
        /// <param name="nbranches">The number of branches</param>
        /// <param name="nmeshnodes">The number of mesh nodes</param>
        /// <returns></returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ggeo_convert_1d_arrays", CallingConvention = CallingConvention.Cdecl)]       
        public static extern int ggeo_convert_1d_arrays_dll([In] ref IntPtr c_meshXCoords, [In] ref IntPtr c_meshYCoords, [In] ref IntPtr c_branchoffset, [In] ref IntPtr c_branchlength, [In] ref IntPtr c_branchids, [In] ref IntPtr c_sourcenodeid, [In] ref IntPtr c_targetnodeid, [In] ref int nbranches, [In] ref int nmeshnodes, [In] ref int startIndex);


        /// <summary>
        /// Gets the number of 1d-2d links produced by ggeo_make1D2Dinternalnetlinks_dll
        /// </summary>
        /// <param name="nlinks">The number of links</param>
        /// <param name="linkType">The link type</param>
        /// <returns></returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ggeo_get_links_count", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ggeo_get_links_count_dll([In, Out] ref int nlinks, [In] ref int linkType);

        /// <summary>
        /// Gets the number the 1d-2d links produced by ggeo_make1D2Dinternalnetlinks_dll
        /// </summary>
        /// <param name="arrayfrom">The cell indexes where the links start</param>
        /// <param name="arrayto">The node indexes where the links end</param>
        /// <param name="nlinks">The number of links</param>
        /// <param name="linkType">The link type</param>
        /// <returns></returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ggeo_get_links", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ggeo_get_links_dll([In, Out] ref IntPtr arrayfrom, [In, Out] ref IntPtr arrayto, [In] ref int nlinks, [In] ref int linkType, [In] ref int startIndex);

        /// <summary>
        /// This function fills the memory pointed by c_edgenodes
        /// </summary>
        /// <param name="c_branchoffset">mesh points offsets</param>
        /// <param name="c_branchlength">network branches lengths</param>
        /// <param name="c_branchids">mesh points branch ids</param>
        /// <param name="c_nedge_nodes">network edges nodes</param>
        /// <param name="c_edgenodes">mesh edge nodes</param>
        /// <param name="num_branches">number of network branches</param>
        /// <param name="num_nodes">mesh number of nodes</param>
        /// <param name="num_edge_nodes">mesh number of edge nodes</param>
        /// <param name="startIndex">start index</param>
        /// <returns></returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ggeo_create_edge_nodes", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ggeo_create_edge_nodes_dll(
            [In] ref IntPtr c_branchoffset, 
            [In] ref IntPtr c_branchlength, 
            [In] ref IntPtr c_branchids, 
            [In] ref IntPtr c_nedge_nodes, 
            [In, Out] ref IntPtr c_edgenodes, 
            [In] ref int num_branches, 
            [In] ref int num_nodes, 
            [In] ref int num_edge_nodes, 
            [In] ref int startIndex);

        /// <summary>
        /// This function returns the number of mesh edge_nodes (num_edge_nodes) 
        /// </summary>
        /// <param name="c_branchoffset">mesh points offsets</param>
        /// <param name="c_branchlength">network branches lengths</param>
        /// <param name="c_branchids">mesh points branch ids</param>
        /// <param name="c_nedge_nodes">network edges nodes</param>
        /// <param name="c_edgenodes">mesh edge nodes, 2 * num_edge_nodes</param>
        /// <param name="num_branches">number of network branches</param>
        /// <param name="num_nodes">mesh number of nodes</param>
        /// <param name="num_edge_nodes">mesh number of edge nodes</param>
        /// <param name="startIndex">start index</param>
        /// <returns></returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ggeo_count_edge_nodes", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ggeo_count_edge_nodes_dll(
            [In] ref IntPtr c_branchoffset, 
            [In] ref IntPtr c_branchlength, 
            [In] ref IntPtr c_branchids, 
            [In] ref IntPtr c_nedge_nodes, 
            [In] ref int num_branches, 
            [In] ref int num_nodes, 
            [In,Out] ref int num_edge_nodes, 
            [In] ref int startIndex);

        /// <summary>
        /// Deallocation of library memory, but not of meshgeom structures used for dll communication
        /// </summary>
        /// <returns></returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ggeo_deallocate",
            CallingConvention = CallingConvention.Cdecl)]
        public static extern int ggeo_deallocate_dll();


        /// <summary>
        /// Count the number of cells of a given mesh (defined by meshDimIn, meshIn) and returns the results in meshDimOut
        /// </summary>
        /// <param name="meshDimIn"> client defined dimensions</param>
        /// <param name="meshIn"> client allocated <see cref="MeshGeometry"/> structure </param>
        /// <param name="meshDimOut"> server defined dimension</param>
        /// <returns></returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ggeo_count_cells", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ggeo_count_cells_dll([In] ref meshgeomdim meshDimIn, [In] ref meshgeom meshIn, [In, Out] ref meshgeomdim meshDimOut);

        /// <summary>
        /// Finds the cells and related quantities
        /// </summary>
        /// <param name="meshDimIn"> client defined dimensions</param>
        /// <param name="meshIn"> client allocated meshgeom structure </param>
        /// <param name="meshDimOut"> server defined dimension</param>
        /// <param name="meshOut"> server allocated meshgeom structure </param>
        /// <param name="startIndex"> for index based array, the start index </param>
        /// <returns></returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ggeo_find_cells", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ggeo_find_cells_dll([In] ref meshgeomdim meshDimIn, [In,Out] ref meshgeom meshInOut);


        /// <summary>
        /// Destroys the memory allocated by the server (fortran library)
        /// </summary>
        /// <param name="meshDimIn">server defined dimension</param>
        /// <param name="meshIn">server allocated meshgeom structure, to destroy</param>
        /// <returns></returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "ggeo_meshgeom_destructor", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ggeo_meshgeom_destructor_dll([In, Out] ref meshgeomdim meshDimIn, [In, Out] ref meshgeom meshIn);

        #endregion ggeo_functions


        public int ggeo_get_xy_coordinates(
            ref IntPtr c_branchids,
            ref IntPtr c_branchoffsets,
            ref IntPtr c_geopointsX,
            ref IntPtr c_geopointsY,
            ref IntPtr c_nbranchgeometrynodes,
            ref IntPtr c_branchlengths,
            ref IntPtr c_meshXCoords,
            ref IntPtr c_meshYCoords,
            ref int jsferic,
            ref int nbranches,
            ref int ngeopoints,
            ref int nmeshnodes
        )
        {
          int ierr = ggeo_get_xy_coordinates_dll(
                ref  c_branchids,
                ref  c_branchoffsets,
                ref  c_geopointsX,
                ref  c_geopointsY,
                ref  c_nbranchgeometrynodes,
                ref  c_branchlengths,
                ref  jsferic,
                ref  c_meshXCoords,
                ref  c_meshYCoords,
                ref  nbranches,
                ref  ngeopoints,
                ref  nmeshnodes);
            return ierr;
        }


        public int ggeo_convert(ref meshgeom c_meshgeom, ref meshgeomdim c_meshgeomdim, ref int startIndex)
        {
            int ierr = ggeo_convert_dll(ref  c_meshgeom, ref  c_meshgeomdim, ref startIndex);
            return ierr;
        }
         
        public int ggeo_make1D2Dinternalnetlinks(ref int c_nin, ref IntPtr c_xpl, ref IntPtr c_ypl, ref IntPtr c_zpl, ref int c_nOneDMask, ref IntPtr c_oneDmask)
        {
            // the following three variables might become part of the function api
            int c_jsferic = 0;
            int c_jasfer3D = 0;
            int c_jglobe = 0;
            int ierr = ggeo_make1D2Dinternalnetlinks_dll(ref c_nin, ref c_xpl, ref c_ypl, ref c_zpl, ref c_nOneDMask, ref c_oneDmask, ref c_jsferic, ref c_jasfer3D, ref c_jglobe);
            return ierr;
        }



        public int ggeo_make1D2DEmbeddedLinks(ref int c_nOneDMask, ref IntPtr c_oneDmask)
        {
            // the following three variables might become part of the function api
            int c_jsferic  = 0;
            int c_jasfer3D = 0;
            int ierr = ggeo_make1D2Dembeddedlinks_dll(ref c_jsferic, ref c_jasfer3D, ref c_nOneDMask, ref c_oneDmask);
            return ierr;
        }


        public int ggeo_make1D2DRiverLinks(ref int c_nin, ref IntPtr c_xpl, ref IntPtr c_ypl, ref IntPtr c_zpl, ref int c_nOneDMask, ref IntPtr c_oneDmask)
        {
            // the following three variables might become part of the function api
            int c_jsferic = 0;
            int c_jasfer3D = 0;
            double c_searchRadius = 5000.0;
            int ierr = ggeo_make1D2DRiverLinks_dll(ref c_nin, ref c_xpl, ref c_ypl, ref c_zpl, ref c_nOneDMask, ref c_oneDmask, ref c_jsferic, ref c_jasfer3D, ref c_searchRadius);

            return ierr;
        }

        public int ggeo_deallocate()
        {
            int ierr = ggeo_deallocate_dll();
            return ierr;
        }

        public int ggeo_convert_1d_arrays(ref IntPtr c_meshXCoords, ref IntPtr c_meshYCoords, ref IntPtr c_branchoffset, ref IntPtr c_branchlength, ref IntPtr c_branchids, ref IntPtr c_sourcenodeid, ref IntPtr c_targetnodeid, ref int nbranches, ref int nmeshnodes, ref int startIndex)
        {
            int ierr = ggeo_convert_1d_arrays_dll(ref c_meshXCoords, ref c_meshYCoords, ref c_branchoffset, ref c_branchlength, ref c_branchids, ref c_sourcenodeid, ref c_targetnodeid, ref nbranches, ref nmeshnodes, ref startIndex);
            return ierr;
        }

        public int ggeo_get_links_count(ref int nbranches, ref int linkType)
        {
            int ierr = ggeo_get_links_count_dll(ref nbranches, ref linkType);
            return ierr;
        }

        public int ggeo_get_links(ref IntPtr arrayfrom, ref IntPtr arrayto, ref int nlinks, ref int linkType, ref int startIndex)
        {
            int ierr = ggeo_get_links_dll(ref arrayfrom, ref arrayto, ref nlinks, ref linkType, ref startIndex);
            return ierr;
        }


        public int ggeo_create_edge_nodes(ref IntPtr c_branchoffset, ref IntPtr c_branchlength, ref IntPtr c_branchids, ref IntPtr c_nedge_nodes, ref IntPtr c_edgenodes, ref int nBranches, ref int nNodes, ref int nEdgeNodes, ref int startIndex)
        {
            int ierr = ggeo_create_edge_nodes_dll(ref c_branchoffset, ref c_branchlength, ref c_branchids, ref c_nedge_nodes, ref c_edgenodes, ref nBranches, ref nNodes, ref nEdgeNodes, ref startIndex);
            return ierr;
        }

        public int ggeo_count_edge_nodes(ref IntPtr c_branchoffset, ref IntPtr c_branchlength, ref IntPtr c_branchids, ref IntPtr c_nedge_nodes, ref int nBranches, ref int nNodes, ref int nEdgeNodes, ref int startIndex)
        {
            int ierr = ggeo_count_edge_nodes_dll(ref c_branchoffset, ref c_branchlength, ref c_branchids, ref c_nedge_nodes, ref nBranches, ref nNodes, ref nEdgeNodes, ref startIndex);
            return ierr;
        }

        public int ggeo_find_cells(ref meshgeomdim meshDimIn,  ref meshgeom meshInOut)
        {
            int ierr = ggeo_find_cells_dll(ref meshDimIn, ref meshInOut);
            return ierr;
        }

        public int ggeo_count_cells(ref meshgeomdim meshDimIn, ref meshgeom meshInOut, ref meshgeomdim meshDimOut)
        {
            int ierr = ggeo_count_cells_dll(ref meshDimIn, ref meshInOut, ref meshDimOut);
            return ierr;
        }

        public int ggeo_meshgeom_destructor(ref meshgeomdim meshDimIn, ref meshgeom meshIn)
        {
            int ierr = ggeo_meshgeom_destructor_dll(ref meshDimIn, ref meshIn);
            return ierr;
        }


    }
}
