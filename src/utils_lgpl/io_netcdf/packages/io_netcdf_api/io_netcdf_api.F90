!----- LGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2025.
!
!  This library is free software; you can redistribute it and/or
!  modify it under the terms of the GNU Lesser General Public
!  License as published by the Free Software Foundation version 2.1.
!
!  This library is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!  Lesser General Public License for more details.
!
!  You should have received a copy of the GNU Lesser General Public
!  License along with this library; if not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D" and "Deltares"
!  are registered trademarks of Stichting Deltares, and remain the property of
!  Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------

!
!

!> \file
!! Basic API for io_netcdf library routines.
!! NOTE: most functionality is in underlying static lib for io_netcdf.
!! TODO: AvD: Check whether DLLEXPORTs from static lib actually end up in DLL.

! NOTE: this module contains mainly wrapper functions around their
! counterpart functions in the static library io_netcdf, and exposes
! the wrappers in the DLL API.
! This is necessary to allow that the static functions can still also
! be called natively when applications use the statically linked library
! (which is based on compiler-dependent mangled names).


!> The io_netcdf dynamic library API functions.
!! Mainly consists of wrappers around underlying static libraries.
module io_netcdf_api
use io_netcdf
use iso_c_binding
implicit none

!-------------------------------------------------------------------------------
contains
!-------------------------------------------------------------------------------


!> Given an error number, return an error message.
!!
!! Use this when a previous function call has returned a nonzero error status.
!! Note that the error number may be an IONC error, but also an underlying UG error.
function ionc_strerror_dll(ierr) result(c_strptr) bind(C, name="ionc_strerror")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_strerror_dll
   integer(kind=c_int),    intent(in   ) :: ierr     !< Integer error code for which to return the error message.
   type(c_ptr)                           :: c_strptr !< String variable in which the message will be stored.

   character(len=MAXSTRLEN) :: str
   character(kind=c_char, len=1), target :: c_str(MAXSTRLEN)

   str = ionc_strerror(ierr)
   c_str = string_to_char_array(str, len_trim(str))
   c_strptr = c_loc(c_str)

end function ionc_strerror_dll


!> Returns the integer value for a named constant, for example 'IONC_NOERR'.
!! When requested constant does not exist, the returned value is undefined, and ierr contains an error code.
function ionc_get_constant_dll(c_constname, constvalue) result(ierr) bind(C, name="ionc_get_constant")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_constant_dll
   character(kind=c_char), intent(in)    :: c_constname(MAXSTRLEN)  !< The name of the requested constant.
   integer(kind=c_int),    intent(  out) :: constvalue              !< The integer value of the requested constant.
   integer(kind=c_int)                   :: ierr                    !< Result status (IONC_NOERR if successful).

   character(len=MAXSTRLEN) :: constname

   ! Store the name
   constname = char_array_to_string(c_constname, strlen(c_constname))

   ierr = ionc_get_constant(constname, constvalue)

end function ionc_get_constant_dll


!> Tries to create a NetCDF file and initialize based on its specified conventions.
function ionc_create_dll(c_path, mode, ioncid) result(ierr) bind(C, name="ionc_create")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_create_dll
    character(kind=c_char), intent(in) :: c_path(MAXSTRLEN)      !< File name for netCDF dataset to be opened.
    integer(kind=c_int),    intent(in) :: mode      !< NetCDF open mode, e.g. NF90_NOWRITE.
    integer(kind=c_int),    intent(out):: ioncid   !< inout The io_netcdf dataset id (this is not the NetCDF ncid, which is stored in datasets(ioncid)%ncid.
    integer(kind=c_int)                :: ierr      !< Result status (IONC_NOERR if successful).

    character(len=MAXSTRLEN) :: path

    ! Store the name
    path = char_array_to_string(c_path, strlen(c_path))

    ! We always create ugrid files
    ierr = ionc_create(path, mode, ioncid, IONC_CONV_UGRID)
end function ionc_create_dll


!> Inquire the NetCDF conventions used in the dataset.
function ionc_inq_conventions_dll(ioncid, iconvtype, convversion) result(ierr) bind(C, name="ionc_inq_conventions")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_inq_conventions_dll
   integer(kind=c_int), intent(in)  :: ioncid    !< The IONC data set id.
   integer(kind=c_int), intent(out) :: iconvtype !< The NetCDF conventions type of the dataset.
   real(kind=c_double), intent(out) :: convversion !< The NetCDF conventions version of the dataset.
   integer(kind=c_int)              :: ierr      !< Result status, ionc_noerr if successful.

   ierr = ionc_inq_conventions(ioncid, iconvtype, convversion)
end function ionc_inq_conventions_dll


!> Checks whether the specified data set adheres to a specific set of conventions.
!! Datasets may adhere to multiple conventions at the same time, so use this method
!! to check for individual conventions.
function ionc_adheresto_conventions_dll(ioncid, iconvtype) result(does_adhere) bind(C, name="ionc_adheresto_conventions")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_adheresto_conventions_dll
   integer(kind=c_int), intent(in)  :: ioncid      !< The IONC data set id.
   integer(kind=c_int), intent(in)  :: iconvtype   !< The NetCDF conventions type to check for.
   logical(kind=c_bool)             :: does_adhere !< Whether or not the file adheres to the specified conventions.

   does_adhere = ionc_adheresto_conventions(ioncid, iconvtype)
end function ionc_adheresto_conventions_dll


!> Tries to open a NetCDF file and initialize based on its specified conventions.
function ionc_open_dll(c_path, mode, ioncid, iconvtype, convversion) result(ierr) bind(C, name="ionc_open")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_open_dll
   use iso_c_binding
   character(kind=c_char), intent(in   ) :: c_path(MAXSTRLEN)      !< File name for netCDF dataset to be opened.
   integer(kind=c_int),           intent(in   ) :: mode      !< NetCDF open mode, e.g. NF90_NOWRITE.
   integer(kind=c_int),           intent(  out) :: ioncid    !< The io_netcdf dataset id (this is not the NetCDF ncid, which is stored in datasets(ioncid)%ncid.
   integer(kind=c_int),           intent(inout) :: iconvtype !< The detected conventions in the file.
   real(kind=c_double),           intent(inout) :: convversion !< The detected conventions in the file.
!   integer(kind=c_int), optional, intent(inout) :: chunksize !< (optional) NetCDF chunksize parameter.
   integer(kind=c_int)                          :: ierr      !< Result status (IONC_NOERR if successful).

   character(len=MAXSTRLEN) :: path
   !character(len=strlen(c_path)) :: nc_file

   ! Store the name
   path = char_array_to_string(c_path, strlen(c_path))

   ierr = ionc_open(path, mode, ioncid, iconvtype, convversion)
end function ionc_open_dll


!> Tries to close an open io_netcdf data set.
function ionc_close_dll(ioncid) result(ierr) bind(C, name="ionc_close")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_close_dll
   integer(kind=c_int),           intent(in   ) :: ioncid    !< The io_netcdf dataset id (this is not the NetCDF ncid, which is stored in datasets(ioncid)%ncid.
   integer(kind=c_int)                          :: ierr      !< Result status (IONC_NOERR if successful).

   ierr = ionc_close(ioncid)
end function ionc_close_dll

!> Tries to end define mode of an open io_netcdf data set so we can start putting data in the file.
function ionc_end_define_mode_dll(ioncid) result(ierr) bind(C, name="ionc_end_define_mode")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_end_define_mode_dll
   integer(kind=c_int),           intent(in   ) :: ioncid    !< The io_netcdf dataset id (this is not the NetCDF ncid, which is stored in datasets(ioncid)%ncid.
   integer(kind=c_int)                          :: ierr      !< Result status (IONC_NOERR if successful).

   ierr = ionc_enddef(ioncid)
end function ionc_end_define_mode_dll

!> Tries to start define mode of an open io_netcdf data set to define new variables or dimensions, etc.
function ionc_start_define_mode_dll(ioncid) result(ierr) bind(C, name="ionc_start_define_mode")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_start_define_mode_dll
   integer(kind=c_int),           intent(in   ) :: ioncid    !< The io_netcdf dataset id (this is not the NetCDF ncid, which is stored in datasets(ioncid)%ncid.
   integer(kind=c_int)                          :: ierr      !< Result status (IONC_NOERR if successful).

   ierr = ionc_redef(ioncid)
end function ionc_start_define_mode_dll

!> Gets the number of mesh from a data set.
function ionc_get_mesh_count_dll(ioncid, nmesh) result(ierr) bind(C, name="ionc_get_mesh_count")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_mesh_count_dll
   integer(kind=c_int),             intent(in)    :: ioncid  !< The IONC data set id.
   integer(kind=c_int),             intent(  out) :: nmesh   !< Number of meshes.
   integer(kind=c_int)                            :: ierr    !< Result status, ionc_noerr if successful.

   ierr = ionc_get_mesh_count(ioncid, nmesh)
end function ionc_get_mesh_count_dll

!> Gets the name of the mesh topology variable in an open dataset.
function ionc_get_mesh_name_dll(ioncid, meshid, c_meshname) result(ierr) bind(C, name="ionc_get_mesh_name")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_mesh_name_dll
   use iso_c_binding
   integer,                      intent(in)    :: ioncid   !< The IONC data set id.
   integer,                      intent(in)    :: meshid   !< The mesh id in the specified data set.
   character(kind=c_char,len=1), intent(  out) :: c_meshname(MAXSTRLEN) !< The name of the mesh topology variable.

   character(len=MAXSTRLEN) :: meshname !< The name of the mesh geometry.
   integer                            :: ierr     !< Result status, ionc_noerr if successful.


   meshname = ''
   ierr = ionc_get_mesh_name(ioncid, meshid, meshname)
   c_meshname= string_to_char_array(meshname,len_trim(meshname))

end function ionc_get_mesh_name_dll

!> Gets the number of nodes in a single mesh from a data set.
function ionc_get_node_count_dll(ioncid, meshid, nnode) result(ierr) bind(C, name="ionc_get_node_count")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_node_count_dll
   integer(kind=c_int),             intent(in)    :: ioncid  !< The IONC data set id.
   integer(kind=c_int),             intent(in)    :: meshid  !< The mesh id in the specified data set.
   integer(kind=c_int),             intent(  out) :: nnode   !< Number of nodes.
   integer(kind=c_int)                            :: ierr    !< Result status, ionc_noerr if successful.

   ierr = ionc_get_node_count(ioncid, meshid, nnode)
end function ionc_get_node_count_dll


!> Gets the number of edges in a single mesh from a data set.
function ionc_get_edge_count_dll(ioncid, meshid, nedge) result(ierr) bind(C, name="ionc_get_edge_count")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_edge_count_dll
   integer(kind=c_int),             intent(in)    :: ioncid  !< The IONC data set id.
   integer(kind=c_int),             intent(in)    :: meshid  !< The mesh id in the specified data set.
   integer(kind=c_int),             intent(  out) :: nedge   !< Number of edges.
   integer(kind=c_int)                            :: ierr    !< Result status, ionc_noerr if successful.

   ierr = ionc_get_edge_count(ioncid, meshid, nedge)
end function ionc_get_edge_count_dll


!> Gets the number of faces in a single mesh from a data set.
function ionc_get_face_count_dll(ioncid, meshid, nface) result(ierr) bind(C, name="ionc_get_face_count")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_face_count_dll
   integer(kind=c_int),             intent(in)    :: ioncid  !< The IONC data set id.
   integer(kind=c_int),             intent(in)    :: meshid  !< The mesh id in the specified data set.
   integer(kind=c_int),             intent(  out) :: nface   !< Number of faces.
   integer(kind=c_int)                            :: ierr    !< Result status, ionc_noerr if successful.

   ierr = ionc_get_face_count(ioncid, meshid, nface)
end function ionc_get_face_count_dll


!> Gets the maximum number of nodes for any face in a single mesh from a data set.
function ionc_get_max_face_nodes_dll(ioncid, meshid, nmaxfacenodes) result(ierr) bind(C, name="ionc_get_max_face_nodes")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_max_face_nodes_dll
   integer(kind=c_int),             intent(in)    :: ioncid        !< The IONC data set id.
   integer(kind=c_int),             intent(in)    :: meshid        !< The mesh id in the specified data set.
   integer(kind=c_int),             intent(  out) :: nmaxfacenodes !< The maximum number of nodes per face in the mesh.
   integer(kind=c_int)                            :: ierr          !< Result status, ionc_noerr if successful.

   ierr = ionc_get_max_face_nodes(ioncid, meshid, nmaxfacenodes)
end function ionc_get_max_face_nodes_dll


!> Gets the x,y coordinates for all nodes in a single mesh from a data set.
function ionc_get_node_coordinates_dll(ioncid, meshid, c_xptr, c_yptr, nnode) result(ierr) bind(C, name="ionc_get_node_coordinates")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_node_coordinates_dll
   integer(kind=c_int), intent(in)  :: ioncid !< The IONC data set id.
   integer(kind=c_int), intent(in)  :: meshid !< The mesh id in the specified data set.
   type(c_ptr),         intent(in)  :: c_xptr !< Pointer to an existing array for x-coordinates
   type(c_ptr),         intent(in)  :: c_yptr !< Pointer to an existing array for y-coordinates
   integer(kind=c_int), intent(in)  :: nnode  !< The number of nodes in the mesh. TODO: AvD: remove this somehow, now only required to call c_f_pointer
   integer(kind=c_int)              :: ierr   !< Result status, ionc_noerr if successful.

   double precision, pointer :: xptr(:), yptr(:)!

   call c_f_pointer(c_xptr, xptr, (/ nnode /))
   call c_f_pointer(c_yptr, yptr, (/ nnode /))

   ierr = ionc_get_node_coordinates(ioncid, meshid, xptr, yptr)
end function ionc_get_node_coordinates_dll


!> Puts the x,y coordinates for all nodes in a single mesh from a data set.
function ionc_put_node_coordinates_dll(ioncid, meshid, c_xptr, c_yptr, nnode) result(ierr) bind(C, name="ionc_put_node_coordinates")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_put_node_coordinates_dll
   integer(kind=c_int), intent(in)  :: ioncid !< The IONC data set id.
   integer(kind=c_int), intent(in)  :: meshid !< The mesh id in the specified data set.
   type(c_ptr),         intent(in)  :: c_xptr !< Pointer to an existing array containing x-coordinates
   type(c_ptr),         intent(in)  :: c_yptr !< Pointer to an existing array containing y-coordinates
   integer(kind=c_int), intent(in)  :: nnode  !< The number of nodes in the mesh. TODO: AvD: remove this somehow, now only required to call c_f_pointer
   integer(kind=c_int)              :: ierr   !< Result status, ionc_noerr if successful.

   double precision, pointer :: xptr(:), yptr(:)!

   call c_f_pointer(c_xptr, xptr, (/ nnode /))
   call c_f_pointer(c_yptr, yptr, (/ nnode /))

   ierr = ionc_put_node_coordinates(ioncid, meshid, xptr, yptr)
end function ionc_put_node_coordinates_dll


!> Gets the edge-facee connectivity table for all edges in the specified mesh.
function ionc_get_edge_faces_dll(ioncid, meshid, c_edge_faces_ptr, nedge, fillvalue, start_index) result(ierr) bind(C, name="ionc_get_edge_faces")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_edge_nodes_dll
   integer(kind=c_int), intent(in)    :: ioncid      !< The IONC data set id.
   integer(kind=c_int), intent(in)    :: start_index !< The requested start index
   integer(kind=c_int), intent(in)    :: meshid       !< The mesh id in the specified data set.
   type(c_ptr),         intent(in)    :: c_edge_faces_ptr !< Pointer to an existing array for the edge-node connectivity table.
   integer(kind=c_int), intent(in)    :: nedge   !< The number of edges in the mesh.
   integer(kind=c_int)                :: fillvalue    !< Scalar for getting the fill value parameter for the requested variable.
   integer(kind=c_int)                :: ierr    !< Result status, ionc_noerr if successful.

   integer, pointer :: edge_faces(:,:)

   call c_f_pointer(c_edge_faces_ptr, edge_faces, (/ 2 , nedge /))

   ierr = ionc_get_edge_faces(ioncid, meshid, edge_faces, fillvalue, start_index)
end function ionc_get_edge_faces_dll


!> Gets the edge-node connectivity table for all edges in the specified mesh.
function ionc_get_edge_nodes_dll(ioncid, meshid, c_edge_nodes_ptr, nedge, start_index) result(ierr) bind(C, name="ionc_get_edge_nodes")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_edge_nodes_dll
   integer(kind=c_int), intent(in)    :: ioncid  !< The IONC data set id.
   integer(kind=c_int), intent(in)    :: start_index !< The requested start index
   integer(kind=c_int), intent(in)    :: meshid  !< The mesh id in the specified data set.
   type(c_ptr),         intent(in)    :: c_edge_nodes_ptr !< Pointer to array for the edge-node connectivity table.
   integer(kind=c_int), intent(in)    :: nedge  !< The number of edges in the mesh.
   integer(kind=c_int)                :: ierr    !< Result status, ionc_noerr if successful.

   integer, pointer :: edge_nodes(:,:)

   call c_f_pointer(c_edge_nodes_ptr, edge_nodes, (/ 2 , nedge /))

   ierr = ionc_get_edge_nodes(ioncid, meshid, edge_nodes, start_index)
end function ionc_get_edge_nodes_dll


!> Gets the face-edge connectivity table for all faces in the specified mesh.
!! The output face_edges array is supposed to be of exact correct size already.
function ionc_get_face_edges_dll(ioncid, meshid, c_face_edges_ptr, nface, nmax_face_edges, fillvalue, startIndex) result(ierr) bind(C, name="ionc_get_face_edges")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_face_edges_dll
   integer(kind=c_int), intent(in)    :: ioncid  !< The IONC data set id.
   integer(kind=c_int), intent(in)    :: meshid  !< The mesh id in the specified data set.
   type(c_ptr),         intent(in)    :: c_face_edges_ptr !< Pointer to an existing array for the face-edge connectivity table.
   integer(kind=c_int), intent(in)    :: nface  !< The number of faces in the mesh. TODO: AvD: remove this somehow, now only required to call c_f_pointer
   integer(kind=c_int), intent(in)    :: nmax_face_edges  !< The maximum number of edges per face in the mesh. TODO: AvD: remove this somehow, now only required to call c_f_pointer
   integer(kind=c_int)                :: fillvalue    !< Scalar for getting the fill value parameter for the requested variable.
   integer(kind=c_int)                :: ierr    !< Result status, ionc_noerr if successful.
   integer(kind=c_int)                :: startIndex         !< The start index the caller asks for

   integer, pointer :: face_edges(:,:)

   call c_f_pointer(c_face_edges_ptr, face_edges, (/ nmax_face_edges, nface /))

   ierr = ionc_get_face_edges(ioncid, meshid, face_edges, fillvalue, startIndex)
end function ionc_get_face_edges_dll


!> Gets the face-node connectivity table for all faces in the specified mesh.
!! The output face_nodes array is supposed to be of exact correct size already.
function ionc_get_face_nodes_dll(ioncid, meshid, c_face_nodes_ptr, nface, nmaxfacenodes, fillvalue, startIndex) result(ierr) bind(C, name="ionc_get_face_nodes")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_face_nodes_dll
   integer(kind=c_int), intent(in)    :: ioncid             !< The IONC data set id.
   integer(kind=c_int), intent(in)    :: meshid             !< The mesh id in the specified data set.
   type(c_ptr),         intent(in)    :: c_face_nodes_ptr   !< Pointer to an existing array for the face-node connectivity table.
   integer(kind=c_int), intent(in)    :: nface              !< The number of faces in the mesh. TODO: AvD: remove this somehow, now only required to call c_f_pointer
   integer(kind=c_int), intent(in)    :: nmaxfacenodes      !< The maximum number of nodes per face in the mesh. TODO: AvD: remove this somehow, now only required to call c_f_pointer
   integer(kind=c_int)                :: fillvalue          !< Scalar for getting the fill value parameter for the requested variable.
   integer(kind=c_int)                :: ierr               !< Result status, ionc_noerr if successful.
   integer(kind=c_int)                :: startIndex         !< The start index the caller asks for

   integer, pointer :: face_nodes(:,:)

   call c_f_pointer(c_face_nodes_ptr, face_nodes, (/ nmaxfacenodes, nface /))

   ierr = ionc_get_face_nodes(ioncid, meshid, face_nodes, fillvalue, startIndex)
end function ionc_get_face_nodes_dll


!> Gets the epsg code of coordinate system from a data set.
!> TO do we should get the crs here!!
function ionc_get_coordinate_system_dll(ioncid, epsg) result(ierr) bind(C, name="ionc_get_coordinate_system")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_coordinate_system_dll
   integer(kind=c_int),             intent(in)    :: ioncid  !< The IONC data set id.
   integer(kind=c_int),             intent(  out) :: epsg    !< Number of epsg code.
   integer(kind=c_int)                            :: ierr    !< Result status, ionc_noerr if successful.

   ierr = ionc_get_epsg_code(ioncid, epsg)
end function ionc_get_coordinate_system_dll


!> Writes a complete mesh geometry
function ionc_write_geom_ugrid_dll(filename) result(ierr) bind(C, name="ionc_write_geom_ugrid")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_write_geom_ugrid_dll
   character(kind=c_char), intent(in   ) :: filename(MAXSTRLEN)      !< File name for netCDF dataset to be created.
   integer(kind=c_int)                   :: ierr    !< Result status, ionc_noerr if successful.
   character(len=MAXSTRLEN)              :: file

   ! Store the name
   file = char_array_to_string(filename, strlen(filename))
   ierr = ionc_write_geom_ugrid(file)
end function ionc_write_geom_ugrid_dll


!> Writes a complete map file
function ionc_write_map_ugrid_dll(filename) result(ierr) bind(C, name="ionc_write_map_ugrid")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_write_map_ugrid_dll
   character(kind=c_char), intent(in   ) :: filename(MAXSTRLEN)      !< File name for netCDF dataset to be created.
   integer(kind=c_int)                   :: ierr    !< Result status, ionc_noerr if successful.
   character(len=MAXSTRLEN)              :: file

   ! Store the name
   file = char_array_to_string(filename, strlen(filename))
   ierr = ionc_write_map_ugrid(file)
end function ionc_write_map_ugrid_dll

!> Initializes the io_netcdf library, setting up the logger.
function ionc_initialize_dll(c_msg_callback, c_prgs_callback) result(ierr) bind(C, name="ionc_initialize")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_initialize_dll
   type(c_funptr), value    :: c_msg_callback   !< Set a callback that will be called with new messages
   type(c_funptr), value    :: c_prgs_callback  !< Set a callback that will be called with new messages for progress
   integer                  :: ierr             !< Result status, ionc_noerr if successful.

   ierr = ionc_initialize(c_msg_callback, c_prgs_callback)
end function ionc_initialize_dll


!> Returns the number of variables that are available in the specified dataset on the specified mesh.
!! The location type allows to select on specific topological mesh locations
!! (UGRID-compliant, so UG_LOC_FACE/EDGE/NODE/ALL2D).
function ionc_get_var_count_dll(ioncid, meshid, iloctype, nvar) result(ierr) bind(C, name="ionc_get_var_count")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_var_count_dll
   integer(kind=c_int),             intent(in)    :: ioncid   !< The IONC data set id.
   integer(kind=c_int),             intent(in)    :: meshid   !< The mesh id in the specified data set.
   integer(kind=c_int),             intent(in)    :: iloctype !< The topological location on which to select data (UGRID-compliant, so UG_LOC_FACE/EDGE/NODE/ALL2D).
   integer(kind=c_int),             intent(  out) :: nvar     !< Number of variables defined on the requested location type+mesh+dataset.
   integer(kind=c_int)                            :: ierr     !< Result status, ionc_noerr if successful.

   ierr = ionc_get_var_count(ioncid, meshid, iloctype, nvar)

end function ionc_get_var_count_dll


!> Gets a list of variable IDs that are available in the specified dataset on the specified mesh.
!! The location type allows to select on specific topological mesh locations
!! (UGRID-compliant, so UG_LOC_FACE/EDGE/NODE/ALL2D)
function ionc_inq_varids_dll(ioncid, meshid, iloctype, c_varids_ptr, nmaxvar) result(ierr) bind(C, name="ionc_inq_varids")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_inq_varids_dll
   integer(kind=c_int),             intent(in)    :: ioncid   !< The IONC data set id.
   integer(kind=c_int),             intent(in)    :: meshid   !< The mesh id in the specified data set.
   integer(kind=c_int),             intent(in)    :: iloctype !< The topological location on which to select data (UGRID-compliant, so UG_LOC_FACE/EDGE/NODE/ALL2D).
   type(c_ptr),                     intent(in)    :: c_varids_ptr !< Pointer to an existing array for the variable ids.
   integer(kind=c_int),             intent(in)    :: nmaxvar  !< The number of variables in the target array. TODO: AvD: remove this somehow, now only required to call c_f_pointer
   integer(kind=c_int)                            :: ierr    !< Result status, ionc_noerr if successful.

   integer, pointer :: varids(:)
   integer :: nvar

   call c_f_pointer(c_varids_ptr, varids, (/ nmaxvar /))

   ! TODO: AvD: extend the interface of this DLL routine, such that it ALSO RETURNS the nvar number of variables found.
   !            Now only the static library does this.
   ierr = ionc_inq_varids(ioncid, meshid, iloctype, varids, nvar)

end function ionc_inq_varids_dll


!> Gets the variable ID for a data variable that is defined in the specified dataset on the specified mesh.
!! The variable is searched based on variable name (without any "meshnd_" prefix), and which :mesh it is defined on.
function ionc_inq_varid_dll(ioncid, meshid, c_varname, varid) result(ierr) bind(C, name="ionc_inq_varid")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_inq_varid_dll
   integer(kind=c_int),             intent(in)    :: ioncid   !< The IONC data set id.
   integer(kind=c_int),             intent(in)    :: meshid   !< The mesh id in the specified data set.
   character(kind=c_char),          intent(in)    :: c_varname(MAXSTRLEN)  !< The name of the variable to be found. Should be without any "meshnd_" prefix.
   integer(kind=c_int),             intent(  out) :: varid    !< The resulting variable id, if found.
   integer(kind=c_int)                            :: ierr     !< Result status, ionc_noerr if successful.

   character(len=MAXSTRLEN) :: varname
   ! Store the name
   varname = char_array_to_string(c_varname, strlen(c_varname))

   ! TODO: AvD: some error handling if ioncid or meshid is wrong
   ierr = ionc_inq_varid(ioncid, meshid, varname, varid)

end function ionc_inq_varid_dll


!> Gets the variable ID for the variable in the specified dataset on the specified mesh,
!! that also has the specified value for its ':standard_name' attribute, and
!! is defined on the specified topological mesh location (UGRID-compliant, so UG_LOC_FACE/EDGE/NODE/ALL2D)
function ionc_inq_varid_by_standard_name_dll(ioncid, meshid, iloctype, c_stdname, varid) result(ierr) bind(C, name="ionc_inq_varid_by_standard_name")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_inq_varid_by_standard_name_dll
   integer(kind=c_int),             intent(in)    :: ioncid   !< The IONC data set id.
   integer(kind=c_int),             intent(in)    :: meshid   !< The mesh id in the specified data set.
   integer(kind=c_int),             intent(in)    :: iloctype !< The topological location on which to select data (UGRID-compliant, so UG_LOC_FACE/EDGE/NODE/ALL2D).
   character(kind=c_char),          intent(in)    :: c_stdname(MAXSTRLEN)  !< The standard_name value that is searched for.
   integer(kind=c_int),             intent(  out) :: varid    !< The resulting variable id, if found.
   integer(kind=c_int)                            :: ierr     !< Result status, ionc_noerr if successful.

   character(len=MAXSTRLEN) :: stdname
   ! Store the name
   stdname = char_array_to_string(c_stdname, strlen(c_stdname))

   ! TODO: AvD: some error handling if ioncid or meshid is wrong
   ierr = ionc_inq_varid_by_standard_name(ioncid, meshid, iloctype, stdname, varid)

end function ionc_inq_varid_by_standard_name_dll


!> Defines a new variable in an existing IONC dataset and sets up proper meta-attributes.
!! The variable can be defined either on a UGRID mesh, or on a UGRID network (via meshid or networkid, respectively).
!! NOTE: File should still be in define mode.
!! Does not write the actual data yet.
function ionc_def_var_dll(ioncid, meshid, networkid, id_var, itype, iloctype, c_var_name, c_standard_name, c_long_name, & ! id_dims,
                    c_unit, ifill, dfill) result(ierr) bind(C, name="ionc_def_var")  ! , cell_method, cell_measures, crs
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_def_var_dll
   integer(kind=c_int),              intent(in)    :: ioncid    !< The IONC data set id.
   integer(kind=c_int),              intent(in)    :: meshid    !< The mesh id in the specified data set (use 0 when instead a networkid is specified).
   integer(kind=c_int),              intent(in)    :: networkid !< The network id in the specified data set (use 0 when instead a meshid is specified).
   integer(kind=c_int),              intent(  out) :: id_var        !< Created NetCDF variable id.
!   integer, dimension(:),      intent(in)    :: id_dims       !< NetCDF dimension ids for this variable. Example: (/ id_edgedim /) for scalar data on edges, or (/ id_twodim, id_facedim /) for vector data on faces.
   integer(kind=c_int),              intent(in)    :: itype         !< The variable type expressed in one of the basic nf90_* types, e.g., nf90_double.
   integer(kind=c_int),              intent(in)    :: iloctype      !< Specifies at which unique mesh location data will be specified.
   character(kind=c_char),           intent(in)    :: c_var_name(MAXSTRLEN)      !< Name for the new data variable.
   character(kind=c_char),           intent(in)    :: c_standard_name(MAXSTRLEN) !< Standard name (CF-compliant) for 'standard_name' attribute in this variable.
   character(kind=c_char),           intent(in)    :: c_long_name(MAXSTRLEN)     !< Long name for 'long_name' attribute in this variable (use empty string if not wanted).
   character(kind=c_char),           intent(in)    :: c_unit(MAXSTRLEN)          !< Unit of this variable (CF-compliant) (use empty string for dimensionless quantities).
!   character(len=*),           intent(in)    :: cell_method   !< Cell method for the spatial dimension (i.e., for edge/face/volume), value should be one of 'point', 'mean', etc. (See CF) (empty string if not relevant).
!   character(len=*),           intent(in)    :: cell_measures !< Cell measures attribute, for example: 'area: mesh2d_cellarea', etc. (See CF) (empty string if not relevant).
!   type(t_crs),      optional, intent(in)    :: crs           !< (Optional) Add grid_mapping attribute based on this coordinate reference system for independent coordinates
   integer(kind=c_int),              intent(in)    :: ifill         !< Integer fill value (only applicable when itype==nf90_int).
   real(kind=c_double),              intent(in)    :: dfill         !< Double precision fill value (only applicable when itype==nf90_double).
   integer(kind=c_int)                             :: ierr          !< Result status (UG_NOERR==NF90_NOERR) if successful.

   character(len=MAXSTRLEN) :: var_name, standard_name, long_name, unit

   ! Store the names
   var_name      = char_array_to_string(c_var_name,      strlen(c_var_name))
   standard_name = char_array_to_string(c_standard_name, strlen(c_standard_name))
   long_name     = char_array_to_string(c_long_name,     strlen(c_long_name))
   unit          = char_array_to_string(c_unit,          strlen(c_unit))

   ierr = ionc_def_var(ioncid, meshid, networkid, id_var, itype, iloctype, var_name, standard_name, long_name, &
                    unit, "", "", ifill = ifill, dfill = dfill)
end function ionc_def_var_dll


!> Gets the numerical values for a named variable in the specified dataset on the specified mesh.
!! The location type allows to select the specific topological mesh location.
!! (UGRID-compliant, so UG_LOC_FACE/EDGE/NODE/ALL2D)
function ionc_get_var_1D_EightByteReal_dll(ioncid, meshid, iloctype, c_varname, c_values_ptr, nval, c_fillvalue) result(ierr) bind(C, name="ionc_get_var")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_var_1D_EightByteReal_dll
   integer,                intent(in)    :: ioncid    !< The IONC data set id.
   integer,                intent(in)    :: meshid    !< The mesh id in the specified data set.
   integer,                intent(in)    :: iloctype  !< The topological location on which to select data (UGRID-compliant, so UG_LOC_FACE/EDGE/NODE/ALL2D).
   character(kind=c_char), intent(in)    :: c_varname(MAXSTRLEN)   !< The name of the variable to be found. Should be without any "meshnd_" prefix.
   type(c_ptr),            intent(in)    :: c_values_ptr !< Pointer to an existing array for the values.
   integer,                intent(in)    :: nval      !< The number of values in the target array. TODO: AvD: remove this somehow, now only required to call c_f_pointer
   real(c_double),         intent(out)   :: c_fillvalue  !< Scalar for getting the fill value parameter for the requested variable.
   integer                               :: ierr      !< Result status, ionc_noerr if successful.

   character(len=MAXSTRLEN) :: varname
   real(kind=kind(1d0)), pointer :: values(:)

   ! Store the name
   varname = char_array_to_string(c_varname, strlen(c_varname))

   call c_f_pointer(c_values_ptr, values, (/ nval /))

   ierr = ionc_get_var_1D_EightByteReal(ioncid, meshid, iloctype, varname, values, c_fillvalue)

end function ionc_get_var_1D_EightByteReal_dll

!> Gets the charater values for a named variable in the specified dataset on the specified mesh.
function ionc_get_var_chars_dll(ioncid, meshid, c_varname, c_values_ptr, nval) result(ierr) bind(C, name="ionc_get_var_chars")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_var_chars_dll
   integer,                intent(in)    :: ioncid                  !< The IONC data set id.
   integer,                intent(in)    :: meshid                  !< The mesh id in the specified data set.
   character(kind=c_char), intent(in)    :: c_varname(MAXSTRLEN)    !< The name of the variable to be found. Should be without any "meshnd_" prefix.
   type(t_ug_charinfo),  intent(inout)   :: c_values_ptr(nval)      !< Pointer to the array of values
   character(len=ug_idsLen)              :: values(nval)            !< Temporary array to store the read values
   integer,                intent(in)    :: nval                    !< The number of values in the target array. TODO: AvD: remove this somehow, now only required to call c_f_pointer
   integer                               :: i, ierr                 !< Result status, ionc_noerr if successful.
   character(len=MAXSTRLEN)              :: varname


   ! Store the name
   varname = char_array_to_string(c_varname, strlen(c_varname))

   ierr = ionc_get_var_chars(ioncid, meshid, varname, values)

    do i=1,nval
        c_values_ptr(i)%id = values(i)
    end do

end function ionc_get_var_chars_dll

!> Puts the numerical values for a named variable into the specified dataset on the specified mesh.
!! NOTE: Assumes that the variable already exists in the file (i.e., needs no def_var anymore).
!! The location type allows to select the specific topological mesh location.
!! (UGRID-compliant, so UG_LOC_FACE/EDGE/NODE/ALL2D)
function ionc_put_var_1D_EightByteReal_dll(ioncid, meshid, iloctype, c_varname, c_values_ptr, nval) result(ierr) bind(C, name="ionc_put_var")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_put_var_1D_EightByteReal_dll
   integer,                intent(in)    :: ioncid    !< The IONC data set id.
   integer,                intent(in)    :: meshid    !< The mesh id in the specified data set.
   integer,                intent(in)    :: iloctype  !< The topological location on which to select data (UGRID-compliant, so UG_LOC_FACE/EDGE/NODE/ALL2D).
   character(kind=c_char), intent(in)    :: c_varname(MAXSTRLEN)   !< The name of the variable to be found. Should be without any "meshnd_" prefix.
   type(c_ptr),            intent(in)    :: c_values_ptr !< Pointer to array for the values.
   integer,                intent(in)    :: nval      !< The number of values in the target array. TODO: AvD: remove this somehow, now only required to call c_f_pointer
   integer                               :: ierr      !< Result status, ionc_noerr if successful.

   character(len=MAXSTRLEN) :: varname
   real(kind=kind(1d0)), pointer :: values(:)

   ! Store the name
   varname = char_array_to_string(c_varname, strlen(c_varname))

   call c_f_pointer(c_values_ptr, values, (/ nval /))

   ierr = ionc_put_var_1D_EightByteReal(ioncid, meshid, iloctype, varname, values)

end function ionc_put_var_1D_EightByteReal_dll

!> Puts the charaters values for a named variable into the specified dataset on the specified mesh.
function ionc_put_var_chars_dll(ioncid, meshid, c_varname, c_values_ptr, nval) result(ierr) bind(C, name="ionc_put_var_chars")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_put_var_chars_dll

   integer,                intent(in)    :: ioncid                  !< The IONC data set id.
   integer,                intent(in)    :: meshid                  !< The mesh id in the specified data set.
   character(kind=c_char), intent(in)    :: c_varname(MAXSTRLEN)    !< The name of the variable to be found. Should be without any "meshnd_" prefix.
   integer,                intent(in)    :: nval                    !< The number of values in the target array.
   type(t_ug_charinfo),    intent(in)    :: c_values_ptr(nval)      !< The array of character arrays, here passed using t_ug_charinfo type. In the future, we could use the field longnames to pass more charaters (e.g. the longnames/descriptions).
   character(len=ug_idsLen)              :: values(nval)            !< The array to write.
   character(len=MAXSTRLEN)              :: varname                 !< The variable name where the data are stored.
   integer                               :: i, ierr                 !< Result status, ionc_noerr if successful.

   !Decode the variable name
   varname = char_array_to_string(c_varname, strlen(c_varname))

   do i=1,nval
       values(i) = c_values_ptr(i)%id
   end do

   ierr = ionc_put_var_chars(ioncid, meshid, varname, values)

end function ionc_put_var_chars_dll

! TODO ******* DERIVED TYPE GIVEN BY C/C++/C#-PROGRAM
!> Add the global attributes to a NetCDF file
function ionc_add_global_attributes_dll(ioncid, meta) result(ierr)  bind(C, name="ionc_add_global_attributes")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_add_global_attributes_dll
   integer(kind=c_int),    intent(in)    :: ioncid      !< The IONC data set id.
   type (t_ug_meta),       intent(in)    :: meta
   integer(kind=c_int)                   :: ierr        !< Result status, ionc_noerr if successful.!
   character(len=ug_strLenMeta)          :: institution, source, references, version, modelname !< variables to be passed to io_netcdf for construction of the metadata structure

   institution = meta%institution
   source      = meta%source
   references  = meta%references
   version     = meta%version
   modelname   = meta%modelname

   ierr = ionc_add_global_attributes(ioncid, institution,source,references,version,modelname)

end function ionc_add_global_attributes_dll


! TODO ******* DERIVED TYPE GIVEN BY C/C++/C#-PROGRAM
!> Writes the complete mesh geometry
!function ionc_write_mesh_struct_dll(ioncid, meshids, meshgeom) result(ierr) bind(C, name="ionc_write_mesh_struct_dll")
!!DEC$ ATTRIBUTES DLLEXPORT :: ionc_write_mesh_struct_dll
!   integer(kind=c_int),             intent(in)    :: ioncid  !< The IONC data set id.
!   type(t_ug_meshids),  intent(inout) :: meshids !< Set of NetCDF-ids for all mesh geometry arrays.
!   type(t_ug_meshgeom), intent(in)    :: meshgeom !< The complete mesh geometry in a single struct.
!   integer(kind=c_int)                :: ierr    !< Result status, ionc_noerr if successful.!
!
!   ierr = ionc_write_mesh_struct(ioncid, meshids, meshgeom)
!end function ionc_write_mesh_struct_dll


!
! private routines/functions
!

! Utility functions, move these to interop module
! Make functions pure so they can be used as input arguments.
integer(c_int) pure function strlen(char_array)
 character(c_char), intent(in) :: char_array(MAXSTRLEN)
 integer :: i
 strlen = 0
 do i = 1, size(char_array)
    if (char_array(i) .eq. C_NULL_CHAR) then
       strlen = i-1
       exit
    end if
 end do
end function strlen

pure function char_array_to_string(char_array, length)
 integer(c_int), intent(in) :: length
 character(c_char),intent(in) :: char_array(length)
 character(len=length) :: char_array_to_string
 integer :: i
 do i = 1, length
    char_array_to_string(i:i) = char_array(i)
 enddo
end function char_array_to_string

function string_to_char_array(string, length) result(char_array)
   integer, intent(in) :: length
   character(len=length), intent(in) :: string
   character(kind=c_char,len=1) :: char_array(MAXSTRLEN)
   integer :: i
   do i = 1, len(string)
       char_array(i) = string(i:i)
   enddo
   char_array(len(string)+1) = C_NULL_CHAR
end function string_to_char_array

!
! Network and mesh1d functions
!

function ionc_create_1d_network_dll(ioncid, networkid, c_networkName, nNodes, nBranches, nGeometry) result(ierr) bind(C, name="ionc_create_1d_network")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_create_1d_network_dll
   integer(kind=c_int),    intent(in) :: ioncid
   character(kind=c_char), intent(in) :: c_networkName(MAXSTRLEN)
   integer(kind=c_int),    intent(in) :: nNodes,nBranches,nGeometry
   integer(kind=c_int), intent(inout) :: networkid
   integer                            :: ierr
   character(len=MAXSTRLEN)           :: networkName

   ! Store the name
   networkName = char_array_to_string(c_networkName, strlen(c_networkName))
   ierr = ionc_create_1d_network_ugrid(ioncid, networkid, networkName, nNodes, nBranches, nGeometry)

end function ionc_create_1d_network_dll

function ionc_write_1d_network_nodes_dll(ioncid,networkid, c_nodesX, c_nodesY, nodeinfo, nNodes) result(ierr) bind(C, name="ionc_write_1d_network_nodes")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_write_1d_network_nodes_dll

   integer(kind=c_int),     intent(in)    :: ioncid,networkid, nNodes
   type(c_ptr),             intent(in)    :: c_nodesX, c_nodesY
   type(t_ug_charinfo),     intent(in)    :: nodeinfo(nNodes)
   double precision, pointer              :: nodesX(:), nodesY(:)
   integer                                :: ierr, i
   character(len=ug_idsLen)               :: nodeids(nNodes)
   character(len=ug_idsLongNamesLen)      :: nodeLongnames(nNodes)

   call c_f_pointer(c_nodesX, nodesX, (/ nNodes /))
   call c_f_pointer(c_nodesY, nodesY, (/ nNodes /))
   do i=1,nNodes
       nodeids(i)       = nodeinfo(i)%id
       nodeLongnames(i) = nodeinfo(i)%longname
   end do

   ierr = ionc_write_1d_network_nodes_ugrid(ioncid, networkId, nodesX, nodesY, nodeids, nodeLongnames)

end function ionc_write_1d_network_nodes_dll

function ionc_put_1d_network_branches_dll(ioncid,networkid, c_sourcenodeid, c_targetnodeid, branchinfo, c_branchlengths, c_nbranchgeometrypoints, nBranches,startIndex) result(ierr) bind(C, name="ionc_put_1d_network_branches")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_put_1d_network_branches_dll

  integer(kind=c_int), intent(in)    :: ioncid, networkid, startIndex
  type(c_ptr),intent(in)             :: c_sourcenodeid, c_targetnodeid,c_nbranchgeometrypoints,c_branchlengths
  type(t_ug_charinfo),  intent(in)   :: branchinfo(nBranches)
  integer(kind=c_int), intent(in)    :: nBranches
  integer, pointer                   :: sourcenodeid(:), targetnodeid(:),nbranchgeometrypoints(:)
  double precision, pointer          :: branchlengths(:)
  character(len=ug_idsLen)           :: branchids(nBranches)
  character(len=ug_idsLongNamesLen)  :: branchlongnames(nBranches)
  integer ::ierr,i

  call c_f_pointer(c_sourcenodeid, sourcenodeid, (/ nBranches /))
  call c_f_pointer(c_targetnodeid, targetnodeid, (/ nBranches /))
  call c_f_pointer(c_nbranchgeometrypoints, nbranchgeometrypoints, (/ nBranches /))
  call c_f_pointer(c_branchlengths, branchlengths, (/ nBranches /))

  do i=1, nBranches
       branchids(i)       = branchinfo(i)%id
       branchlongnames(i) = branchinfo(i)%longname
  end do

  ierr = ionc_put_1d_network_branches_ugrid(ioncid, networkid, sourcenodeid, targetnodeid, branchids, branchlengths, branchlongnames, nbranchgeometrypoints, nBranches, startIndex)

end function ionc_put_1d_network_branches_dll

function ionc_write_1d_network_nodes_v1_dll(ioncid,networkid, c_nodesX, c_nodesY, c_ids, c_longnames, nNodes) result(ierr) bind(C, name="ionc_write_1d_network_nodes_v1")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_write_1d_network_nodes_v1_dll

   integer(kind=c_int),     intent(in)    :: ioncid,networkid, nNodes
   type(c_ptr),             intent(in)    :: c_nodesX, c_nodesY
   type(c_ptr),             intent(in)    :: c_ids, c_longnames
   double precision, pointer              :: nodesX(:), nodesY(:)
   integer                                :: ierr
   character(kind=c_char, len=ug_idsLen),pointer               :: ids(:)
   character(kind=c_char, len=ug_idsLongNamesLen),pointer      :: longnames(:)

   call c_f_pointer(c_nodesX, nodesX, (/ nNodes /))
   call c_f_pointer(c_nodesY, nodesY, (/ nNodes /))

   call c_f_pointer(c_ids, ids, (/ nNodes /))
   call c_f_pointer(c_longnames, longnames, (/ nNodes /))

   ierr = ionc_write_1d_network_nodes_ugrid(ioncid, networkId, nodesX, nodesY, ids, longnames)

end function ionc_write_1d_network_nodes_v1_dll

function ionc_put_1d_network_branches_v1_dll(ioncid,networkid, c_sourcenodeid, c_targetnodeid, c_ids, c_longnames, c_branchlengths, c_nbranchgeometrypoints, nBranches,startIndex) result(ierr) bind(C, name="ionc_put_1d_network_branches_v1")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_put_1d_network_branches_v1_dll

  integer(kind=c_int), intent(in)    :: ioncid, networkid, startIndex
  type(c_ptr), intent(in)            :: c_sourcenodeid, c_targetnodeid,c_nbranchgeometrypoints,c_branchlengths
  type(c_ptr), intent(in)            :: c_ids, c_longnames
  integer(kind=c_int), intent(in)    :: nBranches
  integer, pointer                   :: sourcenodeid(:), targetnodeid(:),nbranchgeometrypoints(:)
  double precision, pointer          :: branchlengths(:)
  character(kind=c_char, len=ug_idsLen),pointer               :: ids(:)
  character(kind=c_char, len=ug_idsLongNamesLen),pointer      :: longnames(:)
  integer ::ierr

  call c_f_pointer(c_sourcenodeid, sourcenodeid, (/ nBranches /))
  call c_f_pointer(c_targetnodeid, targetnodeid, (/ nBranches /))
  call c_f_pointer(c_nbranchgeometrypoints, nbranchgeometrypoints, (/ nBranches /))
  call c_f_pointer(c_branchlengths, branchlengths, (/ nBranches /))

  call c_f_pointer(c_ids, ids, (/ nBranches /))
  call c_f_pointer(c_longnames, longnames, (/ nBranches /))

  ierr = ionc_put_1d_network_branches_ugrid(ioncid, networkid, sourcenodeid, targetnodeid, ids, branchlengths, longnames, nbranchgeometrypoints, nBranches, startIndex)

end function ionc_put_1d_network_branches_v1_dll

!< write the branch order array, it might be temporary function
function ionc_put_1d_network_branchorder_dll(ioncid, networkid, c_branchorder, nbranches) result(ierr) bind(C, name="ionc_put_1d_network_branchorder")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_put_1d_network_branchorder_dll

    integer, intent(in)                :: ioncid, nbranches
    integer, intent(in)                :: networkId
    type(c_ptr), intent(in)            :: c_branchorder
    integer, pointer                   :: branchorder(:)
    integer                            :: ierr

    call c_f_pointer(c_branchorder, branchorder, (/ nbranches /))

    ierr =  ionc_put_1d_network_branchorder_ugrid(ioncid, networkid, branchorder)

end function ionc_put_1d_network_branchorder_dll

!< write the branch order array, it might be temporary function
function ionc_put_1d_network_branchtype_dll(ioncid, networkid, c_branchtype, nbranches) result(ierr) bind(C, name="ionc_put_1d_network_branchtype")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_put_1d_network_branchtype_dll

    integer, intent(in)                :: ioncid, nbranches
    integer, intent(in)                :: networkId
    type(c_ptr), intent(in)            :: c_branchtype
    integer, pointer                   :: branchtype(:)
    integer                            :: ierr

    call c_f_pointer(c_branchtype, branchtype, (/ nbranches /))

    ierr =  ionc_put_1d_network_branchtype_ugrid(ioncid, networkid, branchtype)

end function ionc_put_1d_network_branchtype_dll


function ionc_write_1d_network_branches_geometry_dll(ioncid, networkid, c_geopointsX, c_geopointsY, nGeometry) result(ierr) bind(C, name="ionc_write_1d_network_branches_geometry")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_write_1d_network_branches_geometry_dll

  integer(kind=c_int), intent(in)    :: ioncid, networkid
  type(c_ptr), intent(in)            :: c_geopointsX, c_geopointsY
  double precision, pointer          :: geopointsX(:), geopointsY(:)
  integer(kind=c_int), intent(in)    :: nGeometry
  integer ::ierr

  call c_f_pointer(c_geopointsX, geopointsX, (/ nGeometry /))
  call c_f_pointer(c_geopointsY, geopointsY, (/ nGeometry /))

  ierr = ionc_write_1d_network_branches_geometry_ugrid(ioncid, networkid, geopointsX, geopointsY)

end function ionc_write_1d_network_branches_geometry_dll


function ionc_get_1d_network_nodes_count_dll(ioncid, networkid, nNodes) result(ierr) bind(C, name="ionc_get_1d_network_nodes_count")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_1d_network_nodes_count_dll

  integer(kind=c_int), intent(in)    :: ioncid, networkid
  integer(kind=c_int), intent(out)   :: nNodes
  integer                            :: ierr

  ierr = ionc_get_1d_network_nodes_count_ugrid(ioncid, networkid, nNodes)

end function ionc_get_1d_network_nodes_count_dll

function ionc_get_1d_network_branches_count_dll(ioncid, networkid, nBranches) result(ierr) bind(C, name="ionc_get_1d_network_branches_count")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_1d_network_branches_count_dll

  integer(kind=c_int), intent(in)    :: ioncid, networkid
  integer(kind=c_int), intent(out)   :: nBranches
  integer :: ierr

  ierr = ionc_get_1d_network_branches_count_ugrid(ioncid, networkid, nBranches)

end function ionc_get_1d_network_branches_count_dll

function ionc_get_1d_network_branches_geometry_coordinate_count_dll(ioncid, networkid, ngeometrypoints) result(ierr) bind(C, name="ionc_get_1d_network_branches_geometry_coordinate_count")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_1d_network_branches_geometry_coordinate_count_dll

  integer(kind=c_int), intent(in)    :: ioncid, networkid
  integer(kind=c_int), intent(out) :: ngeometrypoints
  integer :: ierr

  ierr = ionc_get_1d_network_branches_geometry_coordinate_count_ugrid(ioncid, networkid, ngeometrypoints)

end function ionc_get_1d_network_branches_geometry_coordinate_count_dll

function ionc_read_1d_network_nodes_dll(ioncid, networkid, c_nodesX, c_nodesY, nodeinfo, nNodes) result(ierr) bind(C, name="ionc_read_1d_network_nodes")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_read_1d_network_nodes_dll

  integer(kind=c_int), intent(in)       :: ioncid, networkid, nNodes !< The dataset where i do want to create the dataset.
  type(t_ug_charinfo), intent(inout)    :: nodeinfo(nNodes)
  type(c_ptr),         intent(inout)    :: c_nodesX, c_nodesY
  double precision,    pointer          :: nodesX(:),  nodesY(:)
  character(len=ug_idsLen)              :: nodeids(nNodes)
  character(len=ug_idsLongNamesLen)     :: nodelongnames(nNodes)
  integer                               :: ierr,i

  call c_f_pointer(c_nodesX, nodesX, (/ nNodes /))
  call c_f_pointer(c_nodesY, nodesY, (/ nNodes /))

  ierr = ionc_read_1d_network_nodes_ugrid(ioncid, networkid, nodesX, nodesY, nodeids, nodelongnames)

  do i=1,nNodes
       nodeinfo(i)%id = nodeids(i)
       nodeinfo(i)%longname = nodelongnames(i)
  end do

end function ionc_read_1d_network_nodes_dll

function ionc_get_1d_network_branches_dll(ioncid, networkid, c_sourcenodeid, c_targetnodeid, c_branchlengths, branchinfo, c_nbranchgeometrypoints, nBranches, startIndex)  result(ierr) bind(C, name="ionc_get_1d_network_branches")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_1d_network_branches_dll

  integer(kind=c_int), intent(in)       :: ioncid, networkid, startIndex  !< The dataset where i do want to create the dataset.
  type(c_ptr),intent(inout)             :: c_sourcenodeid, c_targetnodeid, c_nbranchgeometrypoints,c_branchlengths
  type(t_ug_charinfo),  intent(inout)   :: branchinfo(nBranches)
  integer,pointer                       :: sourcenodeid(:), targetnodeid(:),nbranchgeometrypoints(:)
  character(len=ug_idsLen)              :: branchids(nBranches)
  character(len=ug_idsLongNamesLen)     :: branchlongnames(nBranches)
  double precision ,pointer             :: branchlengths(:)
  integer                               :: ierr, i, nBranches

  call c_f_pointer(c_sourcenodeid, sourcenodeid, (/ nBranches /))
  call c_f_pointer(c_targetnodeid, targetnodeid, (/ nBranches /))
  call c_f_pointer(c_nbranchgeometrypoints, nbranchgeometrypoints, (/ nBranches /))
  call c_f_pointer(c_branchlengths, branchlengths, (/ nBranches /))

  ierr = ionc_get_1d_network_branches_ugrid(ioncid, networkid, sourcenodeid, targetnodeid, branchids, branchlengths, branchlongnames, nbranchgeometrypoints, startIndex)

  do i=1,nBranches
    branchinfo(i)%id = branchids(i)
    branchinfo(i)%longname = branchlongnames(i)
  end do

end function ionc_get_1d_network_branches_dll

function ionc_read_1d_network_nodes_v1_dll(ioncid, networkid, c_nodesX, c_nodesY, c_ids, c_longnames,  nNodes) result(ierr) bind(C, name="ionc_read_1d_network_nodes_v1")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_read_1d_network_nodes_v1_dll

  integer(kind=c_int), intent(in)       :: ioncid, networkid, nNodes !< The dataset where i do want to create the dataset.
  type(c_ptr),         intent(inout)    :: c_ids, c_longnames
  type(c_ptr),         intent(inout)    :: c_nodesX, c_nodesY
  double precision,    pointer          :: nodesX(:), nodesY(:)
  character(kind=c_char, len=ug_idsLen),pointer               :: ids(:)
  character(kind=c_char, len=ug_idsLongNamesLen),pointer      :: longnames(:)

  integer                               :: ierr

  call c_f_pointer(c_nodesX, nodesX, (/ nNodes /))
  call c_f_pointer(c_nodesY, nodesY, (/ nNodes /))
  call c_f_pointer(c_ids, ids, (/ nNodes /))
  call c_f_pointer(c_longnames, longnames, (/ nNodes /))

  ierr = ionc_read_1d_network_nodes_ugrid(ioncid, networkid, nodesX, nodesY, ids, longnames)

end function ionc_read_1d_network_nodes_v1_dll

function ionc_get_1d_network_branches_v1_dll(ioncid, networkid, c_sourcenodeid, c_targetnodeid, c_branchlengths, c_ids, c_longnames, c_nbranchgeometrypoints, nBranches, startIndex)  result(ierr) bind(C, name="ionc_get_1d_network_branches_v1")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_1d_network_branches_v1_dll

  integer(kind=c_int), intent(in)       :: ioncid, networkid, startIndex  !< The dataset where i do want to create the dataset.
  type(c_ptr),intent(inout)             :: c_sourcenodeid, c_targetnodeid, c_nbranchgeometrypoints,c_branchlengths
  type(c_ptr),  intent(inout)           :: c_ids, c_longnames
  integer,pointer                       :: sourcenodeid(:), targetnodeid(:),nbranchgeometrypoints(:)
  double precision ,pointer             :: branchlengths(:)
  integer                               :: ierr, nBranches
  character(kind=c_char, len=ug_idsLen),pointer               :: ids(:)
  character(kind=c_char, len=ug_idsLongNamesLen),pointer      :: longnames(:)


  call c_f_pointer(c_sourcenodeid, sourcenodeid, (/ nBranches /))
  call c_f_pointer(c_targetnodeid, targetnodeid, (/ nBranches /))
  call c_f_pointer(c_nbranchgeometrypoints, nbranchgeometrypoints, (/ nBranches /))
  call c_f_pointer(c_branchlengths, branchlengths, (/ nBranches /))

  call c_f_pointer(c_ids, ids, (/ nBranches /))
  call c_f_pointer(c_longnames, longnames, (/ nBranches /))

  ierr = ionc_get_1d_network_branches_ugrid(ioncid, networkid, sourcenodeid, targetnodeid, ids, branchlengths, longnames, nbranchgeometrypoints, startIndex)

end function ionc_get_1d_network_branches_v1_dll

!< get the branch order array, it might be temporary function
function ionc_get_1d_network_branchorder_dll(ioncid, networkid, c_branchorder, nbranches) result(ierr) bind(C, name="ionc_get_1d_network_branchorder")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_1d_network_branchorder_dll

   integer, intent(in)                :: ioncid, nbranches
   integer, intent(in)                :: networkid
   type(c_ptr), intent(inout)         :: c_branchorder
   integer,pointer                    :: branchorder(:)
   integer                            :: ierr

   call c_f_pointer(c_branchorder, branchorder, (/ nbranches /))

   ierr = ionc_get_1d_network_branchorder_ugrid(ioncid, networkid, branchorder)

end function ionc_get_1d_network_branchorder_dll

!< get the branch order array, it might be temporary function
function ionc_get_1d_network_branchtype_dll(ioncid, networkid, c_branchtype, nbranches) result(ierr) bind(C, name="ionc_get_1d_network_branchtype")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_1d_network_branchtype_dll

   integer, intent(in)                :: ioncid, nbranches
   integer, intent(in)                :: networkid
   type(c_ptr), intent(inout)         :: c_branchtype
   integer,pointer                    :: branchtype(:)
   integer                            :: ierr

   call c_f_pointer(c_branchtype, branchtype, (/ nbranches /))

   ierr = ionc_get_1d_network_branchtype_ugrid(ioncid, networkid, branchtype)

end function ionc_get_1d_network_branchtype_dll


function ionc_read_1d_network_branches_geometry_dll(ioncid, networkid, c_geopointsX, c_geopointsY, ngeometrypoints) result(ierr) bind(C, name="ionc_read_1d_network_branches_geometry")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_read_1d_network_branches_geometry_dll
  integer(kind=c_int), intent(in)    :: ioncid, networkid
  type(c_ptr), intent(inout)         :: c_geopointsX, c_geopointsY
  double precision,pointer           :: geopointsX(:),geopointsY(:)
  integer                            :: ierr, ngeometrypoints

  call c_f_pointer(c_geopointsX, geopointsX, (/ ngeometrypoints /))
  call c_f_pointer(c_geopointsY, geopointsY, (/ ngeometrypoints /))

  ierr = ionc_read_1d_network_branches_geometry_ugrid(ioncid, networkid, geopointsX, geopointsY)

end function ionc_read_1d_network_branches_geometry_dll

function ionc_create_1d_mesh_dll(ioncid, c_networkname, meshid, c_meshname, nmeshpoints) result(ierr) bind(C, name="ionc_create_1d_mesh")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_create_1d_mesh_dll
  integer(kind=c_int)   , intent(in) :: ioncid, nmeshpoints
  integer(kind=c_int)   , intent(out) ::meshid
  character(kind=c_char), intent(in) :: c_networkname(MAXSTRLEN), c_meshname(MAXSTRLEN)
  integer                            :: ierr
  character(len=MAXSTRLEN)           :: meshname, networkname

  ! Store the name
  meshname = char_array_to_string(c_meshName, strlen(c_meshname))
  networkname = char_array_to_string(c_networkname, strlen(c_networkname))

  ierr = ionc_create_1d_mesh_ugrid(ioncid, networkname, meshid, meshname, nmeshpoints)

  ! Define attributes and variables to store the mesh ids, UG_LOC_NODE = 1
  ierr = ionc_def_mesh_ids_ugrid(ioncid, meshid, 1)

end function ionc_create_1d_mesh_dll

function ionc_create_1d_mesh_v1_dll(ioncid, c_networkname, meshid, c_meshname, nmeshpoints, nmeshedges, writexy) result(ierr) bind(C, name="ionc_create_1d_mesh_v1")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_create_1d_mesh_v1_dll
  integer(kind=c_int),    intent(in)  :: ioncid, nmeshpoints, nmeshedges, writexy
  integer(kind=c_int),    intent(out) :: meshid
  character(kind=c_char), intent(in)  :: c_networkname(MAXSTRLEN), c_meshname(MAXSTRLEN)
  integer                             :: ierr
  character(len=MAXSTRLEN)            :: meshname, networkname

  ! Store the name
  meshname = char_array_to_string(c_meshName, strlen(c_meshname))
  networkname = char_array_to_string(c_networkname, strlen(c_networkname))

  ierr = ionc_create_1d_mesh_ugrid_v1(ioncid, networkname, meshid, meshname, nmeshpoints, nmeshedges, writexy)

  ! Define attributes and variables to store the mesh ids, UG_LOC_NODE = 1
  ierr = ionc_def_mesh_ids_ugrid(ioncid, meshid, 1)

end function ionc_create_1d_mesh_v1_dll

function ionc_def_mesh_ids_dll(ioncid, meshid, locationType) result(ierr) bind(C, name="ionc_def_mesh_ids")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_def_mesh_ids_dll
  integer(kind=c_int)   , intent(in)    ::  ioncid, meshid, locationType
  integer                               ::  ierr

  ierr = ionc_def_mesh_ids_ugrid(ioncid, meshid, locationType)

end function ionc_def_mesh_ids_dll

function ionc_put_1d_mesh_discretisation_points_dll(ioncid, meshid, c_branchidx, c_offset, nodesinfo, nmeshpoints, startIndex) result(ierr) bind(C, name="ionc_put_1d_mesh_discretisation_points")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_put_1d_mesh_discretisation_points_dll
  integer(kind=c_int), intent(in)     :: ioncid, meshid, nmeshpoints, startIndex
  type(c_ptr), intent(in)             :: c_branchidx,c_offset
  type(t_ug_charinfo),  intent(in)    :: nodesinfo(nmeshpoints)
  integer,pointer                     :: branchidx(:)
  double precision,pointer            :: offset(:)
  character(len=ug_idsLen)            :: nodeids(nmeshpoints)
  character(len=ug_idsLongNamesLen)   :: nodelongnames(nmeshpoints)
  character(len=8)                    :: varnameids
  character(len=15)                   :: varnamelongnames
  integer                             :: ierr,i

  call c_f_pointer(c_branchidx, branchidx, (/ nmeshpoints /))
  call c_f_pointer(c_offset, offset, (/ nmeshpoints /))

  ierr = ionc_put_1d_mesh_discretisation_points_ugrid(ioncid, meshid, branchidx, offset, startIndex)

  !get and write the node_ids
  do i=1,nmeshpoints
        nodeids(i)       = nodesinfo(i)%id
        nodelongnames(i) = nodesinfo(i)%longname
  end do

  !these are hard-coded variable names for node ids of the network
  varnameids        = 'node_id'
  ierr              = ionc_put_var_chars(ioncid, meshid, varnameids, nodeids)
  varnamelongnames  = 'node_long_name'
  ierr              = ionc_put_var_chars(ioncid, meshid, varnamelongnames, nodelongnames)

end function ionc_put_1d_mesh_discretisation_points_dll

function ionc_put_1d_mesh_discretisation_points_v1_dll(ioncid, meshid, c_branchidx, c_offset, nodesinfo, nmeshpoints, startIndex, c_coordx, c_coordy) result(ierr) bind(C, name="ionc_put_1d_mesh_discretisation_points_v1")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_put_1d_mesh_discretisation_points_v1_dll
  integer(kind=c_int), intent(in)     :: ioncid, meshid, nmeshpoints, startIndex
  type(c_ptr), intent(in)             :: c_branchidx, c_offset, c_coordx, c_coordy
  type(t_ug_charinfo),  intent(in)    :: nodesinfo(nmeshpoints)
  integer,pointer                     :: branchidx(:)
  double precision,pointer            :: offset(:)
  double precision,pointer            :: coordx(:)
  double precision,pointer            :: coordy(:)
  character(len=ug_idsLen)            :: nodeids(nmeshpoints)
  character(len=ug_idsLongNamesLen)   :: nodelongnames(nmeshpoints)
  character(len=8)                    :: varnameids
  character(len=15)                   :: varnamelongnames
  integer                             :: ierr,i

  call c_f_pointer(c_branchidx, branchidx, (/ nmeshpoints /))
  call c_f_pointer(c_offset, offset, (/ nmeshpoints /))
  call c_f_pointer(c_coordx, coordx, (/ nmeshpoints /))
  call c_f_pointer(c_coordy, coordy, (/ nmeshpoints /))

  ierr = ionc_put_1d_mesh_discretisation_points_ugrid_v1(ioncid, meshid, branchidx, offset, startIndex, coordx, coordy)

  !get and write the node_ids
  do i=1,nmeshpoints
        nodeids(i)       = nodesinfo(i)%id
        nodelongnames(i) = nodesinfo(i)%longname
  end do

  !these are hard-coded variable names for node ids of the network
  varnameids        = 'node_id'
  ierr              = ionc_put_var_chars(ioncid, meshid, varnameids, nodeids)
  varnamelongnames  = 'node_long_name'
  ierr              = ionc_put_var_chars(ioncid, meshid, varnamelongnames, nodelongnames)

end function ionc_put_1d_mesh_discretisation_points_v1_dll

function ionc_put_1d_mesh_discretisation_points_v2_dll(ioncid, meshid, c_branchidx, c_offset, c_ids, c_longnames, nmeshpoints, startIndex, c_coordx, c_coordy) result(ierr) bind(C, name="ionc_put_1d_mesh_discretisation_points_v2")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_put_1d_mesh_discretisation_points_v2_dll
  integer(kind=c_int), intent(in)     :: ioncid, meshid, nmeshpoints, startIndex
  type(c_ptr), intent(in)             :: c_branchidx, c_offset, c_coordx, c_coordy
  type(c_ptr), intent(in)             :: c_ids, c_longnames
  integer,pointer                     :: branchidx(:)

  double precision,pointer            :: offset(:)
  double precision,pointer            :: coordx(:)
  double precision,pointer            :: coordy(:)
  character(len=8)                    :: varnameids
  character(len=15)                   :: varnamelongnames
  integer                             :: ierr
  character(kind=c_char, len=ug_idsLen),pointer               :: ids(:)
  character(kind=c_char, len=ug_idsLongNamesLen),pointer      :: longnames(:)


  call c_f_pointer(c_branchidx, branchidx, (/ nmeshpoints /))
  call c_f_pointer(c_offset, offset, (/ nmeshpoints /))
  call c_f_pointer(c_coordx, coordx, (/ nmeshpoints /))
  call c_f_pointer(c_coordy, coordy, (/ nmeshpoints /))

  ierr = ionc_put_1d_mesh_discretisation_points_ugrid_v1(ioncid, meshid, branchidx, offset, startIndex, coordx, coordy)

  call c_f_pointer(c_ids, ids, (/ nmeshpoints /))
  call c_f_pointer(c_longnames, longnames, (/ nmeshpoints /))

  !these are hard-coded variable names for node ids of the network
  varnameids        = 'node_id'
  ierr              = ionc_put_var_chars(ioncid, meshid, varnameids, ids)
  varnamelongnames  = 'node_long_name'
  ierr              = ionc_put_var_chars(ioncid, meshid, varnamelongnames, longnames)

end function ionc_put_1d_mesh_discretisation_points_v2_dll

function ionc_put_1d_mesh_edges_dll(ioncid, meshid, c_edgebranchidx, c_edgeoffset, nmeshedges, startIndex, c_coordx, c_coordy) result(ierr) bind(C, name="ionc_put_1d_mesh_edges")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_put_1d_mesh_edges_dll
  integer(kind=c_int), intent(in)     :: ioncid, meshid, nmeshedges, startIndex
  type(c_ptr), intent(in)             :: c_edgebranchidx, c_edgeoffset, c_coordx, c_coordy
  integer                             :: ierr
  integer,pointer                     :: edgebranchidx(:)
  double precision,pointer            :: edgeoffset(:)
  double precision,pointer            :: coordx(:)
  double precision,pointer            :: coordy(:)

  call c_f_pointer(c_edgebranchidx, edgebranchidx, (/ nmeshedges /))
  call c_f_pointer(c_edgeoffset, edgeoffset, (/ nmeshedges /))
  call c_f_pointer(c_coordx, coordx, (/ nmeshedges /))
  call c_f_pointer(c_coordy, coordy, (/ nmeshedges /))

  ierr = ionc_put_1d_mesh_edges(ioncid, meshid, edgebranchidx, edgeoffset, startIndex, coordx, coordy)

end function ionc_put_1d_mesh_edges_dll

function ionc_get_1d_mesh_discretisation_points_count_dll(ioncid, meshid, nmeshpoints) result(ierr) bind(C, name="ionc_get_1d_mesh_discretisation_points_count")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_1d_mesh_discretisation_points_count_dll
  integer(kind=c_int), intent(in)    :: ioncid, meshid
  integer(kind=c_int), intent(inout) :: nmeshpoints
  integer ::ierr

  ierr = ionc_get_1d_mesh_discretisation_points_count_ugrid(ioncid, meshid, nmeshpoints)

end function ionc_get_1d_mesh_discretisation_points_count_dll


function ionc_get_1d_mesh_discretisation_points_dll(ioncid, meshid, c_branchidx, c_offset, nodesinfo, nmeshpoints, startIndex) result(ierr) bind(C, name="ionc_get_1d_mesh_discretisation_points")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_1d_mesh_discretisation_points_dll
  integer(kind=c_int), intent(in)   :: ioncid, meshid, nmeshpoints,startIndex
  type(c_ptr), intent(inout)        :: c_branchidx, c_offset
  type(t_ug_charinfo),  intent(inout)  :: nodesinfo(nmeshpoints)
  character(len=ug_idsLen)          :: nodeids(nmeshpoints)
  character(len=ug_idsLongNamesLen) :: nodelongnames(nmeshpoints)
  double precision,pointer          :: offset(:)
  integer,pointer                   :: branchidx(:)
  character(len=8)                  :: varnameids
  character(len=15)                 :: varnamelongnames
  integer                           :: ierr,i

  call c_f_pointer(c_branchidx, branchidx, (/ nmeshpoints /))
  call c_f_pointer(c_offset, offset, (/ nmeshpoints /))

  ierr = ionc_get_1d_mesh_discretisation_points_ugrid(ioncid, meshid, branchidx, offset, startIndex)

  !The names of the variables are hard-coded
  varnameids        = 'node_id'
  ierr              = ionc_get_var_chars(ioncid, meshid, varnameids, nodeids)
  if (ierr /= IONC_NOERR) then
     ! Backwards compatible read of Deltares-0.9 plural-names.
     varnameids     = 'node_ids'
     ierr           = ionc_get_var_chars(ioncid, meshid, varnameids, nodeids)
  end if
  varnamelongnames  = 'node_long_name'
  ierr              = ionc_get_var_chars(ioncid, meshid, varnamelongnames, nodelongnames)
  if (ierr /= IONC_NOERR) then
     ! Backwards compatible read of Deltares-0.9 plural-names.
     varnamelongnames  = 'node_long_names'
     ierr              = ionc_get_var_chars(ioncid, meshid, varnamelongnames, nodelongnames)
  end if

  do i=1,nmeshpoints
     nodesinfo(i)%id       = nodeids(i)
     nodesinfo(i)%longname = nodelongnames(i)
  end do

end function ionc_get_1d_mesh_discretisation_points_dll

function ionc_get_1d_mesh_discretisation_points_v1_dll(ioncid, meshid, c_branchidx, c_offset, nodesinfo, nmeshpoints, startIndex, c_coordx, c_coordy) result(ierr) bind(C, name="ionc_get_1d_mesh_discretisation_points_v1")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_1d_mesh_discretisation_points_v1_dll
  integer(kind=c_int), intent(in)   :: ioncid, meshid, nmeshpoints,startIndex
  type(c_ptr), intent(inout)        :: c_branchidx, c_offset, c_coordx, c_coordy
  type(t_ug_charinfo),  intent(inout)  :: nodesinfo(nmeshpoints)
  character(len=ug_idsLen)          :: nodeids(nmeshpoints)
  character(len=ug_idsLongNamesLen) :: nodelongnames(nmeshpoints)
  double precision,pointer          :: offset(:)
  integer,pointer                   :: branchidx(:)
  double precision,pointer          :: coordx(:), coordy(:)
  integer,parameter                 :: size_of_varname = 2
  character(len=MAXSTRLEN)          :: varnameids(size_of_varname)
  character(len=MAXSTRLEN)          :: varnamelongnames(size_of_varname)
  integer                           :: ierr,i

  call c_f_pointer(c_branchidx, branchidx, (/ nmeshpoints /))
  call c_f_pointer(c_offset, offset, (/ nmeshpoints /))
  call c_f_pointer(c_coordx, coordx, (/ nmeshpoints /))
  call c_f_pointer(c_coordy, coordy, (/ nmeshpoints /))

  ierr = ionc_get_1d_mesh_discretisation_points_ugrid_v1(ioncid, meshid, branchidx, offset, startIndex, coordx, coordy )

  !The names of the variables are hard-coded

  varnameids(1) = 'node_id'
  varnameids(2) = 'node_ids'

  do i=1,size_of_varname
     ierr = ionc_get_var_chars(ioncid, meshid, varnameids(i), nodeids)
     if (ierr==0) then
          exit
     endif
  enddo

  varnamelongnames(1) = 'node_long_name'
  varnamelongnames(2) = 'node_long_names'

  do i=1,size_of_varname
     ierr = ionc_get_var_chars(ioncid, meshid, varnamelongnames(i), nodelongnames)
     if (ierr == 0) then
          exit
     endif
  enddo

  do i=1,nmeshpoints
     nodesinfo(i)%id       = nodeids(i)
     nodesinfo(i)%longname = nodelongnames(i)
  end do

end function ionc_get_1d_mesh_discretisation_points_v1_dll

function ionc_get_1d_mesh_discretisation_points_v2_dll(ioncid, meshid, c_branchidx, c_offset, c_ids, c_longNames, nmeshpoints, startIndex, c_coordx, c_coordy) result(ierr) bind(C, name="ionc_get_1d_mesh_discretisation_points_v2")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_1d_mesh_discretisation_points_v2_dll
  integer(kind=c_int), intent(in)   :: ioncid, meshid, nmeshpoints,startIndex
  type(c_ptr), intent(inout)        :: c_branchidx, c_offset, c_coordx, c_coordy
  type(c_ptr),  intent(inout)       :: c_ids, c_longNames
  double precision,pointer          :: offset(:)
  integer,pointer                   :: branchidx(:)
  double precision,pointer          :: coordx(:), coordy(:)
  integer,parameter                 :: size_of_varname = 2
  character(len=MAXSTRLEN)          :: varnameids(size_of_varname)
  character(len=MAXSTRLEN)          :: varnamelongnames(size_of_varname)
  integer                           :: ierr,i
  character(kind=c_char, len=ug_idsLen),pointer               :: ids(:)
  character(kind=c_char, len=ug_idsLongNamesLen),pointer      :: longnames(:)

  call c_f_pointer(c_branchidx, branchidx, (/ nmeshpoints /))
  call c_f_pointer(c_offset, offset, (/ nmeshpoints /))
  call c_f_pointer(c_coordx, coordx, (/ nmeshpoints /))
  call c_f_pointer(c_coordy, coordy, (/ nmeshpoints /))
  call c_f_pointer(c_ids, ids, (/ nmeshpoints /))
  call c_f_pointer(c_longnames, longnames, (/ nmeshpoints /))

  ierr = ionc_get_1d_mesh_discretisation_points_ugrid_v1(ioncid, meshid, branchidx, offset, startIndex, coordx, coordy )

  !The names of the variables are hard-coded

  varnameids(1) = 'node_id'
  varnameids(2) = 'node_ids'

  do i=1,size_of_varname
     ierr = ionc_get_var_chars(ioncid, meshid, varnameids(i), ids)
     if (ierr==0) then
          exit
     endif
  enddo

  varnamelongnames(1) = 'node_long_name'
  varnamelongnames(2) = 'node_long_names'

  do i=1,size_of_varname
     ierr = ionc_get_var_chars(ioncid, meshid, varnamelongnames(i), longnames)
     if (ierr == 0) then
          exit
     endif
  enddo

end function ionc_get_1d_mesh_discretisation_points_v2_dll

function ionc_get_1d_mesh_edges_dll(ioncid, meshid, c_edgebranchidx, c_edgeoffset, nmeshedges, startIndex, c_coordx, c_coordy) result(ierr) bind(C, name="ionc_get_1d_mesh_edges")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_1d_mesh_edges_dll
  integer(kind=c_int), intent(in)     :: ioncid, meshid, nmeshedges, startIndex
  type(c_ptr), intent(inout)          :: c_edgebranchidx, c_edgeoffset, c_coordx, c_coordy
  integer                             :: ierr
  integer,pointer                     :: edgebranchidx(:)
  double precision,pointer            :: edgeoffset(:)
  double precision,pointer            :: coordx(:)
  double precision,pointer            :: coordy(:)

  call c_f_pointer(c_edgebranchidx, edgebranchidx, (/ nmeshedges /))
  call c_f_pointer(c_edgeoffset, edgeoffset, (/ nmeshedges /))
  call c_f_pointer(c_coordx, coordx, (/ nmeshedges /))
  call c_f_pointer(c_coordy, coordy, (/ nmeshedges /))

  ierr = ionc_get_1d_mesh_edges(ioncid, meshid, edgebranchidx, edgeoffset, startIndex, coordx, coordy)

end function ionc_get_1d_mesh_edges_dll

!
! mesh links
!

function ionc_def_mesh_contact_dll(ioncid, contactsmesh, c_contactmeshname, ncontacts, idmesh1, idmesh2, locationType1Id, locationType2Id) result(ierr) bind(C, name="ionc_def_mesh_contact")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_def_mesh_contact_dll

   integer, intent(in)                :: ioncid, ncontacts, idmesh1, idmesh2,locationType1Id,locationType2Id
   character(kind=c_char), intent(in) :: c_contactmeshname(MAXSTRLEN)
   character(len=MAXSTRLEN)           :: contactmeshname
   integer, intent(inout)             :: contactsmesh
   integer                            :: ierr

   ! Store the name
   contactmeshname = char_array_to_string(c_contactmeshname, strlen(c_contactmeshname))

   ierr = ionc_def_mesh_contact_ugrid(ioncid, contactsmesh, contactmeshname, ncontacts, idmesh1, idmesh2, locationType1Id, locationType2Id)

end function ionc_def_mesh_contact_dll

!> Get the number of contact links in a specified meshcontact set.
function ionc_get_contacts_count_dll(ioncid, contactid, ncontacts) result(ierr) bind(C, name="ionc_get_contacts_count")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_contacts_count_dll
   integer, intent(in   )             :: ioncid     !< The IONC data set id.
   integer, intent(in   )             :: contactid  !< The contact id in the specified data set.
   integer, intent(inout)             :: ncontacts  !< Number of contact links in the specified meshcontact set.
   integer                            :: ierr       !< Result status (IONC_NOERR if successful).

   ierr = ionc_get_contacts_count_ugrid(ioncid, contactid, ncontacts)

end function ionc_get_contacts_count_dll

function ionc_put_mesh_contact_dll(ioncid, contactsmesh, c_mesh1indexes, c_mesh2indexes, c_contacttype, contactsinfo, ncontacts, startIndex) result(ierr) bind(C, name="ionc_put_mesh_contact")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_put_mesh_contact_dll
   integer, intent(in)                   :: ioncid, contactsmesh, ncontacts, startIndex
   type(c_ptr), intent(in)               :: c_mesh1indexes, c_mesh2indexes, c_contacttype
   type(t_ug_charinfo),  intent(in)      :: contactsinfo(ncontacts)
   integer,pointer                       :: mesh1indexes(:), mesh2indexes(:), contacttype(:)
   character(len=ug_idsLen)              :: contactsids(ncontacts)
   character(len=ug_idsLongNamesLen)     :: contactslongnames(ncontacts)
   integer                               :: ierr,i

   call c_f_pointer(c_mesh1indexes, mesh1indexes, (/ ncontacts /))
   call c_f_pointer(c_mesh2indexes, mesh2indexes, (/ ncontacts /))
   call c_f_pointer(c_contacttype, contacttype, (/ ncontacts /))

   do i=1,ncontacts
      contactsids(i) = contactsinfo(i)%id
      contactslongnames(i) = contactsinfo(i)%longname
   end do

   ierr = ionc_put_mesh_contact_ugrid(ioncid, contactsmesh, mesh1indexes, mesh2indexes, contactsids, contactslongnames, contacttype, startIndex)

end function ionc_put_mesh_contact_dll

function ionc_put_mesh_contact_v1_dll(ioncid, contactsmesh, c_mesh1indexes, c_mesh2indexes, c_contacttype, c_ids, c_longnames,  ncontacts, startIndex) result(ierr) bind(C, name="ionc_put_mesh_contact_v1")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_put_mesh_contact_v1_dll
   integer, intent(in)                                         :: ioncid, contactsmesh, ncontacts, startIndex
   type(c_ptr), intent(in)                                     :: c_mesh1indexes, c_mesh2indexes, c_contacttype
   type(c_ptr),  intent(in)                                    :: c_ids, c_longnames
   integer,pointer                                             :: mesh1indexes(:), mesh2indexes(:), contacttype(:)
   integer                                                     :: ierr
   character(kind=c_char, len=ug_idsLen),pointer               :: ids(:)
   character(kind=c_char, len=ug_idsLongNamesLen),pointer      :: longnames(:)

   call c_f_pointer(c_mesh1indexes, mesh1indexes, (/ ncontacts /))
   call c_f_pointer(c_mesh2indexes, mesh2indexes, (/ ncontacts /))
   call c_f_pointer(c_contacttype, contacttype, (/ ncontacts /))

   call c_f_pointer(c_ids, ids, (/ ncontacts /))
   call c_f_pointer(c_longnames, longnames, (/ ncontacts /))

   ierr = ionc_put_mesh_contact_ugrid(ioncid, contactsmesh, mesh1indexes, mesh2indexes, ids, longnames, contacttype, startIndex)

end function ionc_put_mesh_contact_v1_dll

function ionc_write_mesh_1d_edge_nodes_dll(ioncid, meshid, numEdge, c_mesh_1d_edge_nodes, start_index) result(ierr) bind(C, name="ionc_write_mesh_1d_edge_nodes")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_write_mesh_1d_edge_nodes_dll
   integer, intent(in)                                         :: ioncid, meshid, numEdge, start_index
   type(c_ptr), intent(in)                                     :: c_mesh_1d_edge_nodes
   integer,pointer                                             :: mesh_1d_edge_nodes(:,:)
   integer                                                     :: ierr

   call c_f_pointer(c_mesh_1d_edge_nodes, mesh_1d_edge_nodes, (/ numEdge /))

   ierr = ionc_write_mesh_1d_edge_nodes (ioncid, meshid, numEdge, mesh_1d_edge_nodes, start_index)

end function ionc_write_mesh_1d_edge_nodes_dll

function ionc_get_mesh_contact_dll(ioncid, contactsmesh, c_mesh1indexes, c_mesh2indexes, c_contacttype, contactsinfo, ncontacts, startIndex) result(ierr) bind(C, name="ionc_get_mesh_contact")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_mesh_contact_dll
   integer, intent(in)                   :: ioncid, contactsmesh, ncontacts, startIndex
   type(c_ptr), intent(inout)            :: c_mesh1indexes, c_mesh2indexes, c_contacttype
   integer,    pointer                   :: mesh1indexes(:), mesh2indexes(:), contacttype(:)
   character(len=ug_idsLen)              :: contactsids(ncontacts)
   character(len=ug_idsLongNamesLen)     :: contactslongnames(ncontacts)
   type(t_ug_charinfo),  intent(inout)   :: contactsinfo(ncontacts)
   integer                               :: ierr, i

   call c_f_pointer(c_mesh1indexes, mesh1indexes, (/ ncontacts /))
   call c_f_pointer(c_mesh2indexes, mesh2indexes, (/ ncontacts /))
   call c_f_pointer(c_contacttype, contacttype, (/ ncontacts /))

   ierr = ionc_get_mesh_contact_ugrid(ioncid, contactsmesh, mesh1indexes, mesh2indexes, contactsids, contactslongnames, contacttype, startIndex)

   do i=1,ncontacts
     contactsinfo(i)%id = contactsids(i)
     contactsinfo(i)%longname = contactslongnames(i)
   end do

end function ionc_get_mesh_contact_dll

function ionc_get_mesh_contact_v1_dll(ioncid, contactsmesh, c_mesh1indexes, c_mesh2indexes, c_contacttype, c_ids, c_longnames, ncontacts, startIndex) result(ierr) bind(C, name="ionc_get_mesh_contact_v1")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_mesh_contact_v1_dll
   integer, intent(in)                   :: ioncid, contactsmesh, ncontacts, startIndex
   type(c_ptr), intent(inout)            :: c_mesh1indexes, c_mesh2indexes, c_contacttype
   integer,    pointer                   :: mesh1indexes(:), mesh2indexes(:), contacttype(:)
   type(c_ptr),  intent(inout)           :: c_ids, c_longnames
   integer                               :: ierr
   character(kind=c_char, len=ug_idsLen),pointer               :: ids(:)
   character(kind=c_char, len=ug_idsLongNamesLen),pointer      :: longnames(:)

   call c_f_pointer(c_mesh1indexes, mesh1indexes, (/ ncontacts /))
   call c_f_pointer(c_mesh2indexes, mesh2indexes, (/ ncontacts /))
   call c_f_pointer(c_contacttype, contacttype, (/ ncontacts /))

   call c_f_pointer(c_ids, ids, (/ ncontacts /))
   call c_f_pointer(c_longnames, longnames, (/ ncontacts /))

   ierr = ionc_get_mesh_contact_ugrid(ioncid, contactsmesh, mesh1indexes, mesh2indexes, ids, longnames, contacttype, startIndex)


end function ionc_get_mesh_contact_v1_dll

!
! Clone functions
!

function ionc_clone_mesh_definition_dll( ncidin, ncidout, meshidin, meshidout )  result(ierr) bind(C, name="ionc_clone_mesh_definition")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_clone_mesh_definition_dll
   integer, intent(in)    :: ncidin, ncidout, meshidin
   integer, intent(inout)    :: meshidout
   integer                :: ierr

   ierr = ionc_clone_mesh_definition_ugrid( ncidin, ncidout, meshidin, meshidout )

end function ionc_clone_mesh_definition_dll

function ionc_clone_mesh_data_dll( ncidin, ncidout, meshidin, meshidout )  result(ierr) bind(C, name="ionc_clone_mesh_data")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_clone_mesh_data_dll
   integer, intent(in)    :: ncidin, ncidout, meshidin
   integer, intent(in)    :: meshidout
   integer                :: ierr

   ierr = ionc_clone_mesh_data_ugrid( ncidin, ncidout, meshidin, meshidout )

end function ionc_clone_mesh_data_dll

function ionc_get_lib_versionversion_dll( ncidin, c_version_string)  result(ierr) bind(C, name="ionc_get_lib_fullversion")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_lib_versionversion_dll
   integer, intent(in)    :: ncidin
   character(kind=c_char), intent(out) :: c_version_string(MAXSTRLEN)      !< String to contain the full version string of this io_netcdf library.
   integer                :: ierr

   character(len=MAXSTRLEN) :: version_string

   version_string = ' '
   ierr = ionc_getfullversionstring_io_netcdf(version_string)
   c_version_string= string_to_char_array(version_string, len_trim(version_string))

end function ionc_get_lib_versionversion_dll

!
! Get the mesh ids
!

function ionc_get_number_of_networks_dll(ncidin, nnumNetworks) result(ierr) bind(C, name="ionc_get_number_of_networks")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_number_of_networks_dll
   integer, intent(in)    :: ncidin
   integer, intent(inout) :: nnumNetworks
   integer                :: ierr

   ierr = ionc_get_number_of_networks_ugrid(ncidin, nnumNetworks)

end function ionc_get_number_of_networks_dll

function ionc_get_number_of_meshes_dll(ncidin, meshType, numMeshes) result(ierr) bind(C, name="ionc_get_number_of_meshes")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_number_of_meshes_dll
   integer, intent(in)    :: ncidin, meshType
   integer, intent(inout) :: numMeshes
   integer                :: ierr

   ierr = ionc_get_number_of_meshes_ugrid(ncidin, meshType, numMeshes)

end function ionc_get_number_of_meshes_dll

function ionc_get_network_ids_dll(ncidin, c_networkids, nnumNetworks) result(ierr) bind(C, name="ionc_get_network_ids")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_network_ids_dll
   integer, intent(in)        :: ncidin
   type(c_ptr), intent(inout) :: c_networkids
   integer, intent(inout)     :: nnumNetworks
   integer                    :: ierr
   integer, pointer           :: networkids(:)

   call c_f_pointer(c_networkids, networkids, (/ nnumNetworks /))

   ierr = ionc_get_network_ids_ugrid(ncidin, networkids)

end function ionc_get_network_ids_dll

!< this function returns an array of chars
function ionc_get_1d_network_name_dll(ioncid, networkid, c_networkName)  result(ierr) bind(C, name="ionc_get_1d_network_name")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_1d_network_name_dll
   integer, intent(in)                         :: ioncid
   integer(kind=c_int), intent(in)             :: networkid
   character(kind=c_char,len=1), intent(  out) :: c_networkName(MAXSTRLEN) !< The name of the network topology variable.
   character(len=MAXSTRLEN)                    :: networkname !< The name of the network.
   integer                                     :: ierr        !< Result status, ionc_noerr if successful.

   networkname = ''
   ierr = ionc_get_network_name(ioncid, networkid, networkname)
   c_networkName= string_to_char_array(networkname,len_trim(networkname))

end function ionc_get_1d_network_name_dll

function ionc_ug_get_mesh_ids_dll(ioncid, meshType, c_meshids, numMeshes) result(ierr) bind(C, name="ionc_ug_get_mesh_ids")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_ug_get_mesh_ids_dll
   integer, intent(in)        :: ioncid, meshType
   type(c_ptr), intent(inout) :: c_meshids
   integer, intent(in)        :: numMeshes
   integer                    :: ierr
   integer, pointer           :: meshids(:)

   call c_f_pointer(c_meshids, meshids, (/ numMeshes /))

   ierr = ionc_ug_get_mesh_ids_ugrid(ioncid, meshType, meshids)

end function ionc_ug_get_mesh_ids_dll

function ionc_get_1d_network_id_dll(ioncid, networkid) result(ierr) bind(C, name="ionc_get_1d_network_id")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_1d_network_id_dll
   integer, intent(in)    :: ioncid
   integer, intent(inout) :: networkid
   integer                :: ierr

   ierr = ionc_get_1d_network_id_ugrid(ioncid, networkid)

end function ionc_get_1d_network_id_dll


function ionc_get_1d_mesh_id_dll(ioncid, meshid) result(ierr) bind(C, name="ionc_get_1d_mesh_id")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_1d_mesh_id_dll
   integer, intent(in)    :: ioncid
   integer, intent(inout) :: meshid
   integer                :: ierr

   ierr = ionc_get_1d_mesh_id_ugrid(ioncid, meshid)

end function ionc_get_1d_mesh_id_dll


function ionc_get_2d_mesh_id_dll(ioncid, meshid) result(ierr) bind(C, name="ionc_get_2d_mesh_id")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_2d_mesh_id_dll
   integer, intent(in)    :: ioncid
   integer, intent(inout) :: meshid
   integer                :: ierr

   ierr = ionc_get_2d_mesh_id_ugrid(ioncid, meshid)

end function ionc_get_2d_mesh_id_dll

function ionc_get_3d_mesh_id_dll(ioncid, meshid)  result(ierr) bind(C, name="ionc_get_3d_mesh_id")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_3d_mesh_id_dll
   integer, intent(in)    :: ioncid
   integer, intent(inout) :: meshid
   integer                :: ierr

   ierr = ionc_get_3d_mesh_id_ugrid(ioncid, meshid)

end function ionc_get_3d_mesh_id_dll


function ionc_get_contact_id_dll(ioncid, contactid)  result(ierr) bind(C, name="ionc_get_contact_id")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_contact_id_dll
   integer, intent(in)    :: ioncid
   integer, intent(inout) :: contactid
   integer                :: ierr

   ! for now we are assuming that just 1 contact "mesh" exist
   ierr = ionc_get_contact_id_ugrid(ioncid, contactid)

end function ionc_get_contact_id_dll

function ionc_count_mesh_ids_from_network_id_dll(ioncid, networkid, nmeshids) result(ierr) bind(C, name="ionc_count_mesh_ids_from_network_id")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_count_mesh_ids_from_network_id_dll
   integer, intent(in)    :: ioncid
   integer, intent(in)    :: networkid
   integer, intent(inout) :: nmeshids
   integer                :: ierr

   ierr = ionc_count_mesh_ids_from_network_id_ugrid(ioncid, networkid, nmeshids)

end function ionc_count_mesh_ids_from_network_id_dll


function ionc_get_mesh_ids_from_network_id_dll(ioncid, networkid, nmeshids, c_meshids) result(ierr) bind(C, name="ionc_get_mesh_ids_from_network_id")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_mesh_ids_from_network_id_dll
   integer, intent(in)        :: ioncid
   integer, intent(in)        :: networkid
   type(c_ptr), intent(inout) :: c_meshids
   integer, intent(in)        :: nmeshids
   integer, pointer           :: meshids(:)
   integer                    :: ierr

   call c_f_pointer(c_meshids, meshids, (/ nmeshids /))

   ierr = ionc_get_mesh_ids_from_network_id_ugrid(ioncid, networkid, meshids)

end function ionc_get_mesh_ids_from_network_id_dll

function ionc_get_network_id_from_mesh_id_dll(ioncid, meshid, networkid) result(ierr) bind(C, name="ionc_get_network_id_from_mesh_id")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_network_id_from_mesh_id_dll
   integer, intent(in)        :: ioncid
   integer, intent(in)        :: meshid
   integer, intent(inout)     :: networkid
   integer                    :: ierr

   ierr = ionc_get_network_id_from_mesh_id_ugrid(ioncid, meshid, networkid)

end function ionc_get_network_id_from_mesh_id_dll


function ionc_get_meshgeom_dim_dll(ioncid, meshid, networkid, c_meshgeomdim) result(ierr) bind(C, name="ionc_get_meshgeom_dim")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_meshgeom_dim_dll
   integer, intent(in)                         :: ioncid
   integer, intent(in)                         :: meshid, networkid
   type(c_t_ug_meshgeomdim), intent(inout)     :: c_meshgeomdim
   type(t_ug_meshgeom)                         :: meshgeom
   integer                                     :: ierr

   ierr = ionc_get_meshgeom(ioncid, meshid, networkid, meshgeom)

   c_meshgeomdim%dim             = meshgeom%dim
   c_meshgeomdim%numnode         = meshgeom%numnode
   c_meshgeomdim%numedge         = meshgeom%numedge
   c_meshgeomdim%numface         = meshgeom%numface
   c_meshgeomdim%maxnumfacenodes = meshgeom%maxnumfacenodes
   c_meshgeomdim%num_layers      = meshgeom%num_layers
   c_meshgeomdim%layertype       = meshgeom%layertype
   c_meshgeomdim%nnodes          = meshgeom%nnodes
   c_meshgeomdim%nbranches       = meshgeom%nbranches
   c_meshgeomdim%ngeometry       = meshgeom%ngeometry

end function ionc_get_meshgeom_dim_dll

function ionc_get_meshgeom_dll(ioncid, meshid, networkid, c_meshgeom, start_index, includeArrays) result(ierr) bind(C, name="ionc_get_meshgeom")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_meshgeom_dll
   use meshdata
   integer, intent(in)                         :: ioncid, meshid, networkid, start_index
   type (c_t_ug_meshgeom), intent(inout)       :: c_meshgeom
   logical, optional,   intent(in   )          :: includeArrays
   integer                                     :: ierr
   type(t_ug_meshgeom)                         :: meshgeom
   type (c_t_ug_meshgeomdim)                   :: c_meshgeomDim ! currently this is not an output argument, but it could be considered


   !initialize meshgeom
   ierr = t_ug_meshgeom_destructor(meshgeom)
   !get the mesh geometry
   ierr = ionc_get_meshgeom(ioncid, meshid, networkid, meshgeom, start_index, includeArrays)
   !set the pointers in c_meshgeom
   ierr = convert_meshgeom_to_cptr(meshgeom, c_meshgeom, c_meshgeomDim)

end function ionc_get_meshgeom_dll

function ionc_get_meshgeom_v1_dll(ioncid, meshid, networkid, c_meshgeom) result(ierr) bind(C, name="ionc_get_meshgeom_v1")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_meshgeom_v1_dll
   use meshdata
   integer, intent(in)                                     :: ioncid, meshid, networkid
   type (c_t_ug_meshgeom), intent(inout)                   :: c_meshgeom
   integer                                                 :: ierr
   type(t_ug_meshgeom)                                     :: meshgeom
   type (c_t_ug_meshgeomdim)                               :: c_meshgeomDim !currently this is not an output argument, but it could be considered

   character(len=ug_idsLen), allocatable, target           :: nnodeids(:), nbranchids(:), nodeids(:)
   character(len=ug_idsLongNamesLen), allocatable,target   :: nnodelongnames(:), nbranchlongnames(:), nodelongnames(:)
   character(len=ug_nameLen),target                        :: mesh1dname
   character(len=ug_nameLen),target                        :: network1dname

   !initialize meshgeom
   ierr = t_ug_meshgeom_destructor(meshgeom)

   !get the sizes first
   ierr = ionc_get_meshgeom(ioncid, meshid, networkid, meshgeom)

   !allocate the arrays
   allocate(nbranchids(meshgeom%nbranches))
   allocate(nbranchlongnames(meshgeom%nbranches))
   allocate(nnodeids(meshgeom%nnodes))
   allocate(nnodelongnames(meshgeom%nnodes))
   allocate(nodeids(meshgeom%numnode ))
   allocate(nodelongnames(meshgeom%numnode))

   !get the mesh geometry
   ierr = ionc_get_meshgeom(ioncid, meshid, networkid, meshgeom, c_meshgeom%start_index, .true.,nbranchids = nbranchids,&
                            nbranchlongnames = nbranchlongnames, nnodeids = nnodeids, nnodelongnames = nnodelongnames, nodeids = nodeids, nodelongnames = nodelongnames,&
                            mesh1dname = mesh1dname, network1dname = network1dname)

   ! Set the pointers in c_meshgeom
   meshgeom%nbranchids=>nbranchids
   meshgeom%nbranchlongnames=>nbranchlongnames
   meshgeom%nnodeids=>nnodeids
   meshgeom%nnodelongnames=>nnodelongnames
   meshgeom%nodeids=>nodeids
   meshgeom%nodelongnames=>nodelongnames

   ! When separating meshgeom in meshgeom + networkgeom we can put the mesh name and network name in the separate structure fields
   ierr = convert_meshgeom_to_cptr(meshgeom, c_meshgeom, c_meshgeomDim)

end function ionc_get_meshgeom_v1_dll

function ionc_put_meshgeom_dll(ioncid, meshid, networkid, c_meshgeom, c_meshgeomdim, c_meshname, c_networkName, start_index) result(ierr) bind(C, name="ionc_put_meshgeom")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_put_meshgeom_dll
   use meshdata
   integer, intent(in)                         :: ioncid
   integer, intent (inout)                     :: meshid, networkid
   type (c_t_ug_meshgeom), intent(in)          :: c_meshgeom
   type (c_t_ug_meshgeomdim)                   :: c_meshgeomdim
   character(kind=c_char), intent(in)          :: c_meshname(maxstrlen)
   character(kind=c_char), intent(in)          :: c_networkName(maxstrlen)
   integer, intent(in)                         :: start_index
   integer                                     :: ierr
   type(t_ug_meshgeom)                         :: meshgeom

   character(len=maxstrlen)                    :: meshname
   character(len=maxstrlen)                    :: networkName

   ! Store the name
   meshname = char_array_to_string(c_meshname, strlen(c_meshname))
   networkName = char_array_to_string(c_networkName, strlen(c_networkName))

   !initialize meshgeom
   ierr = t_ug_meshgeom_destructor(meshgeom)
   !convert c_meshgeom to meshgeom
   ierr = convert_cptr_to_meshgeom(c_meshgeom, c_meshgeomdim, meshgeom)
   !write meshgeom
   meshgeom%start_index = start_index
   ierr = ionc_put_meshgeom(ioncid, meshgeom, meshid, networkid,  meshname, networkName)

end function ionc_put_meshgeom_dll


function ionc_put_meshgeom_v1_dll(ioncid, meshid, networkid, c_meshgeom, c_meshgeomdim) result(ierr) bind(C, name="ionc_put_meshgeom_v1")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_put_meshgeom_v1_dll

   use meshdata
   integer, intent(in)                         :: ioncid
   integer, intent (inout)                     :: meshid, networkid
   type (c_t_ug_meshgeom), intent(in)          :: c_meshgeom
   type (c_t_ug_meshgeomdim)                   :: c_meshgeomdim
   integer                                     :: ierr
   ! Locals
   type(t_ug_meshgeom)                         :: meshgeom

   !initialize meshgeom
   ierr = t_ug_meshgeom_destructor(meshgeom)
   !convert c_meshgeom to meshgeom
   ierr = convert_cptr_to_meshgeom(c_meshgeom, c_meshgeomdim, meshgeom)
   !write meshgeom
   ierr = ionc_put_meshgeom_v1(ioncid, meshgeom, meshid, networkid)

end function ionc_put_meshgeom_v1_dll


function ionc_put_network_dll(ioncid, networkid, c_networkgeom, c_networkgeomdim) result(ierr) bind(C, name="ionc_put_network")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_put_network_dll

   use meshdata
   integer, intent(in)                         :: ioncid
   integer, intent (inout)                     :: networkid
   type (c_t_ug_meshgeom), intent(in)          :: c_networkgeom
   type (c_t_ug_meshgeomdim)                   :: c_networkgeomdim
   integer                                     :: ierr
   ! Locals
   type(t_ug_meshgeom)                         :: networkgeom

   !initialize meshgeom
   ierr = t_ug_meshgeom_destructor(networkgeom)
   !convert c_meshgeom to meshgeom
   ierr = convert_cptr_to_meshgeom(c_networkgeom, c_networkgeomdim, networkgeom)
   !write meshgeom
   ierr = ionc_put_network(ioncid, networkgeom, networkid)

end function ionc_put_network_dll

end module io_netcdf_api
