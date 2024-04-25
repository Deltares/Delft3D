!----- LGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2024.
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


!> I/O module for reading and writing NetCDF files based on selected NetCDF conventions (UGRID, and more in the future).
!! @see io_ugrid
module io_netcdf
use netcdf
use io_ugrid
use coordinate_reference_system
use m_ug_meshgeom
use m_ug_charinfo

implicit none

!
! Parameters
!
public :: IONC_CONV_NULL
public :: IONC_CONV_CF
public :: IONC_CONV_UGRID
public :: IONC_CONV_SGRID
public :: IONC_CONV_OTHER

!
! Error codes
!
public :: IONC_NOERR
public :: IONC_EBADID
public :: IONC_ENOPEN
public :: IONC_ENOMEM
public :: IONC_ENONCOMPLIANT
public :: IONC_ENOTAVAILABLE

!
! Types
!
public :: t_ionc
public :: t_ug_meta
public :: t_ug_charinfo
public :: ug_strLenMeta
public :: t_ug_meshgeom
public :: c_t_ug_meshgeom
public :: c_t_ug_meshgeomdim
public :: ug_idsLen
public :: ug_idsLongNamesLen

!
! Subroutines
!
public :: ionc_strerror
public :: ionc_get_constant
public :: ionc_create
public :: ionc_inq_conventions
public :: ionc_adheresto_conventions
public :: ionc_add_global_attributes
public :: ionc_open
public :: ionc_close
public :: ionc_enddef
public :: ionc_redef
public :: ionc_get_ncid
public :: ionc_get_mesh_count
public :: ionc_get_mesh_name
public :: ionc_get_topology_dimension
public :: ionc_get_meshgeom
public :: ionc_get_node_count
public :: ionc_get_edge_count
public :: ionc_get_face_count
public :: ionc_get_max_face_nodes
public :: ionc_get_node_coordinates
public :: ionc_put_node_coordinates
public :: ionc_get_edge_faces
public :: ionc_get_edge_nodes
public :: ionc_get_face_coordinates
public :: ionc_put_face_coordinates
public :: ionc_get_face_edges
public :: ionc_get_face_nodes
public :: ionc_get_coordinate_reference_system
public :: ionc_get_epsg_code
public :: ionc_set_coordinate_reference_system
public :: ionc_get_meta_data
public :: ionc_get_var_count
public :: ionc_inq_varids
public :: ionc_inq_varid
public :: ionc_inq_varid_by_standard_name
public :: ionc_def_var
public :: ionc_get_var_1D_EightByteReal
public :: ionc_put_var_1D_EightByteReal
public :: ionc_write_geom_ugrid
public :: ionc_write_mesh_struct
public :: ionc_write_map_ugrid
public :: ionc_initialize
public :: ionc_check_dim_on_a_mesh
!network 1d functions
public :: ionc_create_1d_network_ugrid
public :: ionc_write_1d_network_nodes_ugrid
public :: ionc_put_1d_network_branches_ugrid
public :: ionc_write_1d_network_branches_geometry_ugrid
public :: ionc_get_1d_netids
public :: ionc_get_1d_network_nodes_count_ugrid
public :: ionc_get_1d_network_branches_count_ugrid
public :: ionc_get_1d_network_branches_geometry_coordinate_count_ugrid
public :: ionc_read_1d_network_nodes_ugrid
public :: ionc_get_1d_network_branches_ugrid
public :: ionc_read_1d_network_branches_geometry_ugrid
public :: ionc_create_1d_mesh_ugrid
public :: ionc_put_1d_mesh_discretisation_points_ugrid
public :: ionc_put_1d_mesh_discretisation_points_ugrid_v1
public :: ionc_get_1d_mesh_discretisation_points_count_ugrid
public :: ionc_get_1d_mesh_discretisation_points_ugrid
public :: ionc_get_1d_mesh_discretisation_points_ugrid_v1
public :: ionc_put_1d_mesh_edges
public :: ionc_write_mesh_1d_edge_nodes
public :: ionc_create_1d_mesh_ugrid_v1
public :: ionc_put_network
public :: ionc_get_1d_mesh_edges
!links functions
public :: ionc_def_mesh_contact_ugrid
public :: ionc_get_contacts_count_ugrid
public :: ionc_put_mesh_contact_ugrid
public :: ionc_get_mesh_contact_ugrid
public :: ionc_get_mesh_contact_links
public :: ionc_get_contact_name
public :: ionc_get_contactids
public :: ionc_get_contact_id_from_contact_name_ugrid
!clone functions
public :: ionc_clone_mesh_definition_ugrid
public :: ionc_clone_mesh_data_ugrid
!get ids functions (obsolete)
public :: ionc_get_1d_network_id_ugrid
public :: ionc_get_1d_mesh_id_ugrid
public :: ionc_get_2d_mesh_id_ugrid
public :: ionc_get_3d_mesh_id_ugrid
!get number of meshes
public :: ionc_get_number_of_networks_ugrid
public :: ionc_get_number_of_meshes_ugrid
public :: ionc_get_network_ids_ugrid
public :: ionc_ug_get_mesh_ids_ugrid
!branch order
public :: ionc_put_1d_network_branchorder_ugrid
public :: ionc_get_1d_network_branchorder_ugrid
!branch order
public :: ionc_put_1d_network_branchtype_ugrid
public :: ionc_get_1d_network_branchtype_ugrid
!get network names
public :: ionc_get_network_name
!get the meshids from network ids
public :: ionc_count_mesh_ids_from_network_id_ugrid
public :: ionc_get_mesh_ids_from_network_id_ugrid
public :: ionc_get_network_id_from_mesh_id_ugrid
! define/put/read mesh ids
public :: ionc_def_mesh_ids_ugrid
public :: ionc_put_var_chars
public :: ionc_get_var_chars
public :: ionc_getfullversionstring_io_netcdf
public :: ionc_get_dimid
public :: ionc_get_contact_id_ugrid
public :: ionc_put_meshgeom
public :: ionc_put_meshgeom_v1
public :: ionc_get_contact_topo_count
public :: ionc_get_var_total_count

private

!
! NetCDF conventions support. Based on the conventions used in the file,
! this library supports a bigger or smaller amount of intelligent inquiry functions.
! Note: a data set may adhere to multiple conventions at the same time.
! This is stored as an integer sum of the respective convention types. Therefore,
! all types below must be powers of two, to allow all combinations in one number.
!
integer, parameter :: IONC_CONV_NULL  = 0   !< Dataset conventions not yet detected
integer, parameter :: IONC_CONV_CF    = 1   !< Dataset conventions not yet detected
integer, parameter :: IONC_CONV_UGRID = 2   !< Dataset based on UGRID-conventions
integer, parameter :: IONC_CONV_SGRID = 4   !< Dataset based on SGRID-conventions
integer, parameter :: IONC_CONV_OTHER = -99 !< Dataset based on unknown or unsupported conventions (user should fall back to NetCDF native API calls)

integer, public, parameter :: MAXSTRLEN = 255 !< Max string length (e.g. for inquiring attribute values.

!
! Error statuses
!
integer, parameter :: IONC_NOERR         = 0     !< Successful
integer, parameter :: IONC_EBADID        = -2001 !< Not a valid IONC dataset id
integer, parameter :: IONC_ENOPEN        = -2002 !< File could not be opened
integer, parameter :: IONC_ENOMEM        = -2003 !< Memory allocation error
integer, parameter :: IONC_ENONCOMPLIANT = -2004 !< File is non-compliant with its specified conventions
integer, parameter :: IONC_ENOTAVAILABLE = -2005 !< Requested function is not available, because the file has different conventions.



!> Data type with reference to a NetCDF dataset
type t_ionc
   integer                  :: ncid      =  0               !< The underlying native NetCDF data set id.
   integer                  :: iconvtype =  IONC_CONV_OTHER !< Detected type of the conventions used in this dataset.
   double precision         :: convversion =  0             !< Detected version of the conventions used in this dataset.
   type(t_ug_file), pointer :: ug_file   => null()          !< For UGRID-type files, the underlying file structure.
   type(t_crs),     pointer :: crs                          !< Map projection/coordinate transformation used for the coordinates of this mesh.
end type t_ionc

type(t_ionc), allocatable :: datasets(:) !< List of available datasets, maintained in global library state, indexed by the unique ionc_id.
integer                   :: ndatasets   !< Number of available datasets. May be smaller than array size of datasets(:).

!-------------------------------------------------------------------------------
   contains
!-------------------------------------------------------------------------------

!> Given an error number, return an error message.
!!
!! Use this when a previous function call has returned a nonzero error status.
!! Note that the error number may be an IONC error, but also an underlying UG error.
function ionc_strerror(ierr) result(str)
   integer,                       intent(in) :: ierr !< Integer error code for which to return the error message.
   character(len=:), allocatable             :: str  !< String variable in which the message will be stored.

   select case (ierr)
      ! 1. First try list of IONC error numbers...
   case (IONC_NOERR);         str = 'No error'
   case (IONC_EBADID);        str = 'Bad io_netcdf dataset ID'
   case (IONC_ENOPEN);        str = 'File not opened/created'
   case (IONC_ENOMEM);        str = 'Memory allocation failure'
   case (IONC_ENONCOMPLIANT); str = 'Dataset non-compliant to any of the supported standards'
   case (IONC_ENOTAVAILABLE); str = 'Functionality not available for this dataset (possibly dataset adheres to different conventions?)'
   case default
      ! 2. Otherwise, try in list of UGRID error numbers...
      str = ug_strerror(ierr)
   end select

   if(str .eq. 'Unknown error') then
      str = nf90_strerror(ierr) ! 3. Try list of NETCDF error numbers
   end if

end function ionc_strerror


!> Returns the integer value for a named constant.
!! When requested constant does not exist, the returned value is undefined, and ierr contains an error code.
integer function ionc_get_constant(constname, constvalue) result(ierr)
   character(len=*), intent(in)    :: constname  !< The name of the requested constant.
   integer,          intent(  out) :: constvalue !< The integer value of the requested constant.

   ierr = IONC_NOERR

   select case (trim(constname))
      ! 1. First try list of IONC constants...
   case ('IONC_CONV_NULL');     constvalue = IONC_CONV_NULL
   case ('IONC_CONV_CF');       constvalue = IONC_CONV_CF
   case ('IONC_CONV_UGRID');    constvalue = IONC_CONV_UGRID
   case ('IONC_CONV_SGRID');    constvalue = IONC_CONV_SGRID
   case ('IONC_CONV_OTHER');    constvalue = IONC_CONV_OTHER
   case ('IONC_NOERR');         constvalue = IONC_NOERR
   case ('IONC_EBADID');        constvalue = IONC_EBADID
   case ('IONC_ENOPEN');        constvalue = IONC_ENOPEN
   case ('IONC_ENOMEM');        constvalue = IONC_ENOMEM
   case ('IONC_ENONCOMPLIANT'); constvalue = IONC_ENONCOMPLIANT
   case ('IONC_ENOTAVAILABLE'); constvalue = IONC_ENOTAVAILABLE
   case default
      ! 2. Otherwise, try in list of UGRID constants...
      ierr = ug_get_constant(constname, constvalue)
   end select
end function ionc_get_constant


!> Tries to create a NetCDF file and initialize based on its specified conventions.
function ionc_create(netCDFFile, mode, ioncid, iconvtype, chunksize) result(ierr)
   character (len=*), intent(in   ) :: netCDFFile!< File name for netCDF dataset to be opened.
   integer,           intent(in   ) :: mode      !< NetCDF open mode, e.g. NF90_NOWRITE.
   integer,           intent(  out) :: ioncid    !< The io_netcdf dataset id (this is not the NetCDF ncid, which is stored in datasets(ioncid)%ncid.
   integer,           intent(in   ) :: iconvtype !< (optional) The desired conventions for the new file.
   integer, optional, intent(inout) :: chunksize !< (optional) NetCDF chunksize parameter.
   integer                          :: ierr      !< Result status (IONC_NOERR if successful).

   integer :: ncid, istat

   if (present(chunksize)) then
      ierr = nf90_create(netCDFFile, mode, ncid, chunksize)
   else
      ierr = nf90_create(netCDFFile, mode, ncid)
   end if

   if (ierr /= nf90_noerr) then
      ierr = IONC_ENOPEN
      goto 999
   end if

   ierr = add_dataset(ncid, netCDFFile, ioncid, iconvtype)

999 continue
end function ionc_create


!> Inquire the NetCDF conventions used in the dataset.
function ionc_inq_conventions(ioncid, iconvtype, convversion) result(ierr)
   integer, intent(in)  :: ioncid    !< The IONC data set id.
   integer, intent(out) :: iconvtype !< The NetCDF conventions type of the dataset.
   double precision, intent(out) :: convversion !< The NetCDF conventions version of the dataset.
   integer              :: ierr      !< Result status, ionc_noerr if successful.

   ierr = IONC_NOERR

   if (ioncid < 1 .or. ioncid > ndatasets) then
      ierr = IONC_EBADID
      goto 999
   end if

   ! Either get conventions from previously detected value, or detect them now.
   if (datasets(ioncid)%iconvtype == IONC_CONV_NULL) then
      ierr = detect_conventions(ioncid)
      if (ierr /= IONC_NOERR) then
         goto 999
      end if
   end if
   iconvtype = datasets(ioncid)%iconvtype
   convversion = datasets(ioncid)%convversion

   ! Successful
   return

999 continue
   ! Some error (status was set earlier)
end function ionc_inq_conventions


!> Checks whether the specified data set adheres to a specific set of conventions.
!! Datasets may adhere to multiple conventions at the same time, so use this method
!! to check for individual conventions.
function ionc_adheresto_conventions(ioncid, iconvtype) result(does_adhere)
   integer, intent(in) :: ioncid      !< The IONC data set id.
   integer, intent(in) :: iconvtype   !< The NetCDF conventions type to check for.
   logical             :: does_adhere !< Whether or not the file adheres to the specified conventions.

   if (ioncid < 1 .or. ioncid > ndatasets) then
      does_adhere = .false.
      goto 999
   end if

   ! Perform logical AND to determine whether iconvtype is inside dataset's conventions 'set'.
   does_adhere = iand(datasets(ioncid)%iconvtype, iconvtype) == iconvtype

   ! Successful
   return

999 continue
   ! Some error (return .false.)
end function ionc_adheresto_conventions


!> Tries to open a NetCDF file and initialize based on the file's conventions.
function ionc_open(netCDFFile, mode, ioncid, iconvtype, convversion, chunksize) result(ierr)
   character (len=*), intent(in   ) :: netCDFFile!< File name for netCDF dataset to be opened.
   integer,           intent(in   ) :: mode      !< NetCDF open mode, e.g. NF90_NOWRITE.
   integer,           intent(  out) :: ioncid    !< The io_netcdf dataset id (this is not the NetCDF ncid, which is stored in datasets(ioncid)%ncid.
   integer, optional, intent(  out) :: iconvtype !< (optional) The detected conventions in the file.
   double precision, optional, intent(  out) :: convversion !< (optional) The detected conventions in the file.
   integer, optional, intent(  out) :: chunksize !< (optional) NetCDF chunksize parameter.
   integer                          :: ierr      !< Result status (IONC_NOERR if successful).

   integer :: ncid, istat

   ierr = IONC_NOERR

   if (present(chunksize)) then
      ierr = nf90_open(netCDFFile, mode, ncid, chunksize)
   else
      ierr = nf90_open(netCDFFile, mode, ncid)
   end if

   if (ierr /= nf90_noerr) then
      ierr = IONC_ENOPEN
      goto 999
   end if

   ierr = add_dataset(ncid, netCDFFile, ioncid)
   if (ierr /= ionc_noerr) then
      ierr = IONC_ENOPEN
      goto 999
   end if

   if (present(iconvtype)) then
      iconvtype = datasets(ioncid)%iconvtype
   end if

   if (present(convversion)) then
      convversion = datasets(ioncid)%convversion
   end if

   ! Successful
   return

999 continue

end function ionc_open


!> Tries to close an open io_netcdf data set.
function ionc_close(ioncid) result(ierr)
   integer,           intent(in   ) :: ioncid    !< The io_netcdf dataset id (this is not the NetCDF ncid, which is stored in datasets(ioncid)%ncid.
   integer                          :: ierr      !< Result status (IONC_NOERR if successful).

   integer :: ncid, istat

   if (ioncid <= 0 .or. ioncid > ndatasets) then
      ierr = IONC_EBADID
      goto 999
   end if

   ierr = nf90_close(datasets(ioncid)%ncid)
   datasets(ioncid)%ncid = 0 ! Mark as closed

   select case (datasets(ioncid)%iconvtype)
   case (IONC_CONV_UGRID)
      if (associated(datasets(ioncid)%ug_file)) then
         deallocate(datasets(ioncid)%ug_file)
      end if
   end select

   ! Successful
   return

999 continue
   ! Some error (status was set earlier)
   return
end function ionc_close

!> Tries to end define mode of an open io_netcdf data set.
function ionc_enddef(ioncid) result(ierr)
   integer,           intent(in   ) :: ioncid    !< The io_netcdf dataset id (this is not the NetCDF ncid, which is stored in datasets(ioncid)%ncid.
   integer                          :: ierr      !< Result status (IONC_NOERR if successful).

   if (ioncid <= 0 .or. ioncid > ndatasets) then
      ierr = IONC_EBADID
      goto 999
   end if
   ierr = nf90_enddef(datasets(ioncid)%ncid)
   if (ierr == nf90_enotindefine) then
      ! Already in data mode
      ierr = IONC_NOERR
   end if

   ! Successful
   return

999 continue
   ! Some error (status was set earlier)
   return
end function ionc_enddef

!> Tries to start define mode of an open io_netcdf data set.
function ionc_redef(ioncid) result(ierr)
   integer,           intent(in   ) :: ioncid    !< The io_netcdf dataset id (this is not the NetCDF ncid, which is stored in datasets(ioncid)%ncid.
   integer                          :: ierr      !< Result status (IONC_NOERR if successful).

   integer :: ncid, istat

   if (ioncid <= 0 .or. ioncid > ndatasets) then
      ierr = IONC_EBADID
      goto 999
   end if
   ierr = nf90_redef(datasets(ioncid)%ncid)

   if (ierr == nf90_eindefine) then
      ! Already in define mode
      ierr = IONC_NOERR
   end if

   ! Successful
   return

999 continue
   ! Some error (status was set earlier)
   return
end function ionc_redef


!> Gets the native NetCDF ID for the specified dataset.
!! Intended for use in subsequent calls to nf90_* primitives outside of this library.
!! Use this routine as little as possible, and try and do all via ionc API functions.
function ionc_get_ncid(ioncid, ncid) result(ierr)
   integer,             intent(in)    :: ioncid   !< The IONC data set id.
   integer,             intent(  out) :: ncid     !< The NetCDF ID, if data set ioncid was valid.
   integer                            :: ierr     !< Result status, ionc_noerr if successful.

   ierr = IONC_NOERR

   if (ioncid > 0 .and. ioncid <= ndatasets) then
      ncid = datasets(ioncid)%ncid
   else
      ierr = IONC_EBADID
      goto 999
   end if

   ! Successful
   return

999 continue
   ! Some error (status was set earlier)
   return

end function ionc_get_ncid

!> Gets the NetCDF dimension id for a specific mesh and a specific topology dimension.
function ionc_get_dimid(ioncid, meshid, idim, dimid) result(ierr)
   integer,             intent(in)    :: ioncid   !< The IONC data set id.
   integer,             intent(in)    :: meshid   !< The mesh id in the specified data set.
   integer,             intent(in)    :: idim     !< Mesh dimension (enumerator value), e.g. mdim_face, mdim_node
   integer,             intent(out)   :: dimid    !< Dimension ID
   integer                            :: ierr     !< Result status, ionc_noerr if successful.
   ierr  = IONC_NOERR
   dimid = datasets(ioncid)%ug_file%meshids(meshid)%dimids(idim)
end function ionc_get_dimid

!> Gets the number of mesh from a data set.
function ionc_get_mesh_count(ioncid, nmesh) result(ierr)
   integer,             intent(in)    :: ioncid  !< The IONC data set id.
   integer,             intent(  out) :: nmesh   !< Number of meshes.
   integer                            :: ierr    !< Result status, ionc_noerr if successful.

   ! TODO: AvD: some error handling if ioncid is wrong
   ierr = ug_get_mesh_count(datasets(ioncid)%ncid, nmesh)
end function ionc_get_mesh_count

!> Gets the name of the mesh topology variable in an open dataset.
function ionc_get_mesh_name(ioncid, meshid, meshname) result(ierr)
   integer,             intent(in)    :: ioncid   !< The IONC data set id.
   integer,             intent(in)    :: meshid   !< The mesh id in the specified data set.
   character(len=*),    intent(  out) :: meshname !< The name of the mesh topology variable.
   integer                            :: ierr     !< Result status, ionc_noerr if successful.

   ierr = ug_get_mesh_name(datasets(ioncid)%ncid, datasets(ioncid)%ug_file%meshids(meshid), meshname)
end function ionc_get_mesh_name

!> Gets the name of the network variable in an open dataset.
function ionc_get_network_name(ioncid, networkid, networkname) result(ierr)
   integer,             intent(in)    :: ioncid      !< The IONC data set id.
   integer,             intent(in)    :: networkid   !< The network id in the specified data set.
   character(len=*),    intent(  out) :: networkname !< The name of the network topology variable.
   integer                            :: ierr        !< Result status, ionc_noerr if successful.

   ierr = ug_get_network_name(datasets(ioncid)%ncid, datasets(ioncid)%ug_file%netids(networkid), networkname)
end function ionc_get_network_name

!> Gets the dimension of the mesh topology for the specified mesh in a UGRID data set.
function ionc_get_topology_dimension(ioncid, meshid, dim) result(ierr)
   integer,             intent(in)    :: ioncid  !< The IONC data set id.
   integer,             intent(in)    :: meshid  !< The mesh id in the specified data set.
   integer,             intent(  out) :: dim     !< The dimension of the mesh topology in case of a UGRID file.
   integer                            :: ierr    !< Result status, ionc_noerr if successful.

   ! TODO: AvD: some error handling if ioncid is wrong
   if (datasets(ioncid)%iconvtype /= IONC_CONV_UGRID) then
      ierr = IONC_ENOTAVAILABLE
      goto 999
   end if

   ierr = ug_get_topology_dimension(datasets(ioncid)%ncid, datasets(ioncid)%ug_file%meshids(meshid), dim)

   ! Successful
   return

999 continue
   ! Some error (status was set earlier)
   return

end function ionc_get_topology_dimension


function ionc_put_meshgeom(ioncid, meshgeom, meshid, networkid, meshname, networkName) result(ierr)

   use m_ug_meshgeom
   integer,             intent(in   )                       :: ioncid        !< The IONC data set id.
   type(t_ug_meshgeom)                                      :: meshgeom      !< Structure in which all mesh geometry is be stored.
   integer,             intent(inout)                       :: meshid        !< The mesh id in the specified data set.
   integer,             intent(inout)                       :: networkid     !< The network id in the specified data set.
   character(len=*)                                         :: meshname      !< The mesh name
   character(len=*)                                         :: networkName   !< The network name
   integer                                                  :: ierr          !< Result status, ionc_noerr if successful.

   ! Locals (default values)

   if (len_trim(meshname).gt.0) then
      !adds a meshids structure
      ierr = ug_add_mesh(datasets(ioncid)%ncid, datasets(ioncid)%ug_file, meshid)
      ! set the meshname
      datasets(ioncid)%ug_file%meshnames(meshid) = meshname
      meshgeom%meshname = meshname
   endif

   if (len_trim(networkName).gt.0) then
      ! allocate add a meshids
      ierr = ug_add_network(datasets(ioncid)%ncid, datasets(ioncid)%ug_file, networkid)
      ! set the network name
      datasets(ioncid)%ug_file%networksnames(networkid) = networkName
   endif

   !this call writes mesh and network data contained in meshgeom
   ierr = ionc_write_mesh_struct(ioncid, datasets(ioncid)%ug_file%meshids(meshid), datasets(ioncid)%ug_file%netids(networkid), meshgeom)

end function ionc_put_meshgeom


function ionc_put_meshgeom_v1(ioncid, meshgeom, meshid, networkid) result(ierr)

   integer,             intent(in   )                       :: ioncid        !< The IONC data set id.
   type(t_ug_meshgeom)                                      :: meshgeom      !< Structure in which all mesh geometry is be stored.
   integer,             intent(inout)                       :: meshid        !< The mesh id in the specified data set.
   integer,             intent(inout)                       :: networkid     !< The network id in the specified data set.
   integer                                                  :: ierr          !< Result status, ionc_noerr if successful.

   ! Locals (default values)

   if (len_trim(meshgeom%meshname).gt.0) then
      !adds a meshids structure
      ierr = ug_add_mesh(datasets(ioncid)%ncid, datasets(ioncid)%ug_file, meshid)
      ! set the meshname
      datasets(ioncid)%ug_file%meshnames(meshid) = meshgeom%meshname
   endif

   if (networkid.gt.0) then
      ierr = ionc_write_mesh_struct(ioncid, datasets(ioncid)%ug_file%meshids(meshid), datasets(ioncid)%ug_file%netids(networkid), meshgeom, datasets(ioncid)%ug_file%networksnames(networkid) )
   else
      ierr = ionc_write_mesh_struct(ioncid, datasets(ioncid)%ug_file%meshids(meshid), datasets(ioncid)%ug_file%netids(networkid), meshgeom)
   endif



end function ionc_put_meshgeom_v1


function ionc_put_network(ioncid, networkgeom, networkid) result(ierr)

   integer,             intent(in   )                       :: ioncid        !< The IONC data set id.
   type(t_ug_meshgeom)                                      :: networkgeom   !< Structure in which all mesh geometry is be stored.
   integer,             intent(inout)                       :: networkid     !< The network id in the specified data set.
   integer                                                  :: ierr          !< Result status, ionc_noerr if successful.

   ! Locals (default values)
   type(t_ug_mesh)                                          :: meshids

   character(len=ug_idsLen), allocatable                     :: nnodeids(:), nbranchids(:)
   character(len=ug_idsLongNamesLen), allocatable            :: nnodelongnames(:), nbranchlongnames(:)

   integer :: ierrLocal

   allocate(nnodeids(networkgeom%nnodes))
   allocate(nnodelongnames(networkgeom%nnodes))
   allocate(nbranchids(networkgeom%nbranches))
   allocate(nbranchlongnames(networkgeom%nbranches))
   nnodeids = networkgeom%nnodeids
   nnodelongnames = networkgeom%nnodelongnames
   nbranchids = networkgeom%nbranchids
   nbranchlongnames = networkgeom%nbranchlongnames

   ierr = ug_add_network(datasets(ioncid)%ncid, datasets(ioncid)%ug_file, networkid)
   ! set the network name
   datasets(ioncid)%ug_file%networksnames(networkid) = networkgeom%meshname

   !this call writes mesh and network data contained in meshgeom
   ierr = ug_write_mesh_struct(ncid = datasets(ioncid)%ncid, meshids = meshids, networkids = datasets(ioncid)%ug_file%netids(networkid), crs = datasets(ioncid)%crs, &
      meshgeom = networkgeom, nnodeids = nnodeids, nnodelongnames = nnodelongnames, nbranchids = nbranchids, nbranchlongnames = nbranchlongnames, network1dname = networkgeom%meshname)

end function ionc_put_network


!> Reads the actual mesh geometry and network from the specified mesh in a IONC/UGRID dataset.
!! Can read separate parts, such as dimension AND/OR all coordinate arrays + connectivity tables AND/OR network geometry
function ionc_get_meshgeom(ioncid, meshid, networkid, meshgeom, start_index, includeArrays, nbranchids, nbranchlongnames, nnodeids, nnodelongnames, nodeids, nodelongnames, network1dname, mesh1dname) result(ierr)
   integer,             intent(in   ) :: ioncid        !< The IONC data set id.
   integer,             intent(in   ) :: meshid        !< The mesh id in the specified data set.
   integer                            :: networkid     !< The mesh id in the specified data set.
   type(t_ug_meshgeom), intent(inout) :: meshgeom      !< Structure in which all mesh geometry will be stored.
   integer                            :: ierr          !< Result status, ionc_noerr if successful.
   type(t_ug_network)                 :: netid

   !Optional variables
   logical, optional,   intent(in)                          :: includeArrays !< (optional) Whether or not to include coordinate arrays and connectivity tables. Default: .false., i.e., dimension counts only.
   integer, optional,   intent(in)                          :: start_index   !< (optional) The start index
   character(len=ug_idsLen), allocatable, optional          :: nbranchids(:), nnodeids(:), nodeids(:)
   character(len=ug_idsLongNamesLen), allocatable, optional :: nbranchlongnames(:), nnodelongnames(:), nodelongnames(:)
   character(len=*), optional, intent(inout)                :: network1dname, mesh1dname


   !locals
   logical :: includeNames
   !deduced start index
   integer :: ded_start_index

   ! TODO: AvD: some error handling if ioncid or meshid is wrong
   if (datasets(ioncid)%iconvtype /= IONC_CONV_UGRID) then
      ierr = IONC_ENOTAVAILABLE
      goto 999
   end if

   ! check for network 1d if a valid meshid and an invalid networkid is given
   if (meshid > 0 .and. networkid <= 0) then
      networkid = -1
      ierr = ug_get_network_id_from_mesh_id(datasets(ioncid)%ncid, datasets(ioncid)%ug_file%meshids(meshid), datasets(ioncid)%ug_file, networkid)
   endif

   !requested by the client
   if(present(start_index)) then
      ded_start_index = start_index
   else
      ded_start_index = 1 !As requested by fortran applications
   endif

   includeNames = .false.
   includeNames = present(nbranchids) .and. present(nbranchlongnames) .and. present(nnodeids) .and. present(nnodelongnames).and. &
      present(nodeids) .and. present(nodelongnames) .and. present(network1dname) .and. present(mesh1dname)

   ! we have the following switches
   ! 1. includeArrays is present/not present
   ! 2. includeNames is true/false
   ! 3. networkid is valid(>0)/invalid(<=0)
   ! 4. meshid is valid(>0)/invalid(<=0)
   if (present(includeArrays).and. meshid >0 .and. networkid >0 .and. includeNames) then
      ierr = ug_get_meshgeom(&
      ncid             = datasets(ioncid)%ncid,&
      meshgeom         = meshgeom,&
      start_index      = ded_start_index,&
      meshids          = datasets(ioncid)%ug_file%meshids(meshid),&
      netid            = datasets(ioncid)%ug_file%netids(networkid),&
      includeArrays    = includeArrays,&
      nbranchids       = nbranchids,&
      nbranchlongnames = nbranchlongnames,&
      nnodeids         = nnodeids,&
      nnodelongnames   = nnodelongnames,&
      nodeids          = nodeids,&
      nodelongnames    = nodelongnames,&
      network1dname    = network1dname,&
      mesh1dname       = mesh1dname)
   else if (present(includeArrays).and. meshid <=0 .and. networkid >0 .and. includeNames) then
      ierr = ug_get_meshgeom(&
      ncid             = datasets(ioncid)%ncid,&
      meshgeom         = meshgeom,&
      start_index      = ded_start_index,&
      netid            = datasets(ioncid)%ug_file%netids(networkid),&
      includeArrays    = includeArrays,&
      nbranchids       = nbranchids,&
      nbranchlongnames = nbranchlongnames,&
      nnodeids         = nnodeids,&
      nnodelongnames   = nnodelongnames,&
      nodeids          = nodeids,&
      nodelongnames    = nodelongnames,&
      network1dname    = network1dname)
   else if (present(includeArrays).and. meshid > 0 .and. networkid > 0 .and. (.not.includeNames)) then
      ierr = ug_get_meshgeom(&
      ncid             = datasets(ioncid)%ncid,&
      meshgeom         = meshgeom, &
      start_index      = ded_start_index,&
      meshids          = datasets(ioncid)%ug_file%meshids(meshid),&
      netid            = datasets(ioncid)%ug_file%netids(networkid),&
      includeArrays    = includeArrays)
   else if (present(includeArrays).and. meshid > 0 .and. networkid <= 0 .and. (.not.includeNames)) then
      ierr = ug_get_meshgeom(&
      ncid             = datasets(ioncid)%ncid,&
      meshgeom         = meshgeom,&
      start_index      = ded_start_index,&
      meshids          = datasets(ioncid)%ug_file%meshids(meshid),&
      includeArrays    = includeArrays)
   else if (present(includeArrays).and. meshid <= 0 .and. networkid > 0 .and. (.not.includeNames)) then
      ierr = ug_get_meshgeom(&
      ncid             = datasets(ioncid)%ncid,&
      meshgeom         = meshgeom,&
      start_index      = ded_start_index,&
      netid            = datasets(ioncid)%ug_file%netids(networkid),&
      includeArrays    = includeArrays)
!
! Ask only for dimensions
!
   else if (.not.present(includeArrays).and. meshid > 0 .and. networkid > 0 .and. (.not.includeNames)) then
      ierr = ug_get_meshgeom(&
      ncid             = datasets(ioncid)%ncid,&
      meshgeom         = meshgeom, &
      start_index      = ded_start_index,&
      meshids          = datasets(ioncid)%ug_file%meshids(meshid),&
      netid            = datasets(ioncid)%ug_file%netids(networkid))
   else if (.not.present(includeArrays).and. meshid > 0 .and. networkid <= 0 .and. (.not.includeNames)) then
      ierr = ug_get_meshgeom(&
      ncid             = datasets(ioncid)%ncid,&
      meshgeom         = meshgeom,&
      start_index      = ded_start_index,&
      meshids          = datasets(ioncid)%ug_file%meshids(meshid))
   else if (.not.present(includeArrays).and. meshid <= 0 .and. networkid > 0 .and. (.not.includeNames)) then
      ierr = ug_get_meshgeom(&
      ncid             = datasets(ioncid)%ncid,&
      meshgeom         = meshgeom,&
      start_index      = ded_start_index,&
      netid            = datasets(ioncid)%ug_file%netids(networkid) )
   else
      ierr = IONC_ENOTAVAILABLE
      goto 999
   endif

   ! Successful
   return

999 continue
   ! Some error (status was set earlier)
   return

end function ionc_get_meshgeom


!> Gets the number of nodes in a single mesh from a data set.
function ionc_get_node_count(ioncid, meshid, nnode) result(ierr)
   integer,             intent(in)    :: ioncid  !< The IONC data set id.
   integer,             intent(in)    :: meshid  !< The mesh id in the specified data set.
   integer,             intent(  out) :: nnode   !< Number of nodes.
   integer                            :: ierr    !< Result status, ionc_noerr if successful.

   ! TODO: AvD: some error handling if ioncid or meshid is wrong
   ierr = ug_inquire_dimension(datasets(ioncid)%ncid, datasets(ioncid)%ug_file%meshids(meshid), UG_LOC_NODE, nnode)
end function ionc_get_node_count


!> Gets the number of edges in a single mesh from a data set.
function ionc_get_edge_count(ioncid, meshid, nedge) result(ierr)
   integer,             intent(in)    :: ioncid  !< The IONC data set id.
   integer,             intent(in)    :: meshid  !< The mesh id in the specified data set.
   integer,             intent(  out) :: nedge   !< Number of edges.
   integer                            :: ierr    !< Result status, ionc_noerr if successful.

   ! TODO: AvD: some error handling if ioncid or meshid is wrong
   ierr = ug_inquire_dimension(datasets(ioncid)%ncid, datasets(ioncid)%ug_file%meshids(meshid), UG_LOC_EDGE, nedge)
end function ionc_get_edge_count


!> Gets the number of faces in a single mesh from a data set.
function ionc_get_face_count(ioncid, meshid, nface) result(ierr)
   integer,             intent(in)    :: ioncid  !< The IONC data set id.
   integer,             intent(in)    :: meshid  !< The mesh id in the specified data set.
   integer,             intent(  out) :: nface   !< Number of faces.
   integer                            :: ierr    !< Result status, ionc_noerr if successful.

   ! TODO: AvD: some error handling if ioncid or meshid is wrong
   ierr = ug_inquire_dimension(datasets(ioncid)%ncid, datasets(ioncid)%ug_file%meshids(meshid), UG_LOC_FACE, nface)
end function ionc_get_face_count


!> Gets the maximum number of nodes for any face in a single mesh from a data set.
function ionc_get_max_face_nodes(ioncid, meshid, nmaxfacenodes) result(ierr)
   integer,             intent(in)    :: ioncid        !< The IONC data set id.
   integer,             intent(in)    :: meshid        !< The mesh id in the specified data set.
   integer,             intent(  out) :: nmaxfacenodes !< Number of faces.
   integer                            :: ierr          !< Result status, ionc_noerr if successful.

   ! TODO: AvD: some error handling if ioncid or meshid is wrong
   ierr = ug_inquire_dimension(datasets(ioncid)%ncid, datasets(ioncid)%ug_file%meshids(meshid), UG_DIM_MAXFACENODES, nmaxfacenodes)
end function ionc_get_max_face_nodes


!> Gets the x,y coordinates for all nodes in a single mesh from a data set.
function ionc_get_node_coordinates(ioncid, meshid, xarr, yarr) result(ierr)
   integer,             intent(in)  :: ioncid  !< The IONC data set id.
   integer,             intent(in)  :: meshid  !< The mesh id in the specified data set.
   double precision,    intent(out) :: xarr(:) !< Array for storing x-coordinates
   double precision,    intent(out) :: yarr(:) !< Array for storing y-coordinates
   integer                          :: ierr    !< Result status, ionc_noerr if successful.

   ierr = ug_get_node_coordinates(datasets(ioncid)%ncid, datasets(ioncid)%ug_file%meshids(meshid), xarr, yarr)
end function ionc_get_node_coordinates


!> Puts the x,y coordinates for all nodes in a single mesh from a data set.
function ionc_put_node_coordinates(ioncid, meshid, xarr, yarr) result(ierr)
   integer,             intent(in)  :: ioncid  !< The IONC data set id.
   integer,             intent(in)  :: meshid  !< The mesh id in the specified data set.
   double precision,    intent(in)  :: xarr(:) !< Array containing the x-coordinates
   double precision,    intent(in)  :: yarr(:) !< Array containing the y-coordinates
   integer                          :: ierr    !< Result status, ionc_noerr if successful.

   ierr = ug_put_node_coordinates(datasets(ioncid)%ncid, datasets(ioncid)%ug_file%meshids(meshid), xarr, yarr)

end function ionc_put_node_coordinates


!> Gets the edge_faces connectivity table for all faces in the specified mesh.
!! The output edge_faces array is supposed to be of exact correct size already.
function ionc_get_edge_faces(ioncid, meshid, edge_faces, fillvalue, startIndex) result(ierr)
   integer, intent(in)    :: ioncid  !< The IONC data set id.
   integer, intent(in)    :: meshid  !< The mesh id in the specified data set.
   integer, intent(  out) :: edge_faces(:,:)  !< Array to the face-node connectivity table.
   integer, intent(  out) :: fillvalue  !< Scalar for getting the fill value parameter for the requested variable.
   integer                :: ierr  !< Result status, ionc_noerr if successful.
   integer, intent(in)    :: startIndex      !< The start index the caller asks for

   ierr = ug_get_edge_faces(datasets(ioncid)%ncid, datasets(ioncid)%ug_file%meshids(meshid), edge_faces, fillvalue, startIndex)
end function ionc_get_edge_faces


!> Gets the edge_nodes connectivity table for all edges in the specified mesh.
!! The output edge_nodes array is supposed to be of exact correct size already.
function ionc_get_edge_nodes(ioncid, meshid, edge_nodes, start_index) result(ierr)
   integer, intent(in)    :: ioncid  !< The IONC data set id.
   integer, intent(in)    :: meshid  !< The mesh id in the specified data set.
   integer, intent(  out) :: edge_nodes(:,:)  !< Array to the face-node connectivity table.
   integer                :: ierr  !< Result status, ionc_noerr if successful.
   integer                :: start_index !< The requested start index

   ierr = ug_get_edge_nodes(datasets(ioncid)%ncid, datasets(ioncid)%ug_file%meshids(meshid), edge_nodes, start_index)
end function ionc_get_edge_nodes

!> Gets the x,y coordinates (representative centre) for all faces in a single mesh from a data set.
function ionc_get_face_coordinates(ioncid, meshid, xarr, yarr) result(ierr)
   integer,             intent(in)  :: ioncid  !< The IONC data set id.
   integer,             intent(in)  :: meshid  !< The mesh id in the specified data set.
   double precision,    intent(out) :: xarr(:) !< Array for storing x-coordinates
   double precision,    intent(out) :: yarr(:) !< Array for storing y-coordinates
   integer                          :: ierr    !< Result status, ionc_noerr if successful.

   ierr = ug_get_face_coordinates(datasets(ioncid)%ncid, datasets(ioncid)%ug_file%meshids(meshid), xarr, yarr)
end function ionc_get_face_coordinates


!> Puts the x,y coordinates for all faces in a single mesh from a data set.
function ionc_put_face_coordinates(ioncid, meshid, xarr, yarr) result(ierr)
   integer,             intent(in)  :: ioncid  !< The IONC data set id.
   integer,             intent(in)  :: meshid  !< The mesh id in the specified data set.
   double precision,    intent(in)  :: xarr(:) !< Array containing the x-coordinates
   double precision,    intent(in)  :: yarr(:) !< Array containing the y-coordinates
   integer                          :: ierr    !< Result status, ionc_noerr if successful.

   ierr = ug_put_face_coordinates(datasets(ioncid)%ncid, datasets(ioncid)%ug_file%meshids(meshid), xarr, yarr)

end function ionc_put_face_coordinates


!> Gets the face-edge connectivity table for all faces in the specified mesh.
!! The output face_edges array is supposed to be of exact correct size already.
function ionc_get_face_edges(ioncid, meshid, face_edges, fillvalue, startIndex) result(ierr)
   integer, intent(in)    :: ioncid  !< The IONC data set id.
   integer, intent(in)    :: meshid  !< The mesh id in the specified data set.
   integer, intent(  out) :: face_edges(:,:) !< Array to the face-node connectivity table.
   integer, intent(  out) :: fillvalue !< Scalar for getting the fill value parameter for the requested variable.
   integer, intent(in)    :: startIndex      !< The start index the caller asks for
   integer                :: ierr    !< Result status, ionc_noerr if successful.

   ierr = ug_get_face_edges(datasets(ioncid)%ncid, datasets(ioncid)%ug_file%meshids(meshid), face_edges, fillvalue, startIndex)
end function ionc_get_face_edges


!> Gets the face-node connectivity table for all faces in the specified mesh.
!! The output face_nodes array is supposed to be of exact correct size already.
function ionc_get_face_nodes(ioncid, meshid, face_nodes, fillvalue, startIndex) result(ierr)
   integer, intent(in)    :: ioncid          !< The IONC data set id.
   integer, intent(in)    :: meshid          !< The mesh id in the specified data set.
   integer, intent(  out) :: face_nodes(:,:) !< Array to the face-node connectivity table.
   integer, intent(  out) :: fillvalue       !< Scalar for getting the fill value parameter for the requested variable.
   integer, intent(in)    :: startIndex      !< The start index the caller asks for
   integer                :: ierr            !< Result status, ionc_noerr if successful.

   ierr = ug_get_face_nodes(datasets(ioncid)%ncid, datasets(ioncid)%ug_file%meshids(meshid), face_nodes, fillvalue, startIndex)
end function ionc_get_face_nodes

!> Gets the coordinate system from a data set.
function ionc_get_coordinate_reference_system(ioncid, crs) result(ierr)
   integer,             intent(in)    :: ioncid  !< The IONC data set id.
   type(t_crs),         intent(out)   :: crs     !< The crs
   integer                            :: ierr    !< Result status, ionc_noerr if successful.
   ierr = nf90_noerr
   crs = datasets(ioncid)%crs
end function ionc_get_coordinate_reference_system

!> Gets the epsg code from a data set.
function ionc_get_epsg_code(ioncid, epsg_code) result(ierr)
   integer,             intent(in)    :: ioncid  !< The IONC data set id.
   integer,             intent(  out) :: epsg_code   !< Number of epsg.
   integer                            :: ierr    !< Result status, ionc_noerr if successful.

   ! TODO: AvD: some error handling if ioncid is wrong
   ierr = nf90_noerr
   epsg_code = datasets(ioncid)%crs%epsg_code
end function ionc_get_epsg_code

!> Set the coordinate system of a data set: this is needed to pass the metadata read with ionc_get_coordinate_reference_system.
!> Note: currently io_netcdf only supports ONE crs for each file, not one for each grid.
function ionc_set_coordinate_reference_system(ioncid, crs) result(ierr)
   integer,             intent(in)    :: ioncid  !< The IONC data set id.
   type(t_crs),         intent(in)    :: crs     !< The crs
   integer                            :: ierr    !< Result status, ionc_noerr if successful.
   ierr = nf90_noerr
   datasets(ioncid)%crs = crs
end function ionc_set_coordinate_reference_system

!> Get the meta data from the file/data set.
function ionc_get_meta_data(ioncid, meta) result(ierr)
   integer, intent(in)          :: ioncid    !< The IONC data set id.
   type(t_ug_meta), intent(out) :: meta      !< The meta data of the dataset
   integer                      :: ierr      !< Result status, ionc_noerr if successful.

   ierr = IONC_NOERR
   if (ioncid < 1 .or. ioncid > ndatasets) then
      ierr = IONC_EBADID
      goto 999
   end if

   meta%institution = ' '
   meta%source      = ' '
   meta%references  = ' '
   meta%version     = ' '
   meta%modelname   = ' '

   ierr = nf90_get_att(datasets(ioncid)%ncid, nf90_global, 'institution', meta%institution)
   if (ierr /= nf90_noerr) then
      goto 999
   end if
   ierr = nf90_get_att(datasets(ioncid)%ncid, nf90_global, 'source',     meta%source)
   if (ierr /= nf90_noerr) then
      goto 999
   end if
   ierr = nf90_get_att(datasets(ioncid)%ncid, nf90_global, 'references', meta%references)
   if (ierr /= nf90_noerr) then
      goto 999
   end if
   ierr = nf90_get_att(datasets(ioncid)%ncid, nf90_global, 'version',    meta%version)
   if (ierr /= nf90_noerr) then
      goto 999
   end if
   ierr = nf90_get_att(datasets(ioncid)%ncid, nf90_global, 'modelname',  meta%modelname)
   if (ierr /= nf90_noerr) then
      goto 999
   end if

   ! Successful
   return

999 continue
   ! Some error (status was set earlier)
end function ionc_get_meta_data

!> Returns the number of variables that are available in the specified dataset on the specified mesh.
!! The location type allows to select on specific topological mesh locations
!! (UGRID-compliant, so UG_LOC_FACE/EDGE/NODE/ALL2D).
function ionc_get_var_count(ioncid, meshid, iloctype, nvar) result(ierr)
   integer,             intent(in)    :: ioncid   !< The IONC data set id.
   integer,             intent(in)    :: meshid   !< The mesh id in the specified data set.
   integer,             intent(in)    :: iloctype !< The topological location on which to select data (UGRID-compliant, so UG_LOC_FACE/EDGE/NODE/ALL2D).
   integer,             intent(  out) :: nvar     !< Number of variables defined on the requested location type+mesh+dataset.
   integer                            :: ierr     !< Result status, ionc_noerr if successful.

   ! TODO: AvD: some error handling if ioncid or meshid is wrong
   ierr = ug_get_var_count(datasets(ioncid)%ncid, datasets(ioncid)%ug_file%meshids(meshid), iloctype, nvar)

end function ionc_get_var_count

!> Returns the number of variables that are available on the specified mesh.
!! If meshtype = 0, then the specified mesh is a 1D or 2d mesh. If meshtype = 1, then it is a mesh contact.
!! If jaids > 0, then also return the ids of these variables.
function ionc_get_var_total_count(ioncid, meshid, meshtype, jaids, nvar, varids) result(ierr)
   integer,             intent(in   ) :: ioncid   !< The IONC data set id.
   integer,             intent(in   ) :: meshid   !< The mesh id or contact id in the specified data set.
   integer,             intent(in   ) :: meshtype !< mesh (0) or mesh contact (1)
   integer,             intent(in   ) :: jaids    !< Returns variable ids (1) or not (0)
   integer,             intent(  out) :: nvar     !< Number of variables defined on the requested mesh or contact.
   integer, optional,   intent(  out) :: varids(:)!< Array to store the variable ids in.
   integer                            :: ierr     !< Result status, ionc_noerr if successful.

   if (meshtype == 0) then ! A 1D or 2D mesh
      ierr = ug_get_var_total_count(datasets(ioncid)%ncid, mids=datasets(ioncid)%ug_file%meshids(meshid), jaid=jaids, nvars=nvar, id_vars=varids)
   else if (meshtype == 1) then ! A mesh contact
      ierr = ug_get_var_total_count(datasets(ioncid)%ncid, cids=datasets(ioncid)%ug_file%contactids(meshid), jaid=jaids, nvars=nvar, id_vars=varids)
   end if

end function ionc_get_var_total_count

!> Checks if the dimension is on a specified mesh, given the dimension id.
function ionc_check_dim_on_a_mesh(ioncid, meshid, topodim, id) result(ja)
   integer,             intent(in   ) :: ioncid    !< The IONC data set id.
   integer,             intent(in   ) :: meshid    !< The mesh id in the specified data set.
   integer,             intent(in   ) :: topodim   !< Topology dimention of the specified mesh
   integer,             intent(in   ) :: id        !< Id of the dimension that to be checked if this dimension is on the specified mesh
   integer                            :: ja        !< Result, 1 if it is true
   integer :: i, idtmp
   type(t_ug_mesh) :: meshids
   type(t_ug_file) :: ug_file
   ja = 0
   ug_file = datasets(ioncid)%ug_file
   meshids = ug_file%meshids(meshid)
   do i = mdim_start+1, mdim_end-1
      idtmp = meshids%dimids(i)
      if (idtmp == id) then
         ja = 1
         exit
      end if
   end do

   if (ja == 0 .and. topodim == 1) then
      do i = ntdim_start+1, ntdim_end-1
         idtmp = ug_file%netids(meshid)%dimids(i)
         if (idtmp == id) then
            ja = 1
            exit
         end if
      end do
   end if
end function ionc_check_dim_on_a_mesh

!> Gets a list of variable IDs that are available in the specified dataset on the specified mesh.
!! The location type allows to select on specific topological mesh locations
!! (UGRID-compliant, so UG_LOC_FACE/EDGE/NODE/ALL2D)
function ionc_inq_varids(ioncid, meshid, iloctype, varids, nvar) result(ierr)
   integer,             intent(in)    :: ioncid    !< The IONC data set id.
   integer,             intent(in)    :: meshid    !< The mesh id in the specified data set.
   integer,             intent(in)    :: iloctype  !< The topological location on which to select data (UGRID-compliant, so UG_LOC_FACE/EDGE/NODE/ALL2D).
   integer,             intent(  out) :: varids(:) !< Array to store the variable ids in.
   integer,             intent(  out) :: nvar      !< Number of variables found/stored in array.
   integer                            :: ierr      !< Result status, ionc_noerr if successful.


   ! TODO: AvD: some error handling if ioncid or meshid is wrong
   ierr = ug_inq_varids(datasets(ioncid)%ncid, datasets(ioncid)%ug_file%meshids(meshid), iloctype, varids, nvar)

end function ionc_inq_varids


!> Gets the variable ID for a data variable that is defined in the specified dataset on the specified mesh.
!! The variable is searched based on variable name (without any "meshnd_" prefix), and which :mesh it is defined on.
function ionc_inq_varid(ioncid, meshid, varname, varid) result(ierr)
   integer,             intent(in)    :: ioncid   !< The IONC data set id.
   integer,             intent(in)    :: meshid   !< The mesh id in the specified data set.
   character(len=*),    intent(in)    :: varname  !< The name of the variable to be found. Should be without any "meshnd_" prefix.
   integer,             intent(  out) :: varid    !< The resulting variable id, if found.
   integer                            :: ierr     !< Result status, ionc_noerr if successful.


   ! TODO: AvD: some error handling if ioncid or meshid is wrong
   ierr = ug_inq_varid(datasets(ioncid)%ncid, datasets(ioncid)%ug_file%meshids(meshid), varname, varid)

end function ionc_inq_varid


!> Gets the variable ID for the variable in the specified dataset on the specified mesh,
!! that also has the specified value for its ':standard_name' attribute, and
!! is defined on the specified topological mesh location (UGRID-compliant, so UG_LOC_FACE/EDGE/NODE/ALL2D)
function ionc_inq_varid_by_standard_name(ioncid, meshid, iloctype, stdname, varid) result(ierr)
   integer,             intent(in)    :: ioncid   !< The IONC data set id.
   integer,             intent(in)    :: meshid   !< The mesh id in the specified data set.
   integer,             intent(in)    :: iloctype !< The topological location on which to select data (UGRID-compliant, so UG_LOC_FACE/EDGE/NODE/ALL2D).
   character(len=*),    intent(in)    :: stdname  !< The standard_name value that is searched for.
   integer,             intent(  out) :: varid    !< The resulting variable id, if found.
   integer                            :: ierr     !< Result status, ionc_noerr if successful.


   ! TODO: AvD: some error handling if ioncid or meshid is wrong
   ierr = ug_inq_varid_by_standard_name(datasets(ioncid)%ncid, datasets(ioncid)%ug_file%meshids(meshid), iloctype, stdname, varid)

end function ionc_inq_varid_by_standard_name


!> Gets the numerical values for a named variable in the specified dataset on the specified mesh.
!! The location type allows to select the specific topological mesh location.
!! (UGRID-compliant, so UG_LOC_FACE/EDGE/NODE/ALL2D)
function ionc_get_var_1D_EightByteReal(ioncid, meshid, iloctype, varname, values, fillvalue) result(ierr) ! TODO (?): AvD: support start, count, stride, map
   integer,             intent(in)    :: ioncid    !< The IONC data set id.
   integer,             intent(in)    :: meshid    !< The mesh id in the specified data set.
   integer,             intent(in)    :: iloctype  !< The topological location on which to select data (UGRID-compliant, so UG_LOC_FACE/EDGE/NODE/ALL2D).
   character(len=*),    intent(in)    :: varname   !< The name of the variable to be found. Should be without any "meshnd_" prefix.
   real (kind = kind(1d0)), &
      dimension(:),     intent(inout) :: values    !< Array to store the values in.
   real (kind = kind(1d0)), intent(  out) :: fillvalue !< Scalar for getting the fill value parameter for the requested variable.
   integer                            :: ierr      !< Result status, ionc_noerr if successful.

   integer :: varid

   ! TODO: AvD: some error handling if ioncid or meshid is wrong
   ! TODO: AvD: Verify that location type is correct.
   ierr = ionc_inq_varid(ioncid, meshid, varname, varid)
   if (ierr /= ionc_noerr) then
      goto 999
   end if

   ierr = nf90_get_var(datasets(ioncid)%ncid, varid, values)
   if (ierr /= nf90_noerr) then
      goto 999
   end if

   ierr = nf90_get_att(datasets(ioncid)%ncid, varid, '_FillValue', fillvalue)
   if (ierr /= nf90_noerr) then
      fillvalue = nf90_fill_real8
   end if

   ierr = UG_NOERR
   return ! Return with success

999 continue
    ! Some error (status was set earlier)

end function ionc_get_var_1D_EightByteReal


!> Gets the characters values for a named variable in the specified dataset on the specified mesh.
!! The type is not needed because the variable is retrived from its full name
function ionc_get_var_chars(ioncid, meshid, varname, values) result(ierr)
   integer,             intent(in)          :: ioncid         !< The IONC data set id.
   integer,             intent(in)          :: meshid         !< The mesh id in the specified data set.
   character(len=*),    intent(in)          :: varname        !< The name of the variable to be found. Should be without any "meshnd_" prefix.
   character(len=*),    intent(inout)       :: values(:)      !< Array to store the values in.
   integer                                  :: ierr           !< Result status, ionc_noerr if successful.

   integer :: varid

   ierr = ionc_inq_varid(ioncid, meshid, varname, varid)
   if (ierr /= ionc_noerr) then
      goto 999
   end if

   ierr = nf90_get_var(datasets(ioncid)%ncid, varid, values)
   if (ierr /= nf90_noerr) then
      goto 999
   end if

   ierr = UG_NOERR
   return ! Return with success

999 continue
    ! Some error (status was set earlier)

end function ionc_get_var_chars


!> Puts the values for a named variable into the specified dataset on the specified mesh.
!! NOTE: Assumes that the variable already exists in the file (i.e., needs no def_var anymore).
!! The location type allows to select the specific topological mesh location.
!! (UGRID-compliant, so UG_LOC_FACE/EDGE/NODE/ALL2D)
function ionc_put_var_1D_EightByteReal(ioncid, meshid, iloctype, varname, values) result(ierr) ! TODO (?): AvD: support start, count, stride, map
   integer,             intent(in)    :: ioncid    !< The IONC data set id.
   integer,             intent(in)    :: meshid    !< The mesh id in the specified data set.
   integer,             intent(in)    :: iloctype  !< The topological location on which to select data (UGRID-compliant, so UG_LOC_FACE/EDGE/NODE/ALL2D).
   character(len=*),    intent(in)    :: varname   !< The name of the variable to be found. Should be without any "meshnd_" prefix.
   real (kind = kind(1d0)), &
      dimension(:),     intent(in)    :: values    !< Array with the values to be written.
   integer                            :: ierr      !< Result status, ionc_noerr if successful.

   integer :: varid

   ! TODO: AvD: some error handling if ioncid or meshid is wrong
   ! TODO: AvD: Verify that location type is correct.
   ierr = ionc_inq_varid(ioncid, meshid, varname, varid)

   if (ierr /= ionc_noerr) then
      goto 999
   end if

   ierr = nf90_put_var(datasets(ioncid)%ncid, varid, values)
   if (ierr /= nf90_noerr) then
      goto 999
   end if

   ierr = UG_NOERR
   return ! Return with success

999 continue
    ! Some error (status was set earlier)

end function ionc_put_var_1D_EightByteReal

!> Puts the character values for a named variable into the specified dataset on the specified mesh.
function ionc_put_var_chars(ioncid, meshid, varname, values) result(ierr)

    integer,             intent(in)    :: ioncid           !< The IONC data set id.
    integer,             intent(in)    :: meshid           !< The mesh id in the specified data set.
    character(len=*),    intent(in)    :: varname          !< The name of the variable to be found. Should be without any "meshnd_" prefix.
    character(len=*),    intent(in)    :: values(:)        !< Array with the charter values to be written.
    integer                            :: ierr, varid      !< Result status, ionc_noerr if successful.

    ierr = UG_SOMEERR
   ierr = ionc_inq_varid(ioncid, meshid, varname, varid)

   if (ierr /= ionc_noerr) then
      goto 999
   end if

   ierr = nf90_put_var(datasets(ioncid)%ncid, varid, values)
   if (ierr /= nf90_noerr) then
      goto 999
   end if

   ierr = UG_NOERR
   return ! Return with success

999 continue
    ! Some error (status was set earlier)

end function ionc_put_var_chars


!> Writes a complete mesh geometry
function ionc_write_geom_ugrid(filename) result(ierr)
   character(len=*), intent(in)       :: filename !< File name for netCDF dataset to be opened.
   integer                            :: ierr     !< Result status, ionc_noerr if successful.

   ierr = ug_write_geom_ugrid(filename)
end function ionc_write_geom_ugrid


!> Writes a complete map file
function ionc_write_map_ugrid(filename) result(ierr)
   character(len=*), intent(in)       :: filename !< File name for netCDF dataset to be opened.
   integer                            :: ierr     !< Result status, ionc_noerr if successful.

   ierr = ug_write_map_ugrid(filename)
end function ionc_write_map_ugrid


!> Add the global attributes to a NetCDF file
function ionc_add_global_attributes(ioncid,institution,source,references,version,modelname) result(ierr)

   integer,         intent(in) :: ioncid  !< The IONC data set id.
   integer                     :: ierr    !< Result status, ionc_noerr if successful.
   character(len=*), intent(in):: institution, source, references, version, modelname
   type(t_ug_meta)             :: meta

   meta%institution = institution
   meta%source = source
   meta%references = references
   meta%version = version
   meta%modelname = modelname

   ierr = ug_addglobalatts(datasets(ioncid)%ncid, meta)
end function ionc_add_global_attributes


!> Defines a new variable in an existing IONC dataset and sets up proper meta-attributes.
!! The variable can be defined either on a UGRID mesh, or on a UGRID network (via meshid or networkid, respectively).
!! NOTE: File should still be in define mode.
!! Does not write the actual data yet.
function ionc_def_var(ioncid, meshid, networkid, id_var, itype, iloctype, var_name, standard_name, long_name, & ! id_dims,
                    unit, cell_method, cell_measures, crs, ifill, dfill) result(ierr)
   integer,                    intent(in)    :: ioncid    !< The IONC data set id.
   integer,                    intent(in)    :: meshid    !< The mesh id in the specified data set (use 0 when instead a networkid is specified).
   integer,                    intent(in)    :: networkid !< The network id in the specified data set (use 0 when instead a meshid is specified).
   integer,                    intent(  out) :: id_var        !< Created NetCDF variable id.
!   integer, dimension(:),      intent(in)    :: id_dims       !< NetCDF dimension ids for this variable. Example: (/ id_edgedim /) for scalar data on edges, or (/ id_twodim, id_facedim /) for vector data on faces.
   integer,                    intent(in)    :: itype         !< The variable type expressed in one of the basic nf90_* types, e.g., nf90_double.
   integer,                    intent(in)    :: iloctype      !< Specifies at which unique mesh location data will be specified.
   character(len=*),           intent(in)    :: var_name      !< Name for the new data variable.
   character(len=*),           intent(in)    :: standard_name !< Standard name (CF-compliant) for 'standard_name' attribute in this variable.
   character(len=*),           intent(in)    :: long_name     !< Long name for 'long_name' attribute in this variable (use empty string if not wanted).
   character(len=*),           intent(in)    :: unit          !< Unit of this variable (CF-compliant) (use empty string for dimensionless quantities).
   character(len=*),           intent(in)    :: cell_method   !< Cell method for the spatial dimension (i.e., for edge/face/volume), value should be one of 'point', 'mean', etc. (See CF) (empty string if not relevant).
   character(len=*),           intent(in)    :: cell_measures !< Cell measures attribute, for example: 'area: mesh2d_cellarea', etc. (See CF) (empty string if not relevant).
   type(t_crs),      optional, intent(in)    :: crs           !< (Optional) Add grid_mapping attribute based on this coordinate reference system for independent coordinates
   integer,          optional, intent(in)    :: ifill         !< (Optional) Integer fill value.
   double precision, optional, intent(in)    :: dfill         !< (Optional) Double precision fill value.
   integer                                   :: ierr          !< Result status (UG_NOERR==NF90_NOERR) if successful.

   integer :: id_dims(1)
   character(len=256) :: meshornetname

   ! TODO: UNST-1548: AvD: refactor some of dflowfm's unc_def_var_map functionality into io_ugrid.
   ! For now, auto-insert some commonly used spatial dimension ids, such that caller ONLY needs to specify iloctype.
   ! NOTE: this only supports rank-1 arrays for now then.
   if (meshid > 0) then
      meshornetname = datasets(ioncid)%ug_file%meshnames(meshid)
      if      (iloctype == UG_LOC_NODE) then
         id_dims(1) = datasets(ioncid)%ug_file%meshids(meshid)%dimids(mdim_node)
      else if (iloctype == UG_LOC_EDGE) then
         id_dims(1) = datasets(ioncid)%ug_file%meshids(meshid)%dimids(mdim_edge)
      else if (iloctype == UG_LOC_FACE) then
         id_dims(1) = datasets(ioncid)%ug_file%meshids(meshid)%dimids(mdim_face)
      else
         ! loc type error is caught in ug_def_var below
         continue
      end if
   else if (networkid > 0) then
      meshornetname = datasets(ioncid)%ug_file%networksnames(networkid)
      if      (iloctype == UG_LOC_NODE) then
         id_dims(1) = datasets(ioncid)%ug_file%netids(networkid)%dimids(ntdim_1dnodes)
      else if (iloctype == UG_LOC_EDGE) then
         id_dims(1) = datasets(ioncid)%ug_file%netids(networkid)%dimids(ntdim_1dedges)
      else
         ! face location does not apply to 1d networks
         ierr = UG_INVALID_DATALOCATION
         goto 888
      end if
   else
      ! Error: No meshid, nor networkid.
      ierr = UG_ENOTVAR
      goto 888
   end if

   ierr = ug_def_var(datasets(ioncid)%ncid, id_var, id_dims, itype, iloctype, meshornetname, var_name, standard_name, long_name, &
                    unit, cell_method, cell_measures, crs, ifill, dfill)

   return

888 continue
    ! Some error occurred
end function ionc_def_var


!> Writes the complete mesh geometry
function ionc_write_mesh_struct(ioncid, meshids, networkids, meshgeom, network1dname) result(ierr)
   integer,             intent(in)    :: ioncid      !< The IONC data set id.
   type(t_ug_mesh),     intent(inout) :: meshids     !< Set of NetCDF-ids for all mesh geometry arrays.
   type(t_ug_network),  intent(inout) :: networkids  !< Set of NetCDF-ids for all mesh geometry arrays.
   type(t_ug_meshgeom), intent(in)    :: meshgeom    !< The complete mesh geometry in a single struct.
   character(len=MAXSTRLEN), optional :: network1dname !< The name of the mesh topology variable.
   integer                            :: ierr        !< Result status, ionc_noerr if successful.

   !Locals
   integer :: ierrLocal

   character(len=ug_idsLen), allocatable             :: nodeids(:)
   character(len=ug_idsLongNamesLen), allocatable    :: nodelongnames(:)

   allocate(nodeids(meshgeom%numnode))
   allocate(nodelongnames(meshgeom%numnode))
   if(associated(meshgeom%nodeids)) then
     nodeids       = meshgeom%nodeids
   end if
   if(associated(meshgeom%nodelongnames)) then
     nodelongnames = meshgeom%nodelongnames
   endif

   ierr = ug_write_mesh_struct( ncid = datasets(ioncid)%ncid, meshids = meshids, networkids = networkids, crs = datasets(ioncid)%crs, meshgeom = meshgeom, nodeids=nodeids, nodelongnames=nodelongnames, network1dname = network1dname)

end function ionc_write_mesh_struct

!> Initializes the io_netcdf library, setting up the logger.
function ionc_initialize(c_msg_callback,c_prgs_callback) result(ierr)
   use messagehandling
   use iso_c_binding
   type(c_funptr), value    :: c_msg_callback   !< Set a callback that will be cauled with new messages
   type(c_funptr), value    :: c_prgs_callback  !< Set a callback that will be cauled with new messages for progress
   integer                  :: ierr             !< Result status, ionc_noerr if successful.

   call set_progress_c_callback(c_prgs_callback)
   call progress("Finished initialization", 5.222d0);
   call set_logger(c_msg_callback)
   call SetMessageHandling(write2screen = .false. , &
                           useLog = .true., &
                           callback = netcdferror, &
                           thresholdLevel = LEVEL_INFO, &
                           thresholdLevel_log = LEVEL_INFO, &
                           thresholdLevel_callback = LEVEL_INFO, &
                           reset_counters = .true.)
   call SetMessage(LEVEL_INFO, 'Initialized with Rob')
   call progress("Finished initialization", 100d0);
   ierr = IONC_NOERR
end function ionc_initialize

!
! -- Private routines -----------------------------------------------------
!

!> Detect the conventions used in the given dataset.
!!
!! Detection is based on the :Conventions attribute in the file/data set.
!! Detected type is stored in the global datasets's attribute.
function detect_conventions(ioncid) result(ierr)
   integer, intent(in)  :: ioncid    !< The IONC data set id.
   integer              :: ierr      !< Result status, ionc_noerr if successful.

   integer              :: i         !< index of convention name

   character(len=MAXSTRLEN) :: convstring

   ierr = IONC_NOERR

   if (ioncid < 1 .or. ioncid > ndatasets) then
      ierr = IONC_EBADID
      goto 999
   end if

   convstring = ''
   ierr = nf90_get_att(datasets(ioncid)%ncid, nf90_global, 'Conventions', convstring)
   if (ierr /= nf90_noerr) then
      goto 999
   end if

   ! TODO: AvD: add support for detecting multiple conventions, and store as a sum of integer codes.
   i = index(convstring, 'UGRID')

   if (i > 0) then
      datasets(ioncid)%iconvtype = IONC_CONV_UGRID
      read (convstring(i+6:), *) datasets(ioncid)%convversion ! TODO: AvD: tokenize on ' ' or '/'
   else
      datasets(ioncid)%iconvtype = IONC_CONV_OTHER
   end if

   ! Successful
   return

999 continue
   ! Some error (status was set earlier)
end function detect_conventions

!> Detect the coordinate system used in the given dataset.
!!
!! Detection is based on the :projected_coordinate_system attribute in the file/data set.
!! Detected type is stored in the global datasets's attribute.
function detect_coordinate_system(ioncid) result(ierr)
    use netcdf_utils, only: ncu_get_var_attset

   integer, intent(in)  :: ioncid    !< The IONC data set id.
   integer              :: ierr      !< Result status, ionc_noerr if successful.

   integer            :: id_crs
   character(len=255) :: tmpstring

   ierr = IONC_NOERR

   if (ioncid < 1 .or. ioncid > ndatasets) then
      ierr = IONC_EBADID
      goto 999
   end if

   ierr = find_grid_mapping_var(datasets(ioncid)%ncid, id_crs)
   if (ierr /= nf90_noerr) then
      goto 999
   end if

   if (ierr == nf90_noerr) then
      ierr = nf90_inquire_variable(datasets(ioncid)%ncid, id_crs, name = datasets(ioncid)%crs%varname)
      ierr = nf90_get_var(datasets(ioncid)%ncid, id_crs, datasets(ioncid)%crs%epsg_code) ! NOTE: this is Deltares-convention only

      ! BELOW: add fallback detectin mechanisms to read the epsg codew (e.g. from :EPSG_code attribute (==ADAGUC), see: https://publicwiki.deltares.nl/display/NETCDF/Coordinates
      !ierr = nf90_get_att(datasets(ioncid)%ncid, id_crs, 'epsg', datasets(ioncid)%crs%epsg_code)
      if (ierr /= nf90_noerr .or. datasets(ioncid)%crs%epsg_code == nf90_fill_int) then
         ierr = nf90_get_att(datasets(ioncid)%ncid, id_crs, 'epsg', datasets(ioncid)%crs%epsg_code)
         if (ierr /= nf90_noerr) then
            ierr = nf90_get_att(datasets(ioncid)%ncid, id_crs, 'EPSG', datasets(ioncid)%crs%epsg_code)
         end if

         if (ierr /= nf90_noerr) then
            ierr = nf90_get_att(datasets(ioncid)%ncid, id_crs, 'epsg_code', tmpstring)
            if (ierr /= nf90_noerr) then
               ierr = nf90_get_att(datasets(ioncid)%ncid, id_crs, 'EPSG_code', tmpstring)
            endif
            read(tmpstring(index(tmpstring,':') + 1 : len_trim(tmpstring)),*) datasets(ioncid)%crs%epsg_code ! 'EPSG:99999'
         end if
      end if

      ierr = ncu_get_var_attset(datasets(ioncid)%ncid, id_crs, datasets(ioncid)%crs%attset)
      ierr = detect_proj_string(datasets(ioncid)%crs)
      ierr = IONC_NOERR ! NOTE: AvD: PROJ-string errors should not be fatal now, so always return success.
   else
      goto 999
   end if

   ! Successful
   return

999 continue
   datasets(ioncid)%crs%epsg_code = 0
   ierr = nf90_noerr
   ! Some error (status was set earlier)
end function detect_coordinate_system


!> Re-allocates an array of datasets to a bigger size.
subroutine realloc(arr, uindex, lindex, stat, keepExisting)
   implicit none
   type(t_ionc), allocatable, intent(inout)     :: arr(:)       !< List of opened datasets, maintained in global library state, indexed by the unique ionc_id.
   integer, intent(in)                          :: uindex       !< New upper index of the array (i.e., size, if lindex==1)
   integer, intent(in), optional                :: lindex       !< (optional) New lower index of the array. Default: 1
   integer, intent(out), optional               :: stat         !< Result status of the realloc operation.
   logical, intent(in), optional                :: keepExisting !< Whether or not to keep the original array contents.

   type(t_ionc), allocatable :: b(:)
   integer        :: uind, lind, muind, mlind, lindex_

   integer        :: i
   integer        :: localErr
   logical        :: docopy
   logical        :: equalSize

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = 1
   endif

   if (present(keepExisting)) then
      docopy = keepExisting
   else
      docopy = .true.
   end if

   if (present(stat)) stat = 0
   localErr = 0
   if (allocated(arr)) then
      uind = ubound(arr,1)
      lind = lbound(arr,1)
      equalSize = (uindex == uind) .and. (lindex_ == lind)
      if (equalSize .and. docopy) then
         goto 999 ! output=input
      end if
!
      if (docopy) then
         mlind = max(lind, lindex_)
         muind = min(uind, uindex)
         allocate (b(mlind:muind))
         b(mlind:muind) = arr(mlind:muind)
      endif
      if (.not.equalSize) deallocate(arr, stat = localErr)
   endif
    if (.not.allocated(arr) .and. localErr==0) then
        allocate(arr(lindex_:uindex), stat = localErr)
    endif
    if (allocated(b) .and. localErr==0) then
        if (size(b)>0) then
            arr(mlind:muind) = b(mlind:muind)
        endif
        deallocate(b, stat = localErr)
    endif
999 continue
   if (present(stat)) stat = localErr

end subroutine realloc


!> Adds an open NetCDF file to the datasets array, initiazing this dataset's metadata.
function add_dataset(ncid, netCDFFile, ioncid, iconvtype) result(ierr)
   integer,           intent(in)    :: ncid       !< The NetCDF dataset id.
   character (len=*), intent(in   ) :: netCDFFile !< File name for netCDF dataset to be opened.
   integer,           intent(  out) :: ioncid     !< The io_netcdf dataset id (this is not the NetCDF ncid, which is stored in datasets(ioncid)%ncid.
   integer, optional, intent(in)    :: iconvtype  !<(optional) The detected conventions in the file.
   integer                          :: ierr       !< Result status (IONC_NOERR if successful).

   integer :: istat

   call realloc(datasets, ndatasets+1, keepExisting=.true., stat=istat) ! TODO: AvD: Add buffered growth here.
   if (istat /= 0) then
      ierr = IONC_ENOMEM
      goto 999
   end if
   ndatasets = ndatasets + 1
   datasets(ndatasets)%ncid = ncid
   ioncid = ndatasets

   if (.not. present(iconvtype)) then
      ierr = detect_conventions(ioncid)
   else
      datasets(ioncid)%iconvtype = iconvtype
   endif

   select case (datasets(ioncid)%iconvtype)
   !
   ! UGRID initialization: If the case is not executed the meshidid structure will not be created
   !
   case (IONC_CONV_UGRID)
      allocate(datasets(ioncid)%ug_file)
      datasets(ioncid)%ug_file%filename = trim(netCDFFile)
      ierr = ug_init_dataset(datasets(ioncid)%ncid, datasets(ioncid)%ug_file)
      if (ierr /= UG_NOERR) then
         ! Keep UG error code and exit
         goto 999
      end if

   case default
      ! We accept file with no specific conventions.
   end select

   allocate(datasets(ioncid)%crs)

   ierr = detect_coordinate_system(ioncid)

   ! Successful
   return

999 continue
   ! Some error (status was set earlier)

end function add_dataset

subroutine netcdfError(errorLevel, message)
    use MessageHandling
    implicit none
    integer, intent(in):: errorLevel
    character(len=*), intent(in) :: message
    if (errorLevel >= LEVEL_FATAL) then
        !call closeall()
    endif
end subroutine netcdfError
!
! Network and mesh1d functions
!
!> This function creates a 1d mesh accordingly to the new 1d format.
function ionc_create_1d_network_ugrid(ioncid, networkid, networkName, nNodes, nBranches, nGeometry) result(ierr)

   integer, intent(in)                :: ioncid
   integer, intent(inout)             :: networkid
   character(len=*), intent(in)       :: networkName !< File name for netCDF dataset to be opened.
   integer, intent(in)                :: nNodes, nBranches, nGeometry
   integer                            :: ierr, dum

   ! allocate add a meshids
   ierr = ug_add_network(datasets(ioncid)%ncid, datasets(ioncid)%ug_file, networkid)
   ! set the network name
   datasets(ioncid)%ug_file%networksnames(networkid) = networkName
   ! add a 1d network
   ierr = ug_create_1d_network_v1(datasets(ioncid)%ncid, datasets(ioncid)%ug_file%netids(networkid), networkName, nNodes, nBranches, nGeometry, datasets(ioncid)%crs)

end function ionc_create_1d_network_ugrid

function ionc_write_1d_network_nodes_ugrid(ioncid, networkid, nodesX, nodesY, nodeIds, nodeLongnames) result(ierr)

   integer, intent(in)                :: ioncid
   integer, intent(in)                :: networkid
   double precision,    intent(in)    :: nodesX(:)
   double precision,    intent(in)    :: nodesY(:)
   character(len=*), intent(in)       :: nodeIds(:),nodeLongnames(:) !< File name for netCDF dataset to be opened.
   integer                            :: ierr

   ierr = ug_write_1d_network_nodes(datasets(ioncid)%ncid, datasets(ioncid)%ug_file%netids(networkid), nodesX, nodesY, nodeIds, &
       nodeLongnames)

end function ionc_write_1d_network_nodes_ugrid

function ionc_put_1d_network_branches_ugrid(ioncid, networkid, sourcenodeid, targetnodeid, branchids, branchlengths, branchlongnames, &
    nbranchgeometrypoints, nBranches, startIndex) result(ierr)

   integer, intent(in)                :: ioncid, startIndex
   integer, intent(in)                :: networkId, nBranches
   integer, intent(in)                :: sourcenodeid(:), targetnodeid(:), nbranchgeometrypoints(:)
   character(len=*),intent(in)        :: branchids(:),branchlongnames(:)
   double precision, intent(in)       :: branchlengths(:)
   integer                            :: ierr

   ierr = ug_put_1d_network_branches(datasets(ioncid)%ncid, datasets(ioncid)%ug_file%netids(networkid),sourcenodeid,targetnodeid, &
       branchIds, branchlengths, branchlongnames, nbranchgeometrypoints,nBranches, startIndex)

end function ionc_put_1d_network_branches_ugrid

!< write the branch order array, it might be temporary function
function ionc_put_1d_network_branchorder_ugrid(ioncid, networkid, branchorder) result(ierr)

    integer, intent(in)                :: ioncid
    integer, intent(in)                :: networkId
    integer, intent(in)                :: branchorder(:)
    integer                            :: ierr

    ierr =  ug_put_1d_network_branchorder(datasets(ioncid)%ncid, datasets(ioncid)%ug_file%netids(networkid), branchorder)

end function ionc_put_1d_network_branchorder_ugrid

function ionc_put_1d_network_branchtype_ugrid(ioncid, networkid, branchtype) result(ierr)

    integer, intent(in)                :: ioncid
    integer, intent(in)                :: networkId
    integer, intent(in)                :: branchtype(:)
    integer                            :: ierr

    ierr =  ug_put_1d_network_branchtype(datasets(ioncid)%ncid, datasets(ioncid)%ug_file%netids(networkid), branchtype)

end function ionc_put_1d_network_branchtype_ugrid

function ionc_write_1d_network_branches_geometry_ugrid(ioncid, networkid, geopointsX, geopointsY) result(ierr)

   integer, intent(in)                :: ioncid
   integer, intent(in)                :: networkid
   double precision, intent(in)       :: geopointsX(:),geopointsY(:)
   integer                            :: ierr

   ierr = ug_write_1d_network_branches_geometry(datasets(ioncid)%ncid, datasets(ioncid)%ug_file%netids(networkid), geopointsX, &
       geopointsY)

end function ionc_write_1d_network_branches_geometry_ugrid

!> Returns the netids struct contains dimension and variable ids
!! for a given networkid in a given io_netcdf dataset, if present.
!!
!! \see get_1d_networkid_ugrid()
function ionc_get_1d_netids(ioncid, networkid, netids) result(ierr)
   integer,            intent(in)    :: ioncid    !< The io_netcdf dataset id (this is not the
   integer,            intent(in)    :: networkid !< The id of the requested network in the dataset's subset of network(s).
   type(t_ug_network), intent(inout) :: netids    !< The netids set of dimension and variable ids for the requested networkid.
   integer                           :: ierr      !< Result status (IONC_NOERR if successful).

   ierr = IONC_NOERR

   if (ioncid <= 0 .or. ioncid > ndatasets) then
      ierr = IONC_EBADID
      goto 888
   end if

   if (.not. associated(datasets(ioncid)%ug_file)) then
      ierr = IONC_ENOTAVAILABLE
      goto 888
   end if

   if (networkid <= 0 .or. networkid > datasets(ioncid)%ug_file%numnet) then
      ierr = IONC_EBADID
      goto 888
   end if

   netids = datasets(ioncid)%ug_file%netids(networkid)

   ! Success
   return


888 continue
    ! Some error occurred

end function ionc_get_1d_netids


function ionc_get_1d_network_nodes_count_ugrid(ioncid, networkid, nNodes) result(ierr)

   integer, intent(in)                :: ioncid
   integer, intent(in)                :: networkid
   integer, intent(out)               :: nNodes
   integer                            :: ierr

   ierr = ug_get_1d_network_nodes_count(datasets(ioncid)%ncid, datasets(ioncid)%ug_file%netids(networkid),nNodes)

end function ionc_get_1d_network_nodes_count_ugrid

function ionc_get_1d_network_branches_count_ugrid(ioncid, networkid, nBranches)  result(ierr)

   integer, intent(in)                :: ioncid
   integer, intent(in)                :: networkid
   integer, intent(out)               :: nBranches
   integer                            :: ierr

   ierr = ug_get_1d_network_branches_count(datasets(ioncid)%ncid, datasets(ioncid)%ug_file%netids(networkid),nBranches)

end function ionc_get_1d_network_branches_count_ugrid

function  ionc_get_1d_network_branches_geometry_coordinate_count_ugrid(ioncid, networkid, ngeometrypoints)  result(ierr)

   integer, intent(in)                :: ioncid
   integer, intent(in)                :: networkid
   integer, intent(out)               :: ngeometrypoints
   integer                            :: ierr

   ierr = ug_get_1d_network_branches_geometry_coordinate_count(datasets(ioncid)%ncid, datasets(ioncid)%ug_file%netids(networkid), &
       ngeometrypoints)

end function ionc_get_1d_network_branches_geometry_coordinate_count_ugrid

function  ionc_read_1d_network_nodes_ugrid(ioncid, networkid, nodesX, nodesY, nodeids, nodelongnames) result(ierr)

   integer, intent(in)                :: ioncid
   integer, intent(in)                :: networkid
   double precision,intent(out)       :: nodesX(:), nodesY(:)
   character(len=*),intent(out)       :: nodeids(:), nodelongnames(:)
   integer                            :: ierr

   ierr = ug_read_1d_network_nodes(datasets(ioncid)%ncid, datasets(ioncid)%ug_file%netids(networkid), nodesX, nodesY, nodeids, nodelongnames)

end function ionc_read_1d_network_nodes_ugrid

function  ionc_get_1d_network_branches_ugrid(ioncid, networkid, sourcenodeid, targetnodeid, branchid, branchlengths, branchlongnames, nbranchgeometrypoints, startIndex) result(ierr)

   integer, intent(in)                :: ioncid, startIndex
   integer, intent(in)                :: networkid
   integer,intent(out)                :: sourcenodeid(:), targetnodeid(:), nbranchgeometrypoints(:)
   character(len=*), intent(out)      :: branchid(:), branchlongnames(:)
   double precision, intent(out)      :: branchlengths(:)
   integer                            :: ierr

   ierr = ug_get_1d_network_branches(datasets(ioncid)%ncid, datasets(ioncid)%ug_file%netids(networkid), sourcenodeid, &
       targetnodeid,branchlengths,nbranchgeometrypoints, startIndex, branchid, branchlongnames)

end function ionc_get_1d_network_branches_ugrid


!< get the branch order array, it might be temporary function
function ionc_get_1d_network_branchorder_ugrid(ioncid, networkid, branchorder) result(ierr)

   integer, intent(in)                :: ioncid
   integer, intent(in)                :: networkid
   integer,intent(out)                :: branchorder(:)
   integer                            :: ierr

   ierr = ug_get_1d_network_branchorder(datasets(ioncid)%ncid, datasets(ioncid)%ug_file%netids(networkid), branchorder)

end function ionc_get_1d_network_branchorder_ugrid

function ionc_get_1d_network_branchtype_ugrid(ioncid, networkid, branchtype) result(ierr)

   integer, intent(in)                :: ioncid
   integer, intent(in)                :: networkid
   integer,intent(out)                :: branchtype(:)
   integer                            :: ierr

   ierr = ug_get_1d_network_branchtype(datasets(ioncid)%ncid, datasets(ioncid)%ug_file%netids(networkid), branchtype)

end function ionc_get_1d_network_branchtype_ugrid


function  ionc_read_1d_network_branches_geometry_ugrid(ioncid, networkid, geopointsX, geopointsY) result(ierr)

   integer, intent(in)                :: ioncid, networkid
   double precision, intent(out)      :: geopointsX(:), geopointsY(:)
   integer                            :: ierr

   ierr = ug_read_1d_network_branches_geometry(datasets(ioncid)%ncid, datasets(ioncid)%ug_file%netids(networkid), geopointsX, geopointsY)

end function ionc_read_1d_network_branches_geometry_ugrid


function ionc_create_1d_mesh_ugrid(ioncid, networkname, meshid, meshname, nmeshpoints) result(ierr)

   integer, intent(in)         :: ioncid, nmeshpoints
   integer, intent (inout)     :: meshid
   character(len=*),intent(in) :: meshname, networkname
   integer                     :: ierr

   !adds a meshids structure
   ierr = ug_add_mesh(datasets(ioncid)%ncid, datasets(ioncid)%ug_file, meshid)
   ! set the meshname
   datasets(ioncid)%ug_file%meshnames(meshid) = meshname
   ! create mesh
   ierr = ug_create_1d_mesh(datasets(ioncid)%ncid, networkname, datasets(ioncid)%ug_file%meshids(meshid), meshname, nmeshpoints)

end function ionc_create_1d_mesh_ugrid


function ionc_create_1d_mesh_ugrid_v1(ioncid, networkname, meshid, meshname, nmeshpoints, nmeshedges, writexy) result(ierr)

   integer, intent(in)         :: ioncid, nmeshpoints, nmeshedges
   integer, intent (inout)     :: meshid
   integer, intent(in)         :: writexy
   character(len=*),intent(in) :: meshname, networkname
   integer                     :: ierr

   integer :: ierrLocal

   !adds a meshids structure
   ierr = ug_add_mesh(datasets(ioncid)%ncid, datasets(ioncid)%ug_file, meshid)
   ! set the meshname
   datasets(ioncid)%ug_file%meshnames(meshid) = meshname
   ! create mesh
   ierr = ug_create_1d_mesh_v2(datasets(ioncid)%ncid, networkname, datasets(ioncid)%ug_file%meshids(meshid), meshname, nmeshpoints, nmeshedges, writexy, datasets(ioncid)%crs)

end function ionc_create_1d_mesh_ugrid_v1


function ionc_def_mesh_ids_ugrid(ioncid, meshid, locationType) result(ierr)

   integer, intent(in)         :: ioncid, meshid, locationType
   integer                     :: ierr

   ierr = ug_def_mesh_ids(datasets(ioncid)%ncid, datasets(ioncid)%ug_file%meshids(meshid),datasets(ioncid)%ug_file%meshnames(meshid), locationType)

end function ionc_def_mesh_ids_ugrid

function ionc_put_1d_mesh_discretisation_points_ugrid(ioncid, meshid, branchidx, offset, startIndex) result(ierr)

  integer, intent(in)         :: ioncid, meshid, startIndex
  integer, intent(in)         :: branchidx(:)
  double precision,intent(in) :: offset(:)
  integer                     :: ierr

  ierr = ug_put_1d_mesh_discretisation_points(datasets(ioncid)%ncid, datasets(ioncid)%ug_file%meshids(meshid), branchidx, offset, startIndex)

end function ionc_put_1d_mesh_discretisation_points_ugrid

function ionc_put_1d_mesh_discretisation_points_ugrid_v1(ioncid, meshid, branchidx, offset, startIndex, coordx, coordy) result(ierr)

  integer, intent(in)         :: ioncid, meshid, startIndex
  integer, intent(in)         :: branchidx(:)
  double precision,intent(in) :: offset(:), coordx(:), coordy(:)
  integer                     :: ierr

  ierr = ug_put_1d_mesh_discretisation_points_v1(datasets(ioncid)%ncid, datasets(ioncid)%ug_file%meshids(meshid), branchidx, offset, startIndex, coordx, coordy)

end function ionc_put_1d_mesh_discretisation_points_ugrid_v1

function ionc_get_1d_mesh_edges(ioncid, meshid, edgebranchidx, edgeoffset, startIndex, edgex, edgey) result(ierr)

  integer, intent(in)            :: ioncid, meshid, startIndex
  integer, intent(inout)         :: edgebranchidx(:)
  double precision,intent(inout) :: edgeoffset(:), edgex(:), edgey(:)
  integer                        :: ierr

  ierr=ug_get_1d_mesh_edge_coordinates(datasets(ioncid)%ncid, datasets(ioncid)%ug_file%meshids(meshid), edgebranchidx, edgeoffset, startIndex, edgex, edgey, .false.)

end function ionc_get_1d_mesh_edges

function ionc_put_1d_mesh_edges(ioncid, meshid, edgebranchidx, edgeoffset, startIndex, coordx, coordy) result(ierr)

  integer, intent(in)         :: ioncid, meshid, startIndex
  integer, intent(in)         :: edgebranchidx(:)
  double precision,intent(in) :: edgeoffset(:), coordx(:), coordy(:)
  integer                     :: ierr

  ierr = ug_put_1d_mesh_edges(datasets(ioncid)%ncid, datasets(ioncid)%ug_file%meshids(meshid), edgebranchidx, edgeoffset, startIndex, coordx, coordy)

end function ionc_put_1d_mesh_edges

function ionc_get_1d_mesh_discretisation_points_count_ugrid(ioncid, meshid, nmeshpoints) result(ierr)

   integer, intent(in)    :: ioncid, meshid
   integer, intent(out)   :: nmeshpoints
   integer                :: ierr

   ierr = ug_get_1d_mesh_discretisation_points_count(datasets(ioncid)%ncid, datasets(ioncid)%ug_file%meshids(meshid), nmeshpoints)

end function ionc_get_1d_mesh_discretisation_points_count_ugrid


function ionc_get_1d_mesh_discretisation_points_ugrid(ioncid, meshid, branchidx, offset, startIndex) result(ierr)

  integer, intent(in)         :: ioncid, meshid, startIndex
  integer, intent(out)        :: branchidx(:)
  double precision,intent(out):: offset(:)
  integer                     :: ierr

  ierr = ug_get_1d_mesh_discretisation_points(datasets(ioncid)%ncid, datasets(ioncid)%ug_file%meshids(meshid), branchidx, offset, startIndex)

end function ionc_get_1d_mesh_discretisation_points_ugrid

!> Gets the 1D mesh discretisation points, both the branchidx/offset and the x/y coordinates.
function ionc_get_1d_mesh_discretisation_points_ugrid_v1(ioncid, meshid, branchidx, offset, startIndex, coordx, coordy) result(ierr)

  integer, intent(in)         :: ioncid, meshid, startIndex
  integer, intent(out)        :: branchidx(:)
  double precision,intent(out):: offset(:), coordx(:), coordy(:)
  integer                     :: ierr

  ierr = ug_get_1d_mesh_discretisation_points(datasets(ioncid)%ncid, datasets(ioncid)%ug_file%meshids(meshid), branchidx, offset, startIndex, coordx, coordy)

end function ionc_get_1d_mesh_discretisation_points_ugrid_v1

!
! create mesh links
!

function ionc_def_mesh_contact_ugrid(ioncid, contactsmesh, contactmeshname, ncontacts, idmesh1, idmesh2, locationType1Id, locationType2Id) result(ierr)

   integer, intent(in)                :: ioncid, ncontacts, idmesh1, idmesh2,locationType1Id,locationType2Id
   integer, intent(inout)             :: contactsmesh
   character(len=*), intent(in)       :: contactmeshname
   integer                            :: ierr

  ! first add an ug datastructure
  ierr = ug_add_mesh_contact(datasets(ioncid)%ncid, datasets(ioncid)%ug_file, contactsmesh)

  ! set the contact mesh name
  datasets(ioncid)%ug_file%contactsnames(contactsmesh) = contactmeshname
  ! create the variables and attributes
  ierr = ug_def_mesh_contact(datasets(ioncid)%ncid, datasets(ioncid)%ug_file%contactids(contactsmesh), contactmeshname, ncontacts, datasets(ioncid)%ug_file%meshids(idmesh1), datasets(ioncid)%ug_file%meshids(idmesh2), locationType1Id, locationType2Id)

end function ionc_def_mesh_contact_ugrid

!>Write the mesh 1d node edge array
function ionc_write_mesh_1d_edge_nodes (ioncid, meshid, numEdge, mesh_1d_edge_nodes, start_index) result(ierr)
   integer,          intent(in)         :: ioncid, meshid, numEdge
   integer,          intent(in)         :: mesh_1d_edge_nodes(:,:)  !< Edge-to-node mapping array.
   integer                              :: start_index              !< The base index of the provided arrays (0 if this function writes array from C/C++/C#, 1 for Fortran)
   integer                              :: ierr                     !< Result status (IONC_NOERR==NF90_NOERR) if successful.

   character(len=nf90_max_name)         :: meshName

   !get the mesh name
   ierr = ug_get_mesh_name(datasets(ioncid)%ncid, datasets(ioncid)%ug_file%meshids(meshid), meshname = meshName)

   ! Check for any remaining native NetCDF errors
   if (ierr /= nf90_noerr) then
      goto 801
   end if

   !write the 1d mesh edge node array
   ierr = ug_write_mesh_1d_edge_nodes (datasets(ioncid)%ncid, datasets(ioncid)%ug_file%meshids(meshid), meshName, numEdge, mesh_1d_edge_nodes, start_index)

801 continue
   ! Some error (status was set earlier)

end function ionc_write_mesh_1d_edge_nodes

!> Get the number of contact links in a specified meshcontact set.
function ionc_get_contacts_count_ugrid(ioncid, contactid, ncontacts) result(ierr)

   integer, intent(in)      :: ioncid     !< The IONC data set id.
   integer, intent(in   )   :: contactid  !< The contact id in the specified data set.
   integer, intent(inout)   :: ncontacts  !< Number of contact links in the specified meshcontact set.
   integer                  :: ierr       !< Result status (IONC_NOERR if successful).

   ierr = ug_get_contacts_count(datasets(ioncid)%ncid, datasets(ioncid)%ug_file%contactids(contactid), ncontacts)

end function ionc_get_contacts_count_ugrid

function ionc_put_mesh_contact_ugrid(ioncid, contactsmesh, mesh1indexes, mesh2indexes, contactsids, contactslongnames, contacttype, startIndex)  result(ierr)

   integer, intent(in)                :: ioncid, contactsmesh, startIndex
   integer, intent(in)                :: mesh1indexes(:),mesh2indexes(:), contacttype(:)
   character(len=*), intent(in)       :: contactsids(:), contactslongnames(:)
   integer                            :: ierr

   ierr = ug_put_mesh_contact(datasets(ioncid)%ncid, datasets(ioncid)%ug_file%contactids(contactsmesh), mesh1indexes, mesh2indexes, contacttype, contactsids, contactslongnames, startIndex)

end function ionc_put_mesh_contact_ugrid

function ionc_get_mesh_contact_ugrid(ioncid, contactsmesh, mesh1indexes, mesh2indexes, contactsids, contactslongnames, contacttype, startIndex)  result(ierr)

   integer, intent(in)                :: ioncid, contactsmesh, startIndex
   integer, intent(inout)             :: mesh1indexes(:),mesh2indexes(:), contacttype(:)
   character(len=*), intent(inout)    :: contactsids(:), contactslongnames(:)
   integer                            :: ierr

   ierr = ug_get_mesh_contact(datasets(ioncid)%ncid, datasets(ioncid)%ug_file%contactids(contactsmesh), mesh1indexes, mesh2indexes, contactsids, contactslongnames,contacttype, startIndex)

end function ionc_get_mesh_contact_ugrid


!> Gets the index table of the links in a given meshcontact set.
!! The index table is renumbered automatically when the given.
!! desired startIndex is different from the startIndex in the file.
!! The arrays should be allocated at the correct size already.
function ionc_get_mesh_contact_links(ioncid, contactid, contactlinksfromto, contacttype, startIndex)  result(ierr)
   integer, intent(in   ) :: ioncid         !< NetCDF dataset id
   integer, intent(in   ) :: contactid      !< Mesh contact topology set
   integer, intent(inout) :: contactlinksfromto(:, :) !< (2,numcontactlinks) table with from-to indices
   integer, intent(inout) :: contacttype(:) !< Contact type for each link
   integer, intent(in   ) :: startIndex     !< Desired start index for the table.
   integer                :: ierr           !< Result status (UG_NOERR if succesful)

   ierr = ug_get_mesh_contact_links(datasets(ioncid)%ncid, datasets(ioncid)%ug_file%contactids(contactid), contactlinksfromto, contacttype, startIndex)

end function ionc_get_mesh_contact_links


!
! Cloning functions
!

function ionc_clone_mesh_definition_ugrid( ncidin, ncidout, meshidin, meshidout ) result(ierr)

   integer, intent(in)               :: ncidin, ncidout, meshidin
   integer, intent(inout)            :: meshidout
   integer                           :: ierr,status,sourceid,targetid
   character(len=nf90_max_name)      :: buffer

   !We always add meshes, not links (otherwise we need to check here if ug_add_links should be used instead)
   ierr = ug_add_mesh(datasets(ncidout)%ncid, datasets(ncidout)%ug_file, meshidout)
   ierr = ug_clone_mesh_definition( datasets(ncidin)%ncid, datasets(ncidout)%ncid, datasets(ncidin)%ug_file%meshids(meshidin), datasets(ncidout)%ug_file%meshids(meshidout))

end function ionc_clone_mesh_definition_ugrid

function ionc_clone_mesh_data_ugrid( ncidin, ncidout, meshidin, meshidout ) result(ierr)

   integer, intent(in)    :: ncidin, ncidout, meshidin
   integer, intent(in)    :: meshidout
   integer                :: ierr,status,sourceid,targetid

   ierr = ug_clone_mesh_data( datasets(ncidin)%ncid, datasets(ncidout)%ncid,datasets(ncidin)%ug_file%meshids(meshidin), datasets(ncidout)%ug_file%meshids(meshidout))

end function ionc_clone_mesh_data_ugrid

function ionc_getfullversionstring_io_netcdf( version_string )  result(ierr)
   use io_netcdf_version_module

   character(len=*), intent(inout) :: version_string !< String to contain the full version string of this io_netcdf library.
   integer                :: ierr

   call getfullversionstring_io_netcdf(version_string)
   ierr = 0

end function ionc_getfullversionstring_io_netcdf

!
! Get the mesh ids
!

function ionc_get_number_of_networks_ugrid(ioncid, nnumNetworks)  result(ierr)

   integer, intent(in)    :: ioncid
   integer, intent(inout) :: nnumNetworks
   integer                :: ierr

   ierr = ug_ionc_get_number_of_networks(datasets(ioncid)%ug_file, nnumNetworks)

end function ionc_get_number_of_networks_ugrid

function ionc_get_number_of_meshes_ugrid(ioncid, meshType, numMeshes)  result(ierr)

   integer, intent(in)    :: ioncid,meshType
   integer, intent(inout) :: numMeshes
   integer                :: ierr

   ierr=ug_get_number_of_meshes(datasets(ioncid)%ncid, datasets(ioncid)%ug_file, meshType, numMeshes)

end function ionc_get_number_of_meshes_ugrid

function ionc_get_network_ids_ugrid(ioncid, networkids)  result(ierr)

   integer, intent(in)    :: ioncid
   integer, intent(inout) :: networkids(:)
   integer                :: ierr

   ierr=ug_get_network_ids(datasets(ioncid)%ug_file, networkids)

end function ionc_get_network_ids_ugrid

function ionc_ug_get_mesh_ids_ugrid(ioncid, meshType, meshids)  result(ierr)

   integer, intent(in)    :: ioncid,meshType
   integer, intent(inout) :: meshids(:)
   integer                :: ierr

   ierr = ug_get_mesh_ids(datasets(ioncid)%ncid, datasets(ioncid)%ug_file, meshType, meshids)

end function ionc_ug_get_mesh_ids_ugrid


function ionc_get_1d_network_id_ugrid(ioncid, networkid) result(ierr)

   integer, intent(in)    :: ioncid
   integer, intent(inout) :: networkid
   integer                :: ierr

   ierr = ug_get_1d_network_id(datasets(ioncid)%ncid, datasets(ioncid)%ug_file, networkid)

end function ionc_get_1d_network_id_ugrid

function ionc_get_1d_mesh_id_ugrid(ioncid, meshid) result(ierr)

   integer, intent(in)    :: ioncid
   integer, intent(inout) :: meshid
   integer                :: ierr

   ierr = ug_get_mesh_id(datasets(ioncid)%ncid, datasets(ioncid)%ug_file, meshid, 1)

end function ionc_get_1d_mesh_id_ugrid

function ionc_get_2d_mesh_id_ugrid(ioncid, meshid) result(ierr)

   integer, intent(in)    :: ioncid
   integer, intent(inout) :: meshid
   integer                :: ierr

   ierr = ug_get_mesh_id(datasets(ioncid)%ncid, datasets(ioncid)%ug_file, meshid, 2)

end function ionc_get_2d_mesh_id_ugrid

function ionc_get_3d_mesh_id_ugrid(ioncid, meshid) result(ierr)

   integer, intent(in)    :: ioncid
   integer, intent(inout) :: meshid
   integer                :: ierr

   ierr = ug_get_mesh_id(datasets(ioncid)%ncid, datasets(ioncid)%ug_file, meshid, 3)

end function ionc_get_3d_mesh_id_ugrid

function ionc_get_contact_id_ugrid(ioncid, contactid) result(ierr)

   integer, intent(in)    :: ioncid
   integer, intent(inout) :: contactid
   integer                :: ierr

   ierr = ug_get_contact_id(datasets(ioncid)%ncid, datasets(ioncid)%ug_file, contactid)

end function ionc_get_contact_id_ugrid


!< Count the number of meshes associated with a network
function ionc_count_mesh_ids_from_network_id_ugrid(ioncid, netid, nmeshids) result(ierr)

   integer,  intent(in)              :: ioncid
   integer,  intent(in)              :: netid
   integer,  intent(inout)           :: nmeshids
   integer                           :: ierr

   ierr = ug_count_mesh_ids_from_network_id(datasets(ioncid)%ncid, datasets(ioncid)%ug_file, netid, nmeshids)

end function ionc_count_mesh_ids_from_network_id_ugrid

!< Get an integer array array of the mesh ids associated with the network id
function ionc_get_mesh_ids_from_network_id_ugrid(ioncid, netid, meshids) result(ierr)

   integer,  intent(in)              :: ioncid
   integer,  intent(in)              :: netid
   integer,  intent(inout)           :: meshids(:)
   integer                           :: ierr

   ierr = ug_get_mesh_ids_from_network_id(datasets(ioncid)%ncid, datasets(ioncid)%ug_file, netid, meshids)

end function ionc_get_mesh_ids_from_network_id_ugrid


function ionc_get_network_id_from_mesh_id_ugrid(ioncid, meshid, networkid) result(ierr)

   integer,  intent(in)              :: ioncid
   integer,  intent(in)              :: meshid
   integer,  intent(inout)           :: networkid
   integer                           :: ierr

   ierr = ug_get_network_id_from_mesh_id(datasets(ioncid)%ncid, datasets(ioncid)%ug_file%meshids(meshid), datasets(ioncid)%ug_file, networkid)

end function ionc_get_network_id_from_mesh_id_ugrid

!> Gets the number of contats from a data set.
function ionc_get_contact_topo_count(ioncid, ncontacts) result(ierr)
   integer,             intent(in)    :: ioncid  !< The IONC data set id.
   integer,             intent(  out) :: ncontacts !< Number of meshes.
   integer                            :: ierr    !< Result status, ionc_noerr if successful.

   ! TODO: AvD: some error handling if ioncid is wrong
   ierr = ug_get_contact_topo_count(datasets(ioncid)%ncid, ncontacts)

end function ionc_get_contact_topo_count

!> Gets the name of the contact topology variable in an open dataset.
function ionc_get_contact_name(ioncid, contactid, contactname) result(ierr)
   integer,             intent(in)    :: ioncid      !< The IONC data set id.
   integer,             intent(in)    :: contactid   !< The contact id in the specified data set.
   character(len=*),    intent(  out) :: contactname !< The name of the mesh topology variable.
   integer                            :: ierr        !< Result status, ionc_noerr if successful.

   ierr = ug_get_contact_name(datasets(ioncid)%ncid, datasets(ioncid)%ug_file%contactids(contactid), contactname)
end function ionc_get_contact_name

!> Gets contact id from the name of a mesh contact.
function ionc_get_contact_id_from_contact_name_ugrid(ioncid, contactmeshname, contactid) result(ierr)

   integer,          intent(in   )   :: ioncid          !< The IONC data set id.
   character(len=*), intent(in   )   :: contactmeshname !< The given name of a mesh contact.
   integer,          intent(inout)   :: contactid       !< The requested contact id.
   integer                           :: ierr            !< Result status, ionc_noerr if successful.

   ierr = ug_get_contact_id_from_meshcontactname(datasets(ioncid)%ncid, datasets(ioncid)%ug_file, contactmeshname, contactid)

end function ionc_get_contact_id_from_contact_name_ugrid

!> Returns the contactids struct contains dimension and variable ids
!! for a given contactid in a given io_netcdf dataset.
function ionc_get_contactids(ioncid, contactid, contactids) result(ierr)
   integer,             intent(in)    :: ioncid        !< The IONC dataset id.
   integer,             intent(in)    :: contactid     !< The id of the requested mesh contact in the dataset's subset of contact(s).
   type(t_ug_contacts), intent(inout) :: contactids    !< The contactids set of dimension and variable ids for the requested contactid.
   integer                            :: ierr          !< Result status, ionc_noerr if successful.

   ierr = IONC_NOERR

   if (ioncid <= 0 .or. ioncid > ndatasets) then
      ierr = IONC_EBADID
      goto 888
   end if

   if (.not. associated(datasets(ioncid)%ug_file)) then
      ierr = IONC_ENOTAVAILABLE
      goto 888
   end if

   if (contactid <= 0) then
      ierr = IONC_EBADID
      goto 888
   end if

   contactids = datasets(ioncid)%ug_file%contactids(contactid)

   ! Success
   return


888 continue
    ! Some error occurred

end function ionc_get_contactids
end module io_netcdf
