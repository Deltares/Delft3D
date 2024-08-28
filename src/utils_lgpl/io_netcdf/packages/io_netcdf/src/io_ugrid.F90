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
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

!> I/O module for reading and writing NetCDF files with UGRID-compliant data on unstructured grids.
!! UGRID Conventions website: https://github.com/ugrid-conventions/ugrid-conventions
!! Deltares 1D network proposal: https://content.oss.deltares.nl/delft3d/manuals/D-Flow_FM_User_Manual.pdf#section.B.2
!!
!! Note:  all IDs in ugrid/nf90 are 1-based!

module io_ugrid
   use netcdf
   use messagehandling
   use coordinate_reference_system
   use meshdata
   use m_ug_meta
   use m_ug_mesh
   use m_ug_network
   use m_ug_contacts
   use precision, only: dp

   implicit none

! TODO: AvD: GL2: add 'full_grid_output' support, to write 1. face_edge_connectivity; 2. edge_face_connectivity; and possibly more.
! TODO: AvD: GL2: add cell_methods to edge/face data (:mean)
! TODO: AvD: GL2: add integer variable Mesh2_edge_bc with :flag_meanings = "none closed dirichlet"; :flag_values = 0, 1, 2 ;
! TODO: AvD: GL2: move grid_mapping attribute to all data variables, not coordinate variables.

!! Conventions
   character(len=6), parameter :: UG_CONV_CF = 'CF-1.8'        !< Version of CF conventions currently adopted.
   character(len=9), parameter :: UG_CONV_UGRID = 'UGRID-1.0'     !< Version of UGRID conventions currently adopted.
   character(len=16), parameter :: UG_CONV_DELTARES = 'Deltares-0.10' !< Version of Deltares extension.
!! Conventions history Deltares-x.y:
! 0.10 (2019-08-21): Consistent renaming of variables and attribute names, singular instead of plural.
! 0.9 (2019-04-25): Duplicate mesh nodes on network (connection) nodes discouraged.
! 0.8 (2017-04-21): Initial version for 1D network extension to regular UGRID.

!! Error codes
   integer, parameter :: UG_NOERR = NF90_NOERR !< No error, success. It is convenient to have this identical to NF90_NOERR (==0).
   integer, parameter :: UG_SOMEERR = -1010 !< Some unspecified error.
   integer, parameter :: UG_INVALID_MESHNAME = -1011
   integer, parameter :: UG_INVALID_MESHDIMENSION = -1012
   integer, parameter :: UG_INVALID_DATALOCATION = -1013
   integer, parameter :: UG_ARRAY_TOOSMALL = -1014 !< If while getting data, the target array is too small for the amount of data that needs to be put into it.
   integer, parameter :: UG_ENOTVAR = -1015 !< Some variable was not found. Probably due to a native NF90_ENOTVAR.
   integer, parameter :: UG_VAR_TOOMANYFOUND = -1016 !< Multiple variables were found in an inquiry whereas only one was expected or requested.
   integer, parameter :: UG_INVALID_LAYERS = -1017
   integer, parameter :: UG_ENOTATT = -1018 !< Some attribute was not found. Probably due to a native NF90_ENOTATT.
   integer, parameter :: UG_ENOTDIM = -1019 !< Some dimension was not found. Probably due to a native NF90_EBADDIM
   integer, parameter :: UG_INVALID_CRS = -1030 !< Invalid/missing coordinate reference system (using default)
   integer, parameter :: UG_INVALID_NETNAME = -1031 !< Invalid network name
   integer, parameter :: UG_INVALID_CONTACTNAME = -1032 !< Invalid contact name
   integer, parameter :: UG_NOTIMPLEMENTED = -1099

!! Geometry options
   integer, parameter :: LAYERTYPE_OCEANSIGMA = 1 !< Dimensionless vertical ocean sigma coordinate.
   integer, parameter :: LAYERTYPE_Z = 2 !< Vertical coordinate for fixed z-layers.
   integer, parameter :: LAYERTYPE_OCEAN_SIGMA_Z = 3 !< Combined Z-Sigma layers (some sigma layers above certain level on top of z-layers).

!! Location types
   integer, parameter :: UG_LOC_NONE = 0 !< Mesh data location: nowhere at all (include only required mesh locations)
   integer, parameter :: UG_LOC_NODE = 1 !< Mesh data location: mesh node (corner)
   integer, parameter :: UG_LOC_EDGE = 2 !< Mesh data location: mesh edge
   integer, parameter :: UG_LOC_FACE = 4 !< Mesh data location: mesh face
   integer, parameter :: UG_LOC_VOL = 8 !< Mesh data location: mesh volume
   integer, parameter :: UG_LOC_CONTACT = 9 !< Mesh data location: 1d2d contacts
   integer, parameter :: UG_LOC_ALL2D = UG_LOC_NODE + UG_LOC_EDGE + UG_LOC_FACE !< All three possible 2D locations.

!> The following edge type codes define for each netlink (UGRID 'edge') the type (or absence) of flowlink.
!! Note: these are different from contact types (but intentionally the numbers do not overlap).
   integer, parameter :: UG_EDGETYPE_INTERNAL_CLOSED = 0 !< Edge that is closed/no flow link
   integer, parameter :: UG_EDGETYPE_INTERNAL = 1 !< Regular edge with an open flow link
   integer, parameter :: UG_EDGETYPE_BND = 2 !< Boundary edge with open boundary
   integer, parameter :: UG_EDGETYPE_BND_CLOSED = 3 !< Boundary edge with closed boundary

!! Dimension types (form a supplement to the preceding location types)
   integer, parameter :: UG_DIM_MAXFACENODES = 128 !< The dimension containing the max number of nodes in the face_node_connectivity table.
! TODO: AvD: the above is not a dimension. At most it is a dimension type.

!! Write options, intended to be specified as a single integer with a summation of the parameters below (i.e. as bit pattern)
   integer, parameter :: UG_WRITE_NOOPTS = 0 !< zero parameter for write options
   integer, parameter :: UG_WRITE_LATLON = 1 !< Automatically write also lat lon coordinates if the input is in some non-WGS84 projected system.
   integer, parameter :: UG_WRITE_LYRVAR = 2 !< Automatically write also layer coordinates as variables

!! Basics
   integer, parameter :: maxMessageLen = 1024
   character(len=maxMessageLen) :: ug_messagestr !< Placeholder string for storing diagnostic messages. /see{ug_get_message}

!> Type t_face describes a 'netcell', a cell with net nodes as vertices.
   type t_face
      integer :: n               !< nr of nodes
      integer, allocatable :: nod(:)          !< node nrs
      integer, allocatable :: lin(:)          !< link nrs, kn(1 of 2,netcell(n)%lin(1)) =  netcell(n)%nod(1)
   end type t_face

   type t_ug_file
      character(len=256) :: filename
      integer :: nummesh
      integer :: numcontacts
      integer :: numnet
      type(t_ug_mesh), allocatable :: meshids(:)         !< The type with underlying variable IDs (one column for each mesh topology).
      type(t_ug_network), allocatable :: netids(:)
      type(t_ug_contacts), allocatable :: contactids(:)      !< The array with underlying variable IDs, one column for each link topology.
      character(len=256), allocatable :: meshnames(:)       !< The variable names for all mesh topologies in file.
      character(len=256), allocatable :: networksnames(:)
      character(len=256), allocatable :: contactsnames(:)   !< The variable names for all contacts.
   end type t_ug_file

   integer :: imiss = -999

contains

!> Returns the latest message string from this module.
!!
!! Use this when a previous function call has returned a nonzero error status.
!! Call this function only once for each returned error: message buffer will be cleared on each call.
   integer function ug_get_message(str) result(ierr)
      character(len=*), intent(out) :: str !< String variable in which the message will be stored.

      ierr = UG_NOERR
      str = trim(ug_messagestr)

      ! Directly clear the message buffer, to prevent false messages for future errors.
      ug_messagestr = ' '

   end function ug_get_message

!> Given an error number, return an error message.
!!
!! Use this when a previous function call has returned a nonzero error status.
!! For a more detailed error message (including possible input arguments/filenames) consider using
!! ug_get_message instead.
!! \see ug_get_message
   function ug_strerror(ugerr) result(str)
      integer, intent(in) :: ugerr !< Integer error code for which to return the error message.
      character(len=:), allocatable :: str !< String variable in which the message will be stored.

      select case (ugerr)
      case (UG_NOERR); str = 'No error'
      case (UG_SOMEERR); str = 'Generic error'
      case (UG_INVALID_MESHNAME); str = 'Invalid or missing mesh name, or mesh not found'
      case (UG_INVALID_MESHDIMENSION); str = 'Invalid mesh topology dimension'
      case (UG_INVALID_DATALOCATION); str = 'Invalid topological data location'
      case (UG_ARRAY_TOOSMALL); str = 'Output array too small to store all values'
      case (UG_ENOTVAR); str = 'Variable not in dataset'
      case (UG_ENOTATT); str = 'Attribute not in dataset'
      case (UG_ENOTDIM); str = 'Dimension not in dataset'
      case (UG_VAR_TOOMANYFOUND); str = 'Too many matching variables found in dataset'
      case (UG_INVALID_LAYERS); str = 'Invalid layer type'
      case (UG_INVALID_CRS); str = 'Invalid coordinate reference system'
      case (UG_INVALID_NETNAME); str = 'Invalid network name'
      case (UG_INVALID_CONTACTNAME); str = 'Invalid contact name'
      case (UG_NOTIMPLEMENTED); str = 'Functionality not available yet (not implemented)'
      case default
         str = 'Unknown error'
      end select

   end function ug_strerror

!> Returns the integer value for a named constant.
!! When requested constant does not exist, the returned value is undefined, and ierr contains an error code.
   integer function ug_get_constant(constname, constvalue) result(ierr)
      character(len=*), intent(in) :: constname  !< The name of the requested constant.
      integer, intent(out) :: constvalue !< The integer value of the requested constant.

      ierr = UG_NOERR

      select case (trim(constname))
      case ('ug_strLenMeta'); constvalue = ug_strLenMeta
      case ('ug_idsLen'); constvalue = ug_idsLen
      case ('ug_idsLongNamesLen'); constvalue = ug_idsLongNamesLen
      case ('UG_NOERR'); constvalue = UG_NOERR
      case ('UG_SOMEERR'); constvalue = UG_SOMEERR
      case ('UG_INVALID_MESHNAME'); constvalue = UG_INVALID_MESHNAME
      case ('UG_INVALID_MESHDIMENSION'); constvalue = UG_INVALID_MESHDIMENSION
      case ('UG_INVALID_DATALOCATION'); constvalue = UG_INVALID_DATALOCATION
      case ('UG_ARRAY_TOOSMALL'); constvalue = UG_ARRAY_TOOSMALL
      case ('UG_ENOTVAR'); constvalue = UG_ENOTVAR
      case ('UG_ENOTATT'); constvalue = UG_ENOTATT
      case ('UG_ENOTDIM'); constvalue = UG_ENOTDIM
      case ('UG_VAR_TOOMANYFOUND'); constvalue = UG_VAR_TOOMANYFOUND
      case ('UG_INVALID_LAYERS'); constvalue = UG_INVALID_LAYERS
      case ('UG_INVALID_CRS'); constvalue = UG_INVALID_CRS
      case ('UG_NOTIMPLEMENTED'); constvalue = UG_NOTIMPLEMENTED
      case ('LAYERTYPE_OCEANSIGMA'); constvalue = LAYERTYPE_OCEANSIGMA
      case ('LAYERTYPE_Z'); constvalue = LAYERTYPE_Z
      case ('LAYERTYPE_OCEAN_SIGMA_Z'); constvalue = LAYERTYPE_OCEAN_SIGMA_Z
      case ('UG_LOC_NONE'); constvalue = UG_LOC_NONE
      case ('UG_LOC_NODE'); constvalue = UG_LOC_NODE
      case ('UG_LOC_EDGE'); constvalue = UG_LOC_EDGE
      case ('UG_LOC_FACE'); constvalue = UG_LOC_FACE
      case ('UG_LOC_VOL'); constvalue = UG_LOC_VOL
      case ('UG_LOC_ALL2D'); constvalue = UG_LOC_ALL2D
      case ('UG_EDGETYPE_INTERNAL_CLOSED'); constvalue = UG_EDGETYPE_INTERNAL_CLOSED
      case ('UG_EDGETYPE_INTERNAL'); constvalue = UG_EDGETYPE_INTERNAL
      case ('UG_EDGETYPE_BND'); constvalue = UG_EDGETYPE_BND
      case ('UG_EDGETYPE_BND_CLOSED'); constvalue = UG_EDGETYPE_BND_CLOSED
      case default
         ierr = UG_SOMEERR
      end select
   end function ug_get_constant

! Resets a complete t_ug_mesh type variable, making it ready for re-reading
! a dataset. Effectively resets all dimension and variable ids.
   function ug_reset_mesh(meshids) result(ierr)
      type(t_ug_mesh), intent(inout) :: meshids ! UGRID mesh variable to be reset.
      integer :: ierr    !< Result status (UG_NOERR==NF90_NOERR) if successful.

      ierr = UG_NOERR
      meshids%dimids(1:mdim_end) = -1
      meshids%varids(1:mid_end) = -1
   end function ug_reset_mesh

!
! -- Writing-related routines ---------------------------------------------
!

!> Puts global attributes in an open NetCDF data set.
!! This includes: institution, Conventions, etc.
   function ug_addglobalatts(ncid, meta) result(ierr)
      integer, intent(in) :: ncid              !< Already opened NetCDF id to put global attributes into.
      type(t_ug_meta), intent(in) :: meta
      integer :: ierr     !< Result status (UG_NOERR==NF90_NOERR) if successful.

      character(len=8) :: cdate
      character(len=10) :: ctime
      character(len=5) :: czone
      integer :: wasInDefine

      ierr = UG_NOERR
      wasInDefine = 0

      ierr = nf90_redef(ncid)
      if (ierr == nf90_eindefine) wasInDefine = 1 ! Was still in define mode.
      if (ierr /= nf90_noerr .and. ierr /= nf90_eindefine) then
         ug_messagestr = 'Could not put global attributes in NetCDF'
         return
      end if

      ierr = nf90_put_att(ncid, nf90_global, 'institution', trim(meta%institution))
      ierr = nf90_put_att(ncid, nf90_global, 'references', trim(meta%references))
      ierr = nf90_put_att(ncid, nf90_global, 'source', trim(meta%source)//' '//trim(meta%version)//'. Model: '//trim(meta%modelname))

      call date_and_time(cdate, ctime, czone)
      ierr = nf90_put_att(ncid, nf90_global, 'history', &
                          'Created on '//cdate(1:4)//'-'//cdate(5:6)//'-'//cdate(7:8)//'T'//ctime(1:2)//':'//ctime(3:4)//':'//ctime(5:6)//czone(1:5)// &
                          ', '//trim(meta%source))

      ierr = nf90_put_att(ncid, nf90_global, 'Conventions', trim(UG_CONV_CF)//' '//trim(UG_CONV_UGRID)//' '//trim(UG_CONV_DELTARES))

      ! Leave the dataset in the same mode as we got it.
      if (wasInDefine == 0) then
         ierr = nf90_enddef(ncid)
      end if
   end function ug_addglobalatts

! -- COORDINATES ------------
!> Adds coordinate variables according to CF conventions.
!! Non-standard attributes (such as bounds) should be set elsewhere.
   function ug_addcoordvars(ncid, id_varx, id_vary, id_dimension, name_varx, name_vary, longname_varx, longname_vary, mesh, location, crs) result(ierr)
      integer, intent(in) :: ncid          !< NetCDF dataset id
      integer, intent(inout) :: id_varx       !< NetCDF 'x' variable id
      integer, intent(inout) :: id_vary       !< NetCDF 'y' variable id
      integer, dimension(:), intent(in) :: id_dimension  !< NetCDF dimension id
      character(len=*), intent(in) :: name_varx     !< NetCDF 'x' variable name
      character(len=*), intent(in) :: name_vary     !< NetCDF 'y' variable name
      character(len=*), intent(in) :: longname_varx !< NetCDF 'x' variable long name
      character(len=*), intent(in) :: longname_vary !< NetCDF 'y' variable long name
      character(len=*), intent(in) :: mesh          !< Name of the mesh that contains the coordinate variables to add
      character(len=*), intent(in) :: location      !< location on the mesh of the coordinate variables to add
      type(t_crs), intent(in) :: crs           !< Coordinate reference system for the x/y-coordinates variables.
      integer :: ierr          !< Result status (UG_NOERR==NF90_NOERR) if successful.

      ierr = UG_NOERR

      ierr = nf90_def_var(ncid, name_varx, nf90_double, id_dimension, id_varx)
      ierr = nf90_def_var(ncid, name_vary, nf90_double, id_dimension, id_vary)
      ierr = ug_addcoordatts(ncid, id_varx, id_vary, crs)

      ! UNST-2791: until further notice we will *not* write :mesh and :location attributes for UGRID coordinate variables anymore.
      !ierr = nf90_put_att(ncid, id_varx, 'mesh',      mesh)
      !ierr = nf90_put_att(ncid, id_vary, 'mesh',      mesh)
      !ierr = nf90_put_att(ncid, id_varx, 'location',  location)
      !ierr = nf90_put_att(ncid, id_vary, 'location',  location)

      ierr = nf90_put_att(ncid, id_varx, 'long_name', longname_varx)
      ierr = nf90_put_att(ncid, id_vary, 'long_name', longname_vary)
   end function ug_addcoordvars

!> Adds WGS84 coordinate variables according to CF conventions.
!! Non-standard attributes (such as bounds) should be set elsewhere.
   function ug_addlonlatcoordvars(ncid, id_varlon, id_varlat, id_dimension, name_varlon, name_varlat, longname_varlon, longname_varlat, mesh, location) result(ierr)
      integer, intent(in) :: ncid            !< NetCDF dataset id
      integer, intent(inout) :: id_varlon       !< NetCDF 'lon' variable id
      integer, intent(inout) :: id_varlat       !< NetCDF 'lat' variable id
      integer, dimension(:), intent(in) :: id_dimension    !< NetCDF dimension id
      character(len=*), intent(in) :: name_varlon     !< NetCDF 'lon' variable name
      character(len=*), intent(in) :: name_varlat     !< NetCDF 'lat' variable name
      character(len=*), intent(in) :: longname_varlon !< NetCDF 'lon' variable long name
      character(len=*), intent(in) :: longname_varlat !< NetCDF 'lat' variable long name
      character(len=*), optional, intent(in) :: mesh            !< (Optional) Name of the mesh that contains the coordinate variables to add.
      character(len=*), optional, intent(in) :: location        !< (Optional) Location on the mesh of the coordinate variables to add.
      integer :: ierr            !< Result status (UG_NOERR==NF90_NOERR) if successful.

      ierr = UG_NOERR

      ierr = nf90_def_var(ncid, name_varlon, nf90_double, id_dimension, id_varlon)
      ierr = nf90_def_var(ncid, name_varlat, nf90_double, id_dimension, id_varlat)
      ierr = ug_addlonlatcoordatts(ncid, id_varlon, id_varlat)

      ! UNST-2791: until further notice we will *not* write :mesh and :location attributes for UGRID coordinate variables anymore.
      !if (present(mesh)) then
      !   ierr = nf90_put_att(ncid, id_varlon, 'mesh',      mesh)
      !   ierr = nf90_put_att(ncid, id_varlat, 'mesh',      mesh)
      !end if
      !if (present(location)) then
      !   ierr = nf90_put_att(ncid, id_varlon, 'location',  location)
      !   ierr = nf90_put_att(ncid, id_varlat, 'location',  location)
      !end if

      ierr = nf90_put_att(ncid, id_varlon, 'long_name', longname_varlon)
      ierr = nf90_put_att(ncid, id_varlat, 'long_name', longname_varlat)
   end function ug_addlonlatcoordvars

!> Adds coordinate attributes according to CF conventions, based on given coordinate projection type.
!! Non-standard attributes (such as long_name) should be set elsewhere.
   function ug_addcoordatts(ncid, id_varx, id_vary, crs) result(ierr)
      integer, intent(in) :: ncid     !< NetCDF dataset id
      integer, intent(in) :: id_varx  !< NetCDF 'x' variable id
      integer, intent(in) :: id_vary  !< NetCDF 'y' variable id
      type(t_crs), intent(in) :: crs           !< Coordinate reference system for the x/y-coordinates variables.
      integer :: ierr     !< Result status (UG_NOERR==NF90_NOERR) if successful.

      ierr = UG_NOERR

      if (crs%epsg_code == 4326) then ! epsg 4326 is assumed spherical, WGS84 system.
         ierr = ug_addlonlatcoordatts(ncid, id_varx, id_vary)
      else ! If projected.
         ierr = nf90_put_att(ncid, id_varx, 'units', 'm')
         ierr = nf90_put_att(ncid, id_vary, 'units', 'm')
         ierr = nf90_put_att(ncid, id_varx, 'standard_name', 'projection_x_coordinate')
         ierr = nf90_put_att(ncid, id_vary, 'standard_name', 'projection_y_coordinate')
         ierr = nf90_put_att(ncid, id_varx, 'long_name', 'x')
         ierr = nf90_put_att(ncid, id_vary, 'long_name', 'y')
      end if
   end function ug_addcoordatts

!> Adds WGS84 coordinate attributes according to CF conventions.
   function ug_addlonlatcoordatts(ncid, id_varlon, id_varlat) result(ierr)
      integer, intent(in) :: ncid      !< NetCDF dataset id
      integer, intent(in) :: id_varlon !< NetCDF 'longitude' variable id
      integer, intent(in) :: id_varlat !< NetCDF 'latitude' variable id
      integer :: ierr      !< Result status (UG_NOERR==NF90_NOERR) if successful.

      ierr = UG_NOERR

      ierr = nf90_put_att(ncid, id_varlon, 'units', 'degrees_east')
      ierr = nf90_put_att(ncid, id_varlat, 'units', 'degrees_north')
      ierr = nf90_put_att(ncid, id_varlon, 'standard_name', 'longitude')
      ierr = nf90_put_att(ncid, id_varlat, 'standard_name', 'latitude')
      ierr = nf90_put_att(ncid, id_varlon, 'long_name', 'longitude')
      ierr = nf90_put_att(ncid, id_varlat, 'long_name', 'latitude')
   end function ug_addlonlatcoordatts

!> Adds coordinate mapping attributes according to CF conventions, based on jsferic.
!! Attributes are put in a scalar integer variable.
   function ug_add_coordmapping(ncid, crs) result(ierr)
      use netcdf_utils, only: ncu_put_var_attset

      integer, intent(in) :: ncid   !< NetCDF dataset id
      type(t_crs), intent(in) :: crs   !< Coordinate reference system that was used for the coordinate mapping.
      integer :: ierr   !< Result status (UG_NOERR==NF90_NOERR) if successful.
      integer :: id_crs
      integer :: ierr_missing
      character(len=11) :: epsgstring
      character(len=30) :: varname  !< Name of the created grid mapping variable.

      ierr = UG_NOERR
      ierr_missing = UG_NOERR ! Store whether crs was missing (and default was used)

      epsgstring = ' '
      varname = ' '
      if (crs%epsg_code == 4326) then ! epsg 4326 is assumed spherical
         ierr_missing = UG_INVALID_CRS
         varname = 'wgs84'
      else if (len_trim(crs%varname) > 0) then
         varname = crs%varname
      else
         ierr_missing = UG_INVALID_CRS
         varname = 'projected_coordinate_system'
      end if

      ierr = nf90_inq_varid(ncid, trim(varname), id_crs)
      if (ierr == nf90_noerr) then
         ! A variable with that name already exists. Return without error.
         ierr = UG_NOERR
         goto 888
      end if

      ierr = nf90_def_var(ncid, trim(varname), nf90_int, id_crs)

      !The meta info other than epsg code should be retrived from proj4 library, as done in Delta Shell
      !otherwise we will generate inconsistent information
      if (crs%epsg_code == 4326) then
         ierr_missing = UG_INVALID_CRS
         write (epsgstring, '("EPSG:",I0)') crs%epsg_code
         ierr = nf90_put_att(ncid, id_crs, 'name', 'WGS84') ! CF
         ierr = nf90_put_att(ncid, id_crs, 'epsg', crs%epsg_code) ! CF
         ierr = nf90_put_att(ncid, id_crs, 'grid_mapping_name', 'latitude_longitude') ! CF
         ierr = nf90_put_att(ncid, id_crs, 'longitude_of_prime_meridian', 0d0) ! CF
         ierr = nf90_put_att(ncid, id_crs, 'semi_major_axis', 6378137d0) ! CF
         ierr = nf90_put_att(ncid, id_crs, 'semi_minor_axis', 6356752.314245d0) ! CF
         ierr = nf90_put_att(ncid, id_crs, 'inverse_flattening', 298.257223563d0) ! CF
         ierr = nf90_put_att(ncid, id_crs, 'EPSG_code', trim(epsgstring)) ! ADAGUC
!      ierr = nf90_put_att(ncid, id_crs, 'projection_name',             ' '                ) ! ADAGUC
!      ierr = nf90_put_att(ncid, id_crs, 'wkt',                         ' '                ) ! WKT
!      ierr = nf90_put_att(ncid, id_crs, 'comment',                     ' '                )
         ierr = nf90_put_att(ncid, id_crs, 'value', 'value is equal to EPSG code')
      else if (allocated(crs%attset)) then
         ierr = ncu_put_var_attset(ncid, id_crs, crs%attset)
      else
         ierr_missing = UG_INVALID_CRS      ! TODO: remove hardcoded defaults below. Replace by cloning the crs%attset  into this new NetCDF var.
         write (epsgstring, '("EPSG:",I0)') crs%epsg_code
         ierr = nf90_put_att(ncid, id_crs, 'name', 'Unknown projected') ! CF
         ierr = nf90_put_att(ncid, id_crs, 'epsg', crs%epsg_code) ! CF
         ierr = nf90_put_att(ncid, id_crs, 'grid_mapping_name', 'Unknown projected') ! CF
         ierr = nf90_put_att(ncid, id_crs, 'longitude_of_prime_meridian', 0d0) ! CF
         ierr = nf90_put_att(ncid, id_crs, 'semi_major_axis', 6378137d0) ! CF
         ierr = nf90_put_att(ncid, id_crs, 'semi_minor_axis', 6356752.314245d0) ! CF
         ierr = nf90_put_att(ncid, id_crs, 'inverse_flattening', 298.257223563d0) ! CF
         ierr = nf90_put_att(ncid, id_crs, 'EPSG_code', trim(epsgstring)) ! ADAGUC
!      ierr = nf90_put_att(ncid, id_crs, 'projection_name',             ' '                 ) ! ADAGUC
!      ierr = nf90_put_att(ncid, id_crs, 'wkt',                         ' '                 ) ! WKT
!      ierr = nf90_put_att(ncid, id_crs, 'comment',                     ' '                 )
         ierr = nf90_put_att(ncid, id_crs, 'value', 'value is equal to EPSG code')
      end if
      if (len_trim(crs%proj_string) > 0) then
         ierr = nf90_put_att(ncid, id_crs, 'proj4_params', trim(crs%proj_string)) ! ADAGUC
      end if

      if (ierr_missing /= UG_NOERR) then
         ierr = ierr_missing
         ug_messagestr = 'Missing coordinate reference system. Now using default: '//trim(varname)//' ('//trim(epsgstring)//').'
         ! But continue...
      end if

      ! Check for any remaining native NetCDF errors
      if (ierr /= nf90_noerr) then
         goto 888
      end if

      ierr = UG_NOERR
      return ! Return with success

      ! TODO: AvD: actual epsg value is not put in variable value yet (redef stuff)

888   continue

   end function ug_add_coordmapping

!> Add the grid mapping attribute to one or more NetCDF variables.
   function ug_put_gridmapping_att(ncid, id_vars, crs) result(ierr)
      integer, intent(in) :: ncid     !< NetCDF dataset id
      integer, dimension(:), intent(in) :: id_vars  !< Array of NetCDF variable ids
      type(t_crs), intent(in) :: crs      !< Projection type that was used for the coordinate mapping.
      integer :: ierr     !< Result status (UG_NOERR==NF90_NOERR) if successful.

      integer :: i, n
      character(len=30) :: gridmappingvar           !< Name of grid mapping variable

      ierr = UG_SOMEERR
      gridmappingvar = crs%varname

      ierr = UG_NOERR
      n = size(id_vars)

      do i = 1, n
         ierr = nf90_put_att(ncid, id_vars(i), 'grid_mapping', trim(gridmappingvar))
         if (ierr /= nf90_noerr) then
            goto 888
         end if
      end do

      ierr = UG_NOERR
      return ! Return with success

888   continue

   end function ug_put_gridmapping_att

!> Checks whether a specific mesh data location is inside a location specification code.
!! Mesh data may be specified on nodes (corners), edges and faces, encoded as a sum of location codes.
!! Used to decide which optional mesh topology data should be written to file, and which not.
!! \see UG_LOC_NODE, UG_LOC_EDGE, UG_LOC_FACE
   function ug_checklocation(dataLocsCode, locType) result(is_used)
      integer, intent(in) :: dataLocsCode  !< Integer code describing on which topological locations data is/will be used.
      integer, intent(in) :: locType       !< Integer location code to test on (one of UG_LOC_NODE, UG_LOC_EDGE, UG_LOC_FACE, UG_LOC_VOL).
      logical :: is_used       !< Returns whether specified locType is contained in dataLocsCode.

      ! Perform logical AND to determine whether locType is inside dataLocs 'set'.
      is_used = iand(dataLocsCode, locType) == locType
   end function ug_checklocation

!> Translates the string name of a topological location into the integer location type.
   subroutine ug_location_to_loctype(locName, locType)
      character(len=*), intent(in) :: locName !< String name of the location, e.g., as read from a :location attribute value.
      integer, intent(out) :: locType !< Integer location code (one of UG_LOC_NODE, UG_LOC_EDGE, UG_LOC_FACE, UG_LOC_VOL).

      select case (trim(locName))
      case ('face')
         locType = UG_LOC_FACE
      case ('edge')
         locType = UG_LOC_EDGE
      case ('node')
         locType = UG_LOC_NODE
      case ('volume')
         locType = UG_LOC_VOL
      case default
         locType = UG_LOC_NONE
      end select

   end subroutine ug_location_to_loctype

!> Translates the integer location type to a topological location.
   subroutine ug_loctype_to_location(locType, locName)
      character(len=*), intent(out) :: locName !< String name of the location.
      integer, intent(in) :: locType !< Integer location code (one of UG_LOC_NODE, UG_LOC_EDGE, UG_LOC_FACE, UG_LOC_VOL).

      select case (locType)
      case (UG_LOC_FACE)
         locName = 'face'
      case (UG_LOC_EDGE)
         locName = 'edge'
      case (UG_LOC_NODE)
         locName = 'node'
      case (UG_LOC_VOL)
         locName = 'volume'
      case default
         locName = 'not implemented'
      end select

   end subroutine ug_loctype_to_location

!> Write mesh topoplogy
!! This only writes the mesh topology variable, not the other variables that are part of the mesh.
   function ug_write_meshtopology(ncid, meshids, meshName, dim, dataLocsCode, add_edge_face_connectivity, add_face_edge_connectivity, add_face_face_connectivity, add_layers, add_latlon) result(ierr)
      implicit none

      integer, intent(in) :: ncid       !< NetCDF dataset id
      type(t_ug_mesh), intent(inout) :: meshids !< Set of NetCDF-ids for all mesh geometry arrays.
      character(len=*), intent(in) :: meshName     !< Name for the mesh variable, also used as prefix for all related entities.
      integer, intent(in) :: dim          !< Dimensionality of the mesh (1/2/3)
      integer, intent(in) :: dataLocsCode !< Specifies at which mesh locations data may be specified.
      logical, intent(in) :: add_edge_face_connectivity !< Specifies whether edge_face_connectivity should be added.
      logical, intent(in) :: add_face_edge_connectivity !< Specifies whether face_edge_connectivity should be added.
      logical, intent(in) :: add_face_face_connectivity !< Specifies whether face_face_connectivity should be added.
      logical, intent(in) :: add_layers   !< Specifies whether layer and interface vertical dimensions should be added.
      logical, intent(in) :: add_latlon   !< Specifies whether latlon coordinate reference should be added.
      integer :: ierr         !< Result status (UG_NOERR==NF90_NOERR) if successful.

      character(len=len_trim(meshName)) :: prefix
      character(len=256) :: buffer

      ierr = UG_SOMEERR

      prefix = trim(meshName)

      if (len_trim(meshName) == 0) then
         ierr = UG_INVALID_MESHNAME
         goto 888
      end if

      if (dim <= 0 .or. dim > 3) then
         ierr = UG_INVALID_MESHDIMENSION
         goto 888
      end if

      ! TODO: AvD: check for conflicts between dataLocsCode and dim (e.g. FACE data in a 1D model)

      ! Define the mesh topology variable
      ierr = nf90_def_var(ncid, prefix, nf90_int, meshids%varids(mid_meshtopo))

      ! Attributes for all dimensions:
      ierr = nf90_put_att(ncid, meshids%varids(mid_meshtopo), 'cf_role', 'mesh_topology')
      write (buffer, '(a,i0,a)') 'Topology data of ', dim, 'D mesh'
      ierr = nf90_put_att(ncid, meshids%varids(mid_meshtopo), 'long_name', trim(buffer))
      ierr = nf90_put_att(ncid, meshids%varids(mid_meshtopo), 'topology_dimension', dim)
      if (add_latlon) then
         ierr = nf90_put_att(ncid, meshids%varids(mid_meshtopo), 'node_coordinates', prefix//'_node_x '//prefix//'_node_y '//prefix//'_node_lon '//prefix//'_node_lat')
      else
         ierr = nf90_put_att(ncid, meshids%varids(mid_meshtopo), 'node_coordinates', prefix//'_node_x '//prefix//'_node_y')
      end if
      ierr = nf90_put_att(ncid, meshids%varids(mid_meshtopo), 'node_dimension', prefix//'_nNodes')
      ierr = nf90_put_att(ncid, meshids%varids(mid_meshtopo), 'max_face_nodes_dimension', prefix//'_nMax_face_nodes') ! non ugrid standard!

      ! 1D: required, 2D: optionally required if data there
      if (dim == 1 .or. ug_checklocation(dataLocsCode, UG_LOC_EDGE)) then
         ierr = nf90_put_att(ncid, meshids%varids(mid_meshtopo), 'edge_node_connectivity', prefix//'_edge_nodes')
         ierr = nf90_put_att(ncid, meshids%varids(mid_meshtopo), 'edge_dimension', prefix//'_nEdges')
      end if

      ! Optionally required if data there:
      if (ug_checklocation(dataLocsCode, UG_LOC_EDGE)) then
         if (add_latlon) then
            ierr = nf90_put_att(ncid, meshids%varids(mid_meshtopo), 'edge_coordinates', prefix//'_edge_x '//prefix//'_edge_y '//prefix//'_edge_lon '//prefix//'_edge_lat')
         else
            ierr = nf90_put_att(ncid, meshids%varids(mid_meshtopo), 'edge_coordinates', prefix//'_edge_x '//prefix//'_edge_y')
         end if
      end if

      ! 2D: required, 3D: optionally required if data there:
      if (dim == 2 .or. ug_checklocation(dataLocsCode, UG_LOC_FACE)) then
         ierr = nf90_put_att(ncid, meshids%varids(mid_meshtopo), 'face_node_connectivity', prefix//'_face_nodes')
         ierr = nf90_put_att(ncid, meshids%varids(mid_meshtopo), 'face_dimension', prefix//'_nFaces')
         if (add_face_edge_connectivity) then
            ierr = nf90_put_att(ncid, meshids%varids(mid_meshtopo), 'face_edge_connectivity', prefix//'_face_edges')
         end if
         if (add_face_face_connectivity) then
            ierr = nf90_put_att(ncid, meshids%varids(mid_meshtopo), 'face_face_connectivity', prefix//'_face_links')
         end if
         ! Note that edge_face_connectivity is not officially part of the UGRID conventions, however it is very similar to e.g. face_edge_connectivity, which is part of the UGRID conventions.
         if (add_edge_face_connectivity) then
            ierr = nf90_put_att(ncid, meshids%varids(mid_meshtopo), 'edge_face_connectivity', prefix//'_edge_faces')
         end if
      end if

      ! Optionally required if data there:
      if (ug_checklocation(dataLocsCode, UG_LOC_FACE)) then
         if (add_latlon) then
            ierr = nf90_put_att(ncid, meshids%varids(mid_meshtopo), 'face_coordinates', prefix//'_face_x '//prefix//'_face_y '//prefix//'_face_lon '//prefix//'_face_lat')
         else
            ierr = nf90_put_att(ncid, meshids%varids(mid_meshtopo), 'face_coordinates', prefix//'_face_x '//prefix//'_face_y')
         end if
      end if

      ! Optionally required if layers present (1D or 2D layered mesh topology):
      if (add_layers) then
         ierr = nf90_put_att(ncid, meshids%varids(mid_meshtopo), 'layer_dimension', prefix//'_nLayers')
         ierr = nf90_put_att(ncid, meshids%varids(mid_meshtopo), 'interface_dimension', prefix//'_nInterfaces')
         ierr = nf90_put_att(ncid, meshids%varids(mid_meshtopo), 'vertical_dimensions', prefix//'_nLayers: '//prefix//'_nInterfaces (padding: none)')
      end if

      if (dim >= 3) then
         ierr = UG_NOTIMPLEMENTED
         goto 888
      end if

      ! Check for any remaining native NetCDF errors
      if (ierr /= nf90_noerr) then
         goto 888
      end if

      ierr = UG_NOERR
      return ! Return with success

888   continue

   end function ug_write_meshtopology

!> Defines a new variable in an existing dataset.
!! Does not write the actual data yet.
   function ug_def_var(ncid, id_var, id_dims, itype, iloctype, mesh_name, var_name, standard_name, long_name, &
                       units, cell_method, cell_measures, crs, ifill, dfill, writeopts, do_deflate) result(ierr)
      use precision, only: sp

      integer, intent(in) :: ncid          !< NetCDF dataset id
      integer, intent(out) :: id_var        !< Created NetCDF variable id.
      integer, dimension(:), intent(in) :: id_dims       !< NetCDF dimension ids for this variable. Example: (/ id_edgedim /) for scalar data on edges, or (/ id_twodim, id_facedim /) for vector data on faces.
      integer, intent(in) :: itype         !< The variable type expressed in one of the basic nf90_* types, e.g., nf90_double.
      integer, intent(in) :: iloctype      !< Specifies at which unique mesh location data will be specified.
      character(len=*), intent(in) :: mesh_name     !< Name for the mesh variable, also used as prefix for all related entities.
      character(len=*), intent(in) :: var_name      !< Name for the new data variable.
      character(len=*), intent(in) :: standard_name !< Standard name (CF-compliant) for 'standard_name' attribute in this variable.
      character(len=*), intent(in) :: long_name     !< Long name for 'long_name' attribute in this variable (use empty string if not wanted).
      character(len=*), intent(in) :: units         !< Units of this variable (CF-compliant) (use empty string for dimensionless quantities).
      character(len=*), intent(in) :: cell_method   !< Cell method for the spatial dimension (i.e., for edge/face/volume), value should be one of 'point', 'mean', etc. (See CF) (empty string if not relevant).
      character(len=*), intent(in) :: cell_measures !< Cell measures attribute, for example: 'area: mesh2d_cellarea', etc. (See CF) (empty string if not relevant).
      type(t_crs), optional, intent(in) :: crs        !< (Optional) Add grid_mapping attribute based on this coordinate reference system for independent coordinates
      integer, optional, intent(in) :: ifill         !< (Optional) Integer fill value.
      double precision, optional, intent(in) :: dfill         !< (Optional) Double precision fill value.
      integer, optional, intent(in) :: writeopts  !< integer option, currently only: UG_WRITE_LATLON
      logical, optional, intent(in) :: do_deflate !< whether or not to apply compression (deflation)
      integer :: ierr          !< Result status (UG_NOERR==NF90_NOERR) if successful.

      character(len=len_trim(mesh_name)) :: prefix
      integer :: wasInDefine
      logical :: add_latlon

      logical :: do_deflate_

      ierr = UG_SOMEERR

      wasInDefine = 0

      ierr = nf90_redef(ncid)
      if (ierr == nf90_eindefine) then
         wasInDefine = 1 ! Was still in define mode.
      end if

      if (ierr /= nf90_noerr .and. ierr /= nf90_eindefine) then
         goto 888
      end if

#ifdef HAVE_PROJ
      if (present(crs) .and. present(writeopts)) then
         add_latlon = crs%epsg_code /= 4326 .and. iand(writeopts, UG_WRITE_LATLON) == UG_WRITE_LATLON
      else
         add_latlon = .false.
      end if
#else
      add_latlon = .false.
#endif

      prefix = trim(mesh_name)

      if (present(do_deflate)) then
         do_deflate_ = do_deflate
      else
         do_deflate_ = .false.
      end if

      if (do_deflate_) then
         ! Define variable with shuffle and deflation. Chunk size is by default set to a single timeframe.
         ! TB: some small-scale tests indicate that deflate_level > 1 barely reduces the file size any further.
         ierr = nf90_def_var(ncid=ncid, name=prefix//'_'//trim(var_name), xtype=itype, dimids=id_dims, varid=id_var, &
                             shuffle=.true., deflate_level=1)
      else
         ierr = nf90_def_var(ncid=ncid, name=prefix//'_'//trim(var_name), xtype=itype, dimids=id_dims, varid=id_var)
      end if

      if (ierr /= nf90_noerr) then
         goto 888
      end if
      ierr = nf90_put_att(ncid, id_var, 'mesh', trim(mesh_name))
      select case (iloctype)
      case (UG_LOC_NODE)
         ierr = nf90_put_att(ncid, id_var, 'location', 'node')
         if (add_latlon) then
            ierr = nf90_put_att(ncid, id_var, 'coordinates', prefix//'_node_x '//prefix//'_node_y '//prefix//'_node_lon '//prefix//'_node_lat')
         else
            ierr = nf90_put_att(ncid, id_var, 'coordinates', prefix//'_node_x '//prefix//'_node_y')
         end if
         if (len_trim(cell_method) > 0) then
            ierr = nf90_put_att(ncid, id_var, 'cell_methods', prefix//'_nNodes: '//trim(cell_method))
         end if
         if (len_trim(cell_measures) > 0) then
            ierr = nf90_put_att(ncid, id_var, 'cell_measures', trim(cell_measures))
         end if
      case (UG_LOC_EDGE)
         ierr = nf90_put_att(ncid, id_var, 'location', 'edge')
         if (add_latlon) then
            ierr = nf90_put_att(ncid, id_var, 'coordinates', prefix//'_edge_x '//prefix//'_edge_y '//prefix//'_edge_lon '//prefix//'_edge_lat')
         else
            ierr = nf90_put_att(ncid, id_var, 'coordinates', prefix//'_edge_x '//prefix//'_edge_y')
         end if
         if (len_trim(cell_method) > 0) then
            ierr = nf90_put_att(ncid, id_var, 'cell_methods', prefix//'_nEdges: '//trim(cell_method))
         end if
         if (len_trim(cell_measures) > 0) then
            ierr = nf90_put_att(ncid, id_var, 'cell_measures', trim(cell_measures))
         end if
      case (UG_LOC_FACE)
         ierr = nf90_put_att(ncid, id_var, 'location', 'face')
         if (add_latlon) then
            ierr = nf90_put_att(ncid, id_var, 'coordinates', prefix//'_face_x '//prefix//'_face_y '//prefix//'_face_lon '//prefix//'_face_lat')
         else
            ierr = nf90_put_att(ncid, id_var, 'coordinates', prefix//'_face_x '//prefix//'_face_y')
         end if
         if (len_trim(cell_method) > 0) then
            ierr = nf90_put_att(ncid, id_var, 'cell_methods', prefix//'_nFaces: '//trim(cell_method))
         end if
         if (len_trim(cell_measures) > 0) then
            ierr = nf90_put_att(ncid, id_var, 'cell_measures', trim(cell_measures))
         end if
      case (UG_LOC_CONTACT)
         ierr = nf90_put_att(ncid, id_var, 'location', 'contact')
         ierr = nf90_put_att(ncid, id_var, 'coordinates', prefix//'_contact_id')
         ! TODO: AvD: UNST-1100: cell_measures for contacts not yet supported/well defined...
      case (UG_LOC_VOL)
         ierr = UG_NOTIMPLEMENTED
         goto 888
      case default
         ierr = UG_INVALID_DATALOCATION
         goto 888
      end select

      if (len_trim(standard_name) > 0) then
         ierr = nf90_put_att(ncid, id_var, 'standard_name', trim(standard_name))
      end if
      if (len_trim(long_name) > 0) then
         ierr = nf90_put_att(ncid, id_var, 'long_name', trim(long_name))
      end if
      if (len_trim(units) > 0) then
         ierr = nf90_put_att(ncid, id_var, 'units', trim(units))
      end if

      if (present(crs)) then
         ierr = ug_put_gridmapping_att(ncid, (/id_var/), crs)
      end if
      if (itype == nf90_int .and. present(ifill)) then
         ierr = nf90_put_att(ncid, id_var, '_FillValue', ifill)
      end if
      if (itype == nf90_double .and. present(dfill)) then
         ierr = nf90_put_att(ncid, id_var, '_FillValue', dfill)
      end if
      if (itype == nf90_float .and. present(dfill)) then
         ierr = nf90_put_att(ncid, id_var, '_FillValue', real(dfill, sp))
      end if

      ! Leave the dataset in the same mode as we got it.
      if (wasInDefine == 0) then
         ierr = nf90_enddef(ncid)
      end if

      ! Check for any remaining native NetCDF errors
      if (ierr /= nf90_noerr) then
         goto 888
      end if

      ierr = UG_NOERR
      return ! Return with success

888   continue

   end function ug_def_var

!> Writes a complete mesh geometry to an open NetCDF data set.
!! The mesh geometry is the required starting point for all variables/data defined ON that mesh.
!! This function accepts the mesh geometry derived type as input, for the arrays-based function, see ug_write_mesh_arrays
!! This only writes the mesh variables, not the actual data variables that are defined ON the mesh.
   function ug_write_mesh_struct(ncid, meshids, networkids, crs, meshgeom, nnodeids, nbranchids, nnodelongnames, nbranchlongnames, nodeids, nodelongnames, network1dname) result(ierr)
      integer, intent(in) :: ncid     !< NetCDF dataset id, should be already open and ready for writing.
      type(t_ug_mesh), intent(inout) :: meshids  !< Set of NetCDF-ids for all mesh geometry arrays.
      type(t_ug_network), intent(inout) :: networkids  !< Set of NetCDF-ids for all mesh geometry arrays.
      type(t_ug_meshgeom), intent(in) :: meshgeom !< The complete mesh geometry in a single struct.
      type(t_crs), intent(in) :: crs      !< Optional crs containing metadata of unsupported coordinate reference systems
      integer :: ierr     !< Result status (UG_NOERR==NF90_NOERR) if successful.
!   integer, optional, intent(in)      :: nsigma_opt !< Number of
      character(len=ug_idsLen), optional, allocatable :: nnodeids(:), nbranchids(:), nodeids(:)
      character(len=ug_idsLongNamesLen), optional, allocatable :: nnodelongnames(:), nbranchlongnames(:), nodelongnames(:)
      character(len=*), optional :: network1dname

      ierr = ug_write_mesh_arrays(ncid, meshids, meshgeom%meshName, meshgeom%dim, UG_LOC_ALL2D, meshgeom%numNode, meshgeom%numEdge, meshgeom%numFace, meshgeom%maxNumFaceNodes, &
                                  meshgeom%edge_nodes, meshgeom%face_nodes, meshgeom%edge_faces, meshgeom%face_edges, meshgeom%face_links, meshgeom%nodex, meshgeom%nodey, & ! meshgeom%nodez, &
                                  meshgeom%edgex, meshgeom%edgey, meshgeom%facex, meshgeom%facey, &
                                  crs, -999, -999d0, meshgeom%start_index, meshgeom%num_layers, meshgeom%layertype, meshgeom%layer_zs, meshgeom%interface_zs, &
                                  networkids, network1dname, meshgeom%nnodex, meshgeom%nnodey, nnodeids, nnodelongnames, &
                                  meshgeom%nedge_nodes(1, :), meshgeom%nedge_nodes(2, :), nbranchids, nbranchlongnames, meshgeom%nbranchlengths, meshgeom%nbranchgeometrynodes, meshgeom%nbranches, &
                                  meshgeom%ngeopointx, meshgeom%ngeopointy, meshgeom%ngeometry, &
                                  meshgeom%nbranchorder, &
                                  nodeids, nodelongnames, meshgeom%nodebranchidx, meshgeom%nodeoffsets, meshgeom%edgebranchidx, meshgeom%edgeoffsets, &
                                  zn=meshgeom%nodez, nsigma_opt=meshgeom%numtopsig)

   end function ug_write_mesh_struct

!> Writes a complete mesh geometry to an open NetCDF data set based on separate arrays with all mesh data.
!! The mesh geometry is the required starting point for all variables/data defined ON that mesh.
!! This function requires all mesh arrays as input, for the derived type-based function, see ug_write_mesh_struct.
!! This only writes the mesh variables, not the actual data variables that are defined ON the mesh.
   function ug_write_mesh_arrays(ncid, meshids, meshName, dim, dataLocs, numNode, numEdge, numFace, maxNumNodesPerFace, &
                                 edge_nodes, face_nodes, edge_faces, face_edges, face_links, xn, yn, xe, ye, xf, yf, &
                                 crs, imiss, dmiss, start_index, num_layers, layerType, layer_zs, interface_zs, &
                                 networkids, network1dname, nnodex, nnodey, nnodeids, nnodelongnames, &
                                 sourceNodeId, targetNodeId, nbranchids, nbranchlongnames, nbranchlengths, nbranchgeometrynodes, nbranches, &
                                 ngeopointx, ngeopointy, ngeometry, &
                                 nbranchorder, &
                                 nodeids, nodelongnames, nodebranchidx, nodeoffsets, edgebranchidx, edgeoffsets, &
                                 nsigma_opt, waterlevelname, bldepthname, writeopts, zn) result(ierr)
      use m_alloc
      use string_module

      implicit none

      integer, intent(in) :: ncid     !< NetCDF dataset id, should be already open and ready for writing.
      type(t_ug_mesh), intent(inout) :: meshids !< Set of NetCDF-ids for all mesh geometry arrays.
      character(len=*), intent(in) :: meshName !< Name for the mesh variable, also used as prefix for all related entities.
      integer, intent(in) :: dim      !< Dimensionality of the mesh (1/2/3)
      integer, intent(in) :: dataLocs !< Integer code describing on which topological locations data is/will be used.
      integer, intent(in) :: numNode  !< Number of nodes in the mesh.
      integer, intent(in) :: numEdge  !< Number of edges in the mesh.
      integer, intent(in) :: numFace  !< Number of faces in the mesh.
      integer, intent(in) :: maxNumNodesPerFace  !< Maximum number of nodes per face in the mesh.
      integer, intent(in) :: edge_nodes(:, :) !< Edge-to-node mapping array.
      integer, intent(in) :: face_nodes(:, :) !< Face-to-node mapping array.
      integer, pointer, intent(in) :: edge_faces(:, :) !< Edge-to-face mapping array (optional, can be null()).
      integer, pointer, intent(in) :: face_edges(:, :) !< Face-to-edge mapping array (optional, can be null()).
      integer, pointer, intent(in) :: face_links(:, :) !< Face-to-face mapping array (optional, can be null()).
      real(kind=dp), intent(in) :: xn(:), yn(:) !< x,y-coordinates of the mesh nodes.
      real(kind=dp), intent(in) :: xe(:), ye(:) !< representative x,y-coordinates of the mesh edges.
      real(kind=dp), intent(in) :: xf(:), yf(:) !< representative x,y-coordinates of the mesh faces.
      type(t_crs), intent(in) :: crs      !< Coordinate reference system for input coordinates
      integer, intent(in) :: imiss    !< Fill value used for integer values (e.g. in edge/face_nodes arrays).
      real(kind=dp), intent(in) :: dmiss    !< Fill value used for double precision values (e.g. in face_x_bnd variable).
      integer, intent(in) :: start_index     !< The base index of the provided arrays (0 if this function writes array from C/C++/C#, 1 for Fortran)
      integer, optional, intent(in) :: num_layers  !< Number of vertical layers in the mesh. Optional.
      integer, optional, intent(in) :: layerType !< Type of vertical layering in the mesh. One of LAYERTYPE_* parameters. Optional, only used if num_layers >= 1.
      integer, optional, intent(in) :: nsigma_opt   !< Number of sigma layers

      real(kind=dp), optional, pointer, intent(in) :: layer_zs(:)     !< Vertical coordinates of the mesh layers' center (z or sigma or z-sigma combined). Optional, only used if num_layers >= 1. For ocean_sigma_z, the sigma coordinates should be in the second part of the array.
      real(kind=dp), optional, pointer, intent(in) :: interface_zs(:) !< Vertical coordinates of the mesh layers' interface (z or sigma or z-sigma combined). Optional, only used if num_layers >= 1. For ocean_sigma_z, the sigma coordinates should be in the second part of the array.
      real(kind=dp), optional, pointer, intent(in) :: zn(:)           !< z-coordinates of the mesh nodes.
      character(len=*), optional :: waterlevelname  !< Water level variable name for sigma and sigma-z coordinates, should be without "mesh2d_" prefix, for example. Default: "s1".
      character(len=*), optional :: bldepthname     !< Bed level depth variable name for sigma and sigma-z coordinates, should be without "mesh2d_" prefix, for example. Default: "bldepth".

      ! Optional network1d variables for 1d UGrid
      type(t_ug_network), optional, intent(inout) :: networkids
      double precision, optional, pointer, intent(in) :: nnodex(:), nnodey(:), nbranchlengths(:), ngeopointx(:), ngeopointy(:)
      integer, optional, intent(in) :: sourceNodeId(:), targetNodeId(:), nbranchgeometrynodes(:), nbranchorder(:), nbranches, ngeometry
      character(len=ug_idsLen), optional, allocatable :: nnodeids(:), nbranchids(:), nodeids(:)
      character(len=ug_idsLongNamesLen), optional, allocatable :: nnodelongnames(:), nbranchlongnames(:), nodelongnames(:)
      character(len=*), optional :: network1dname
      ! Optional mesh1d variables for 1d UGrid
      integer, optional, pointer, intent(in) :: nodebranchidx(:) !< Branch indexes for each mesh1d node.
      double precision, optional, pointer, intent(in) :: nodeoffsets(:)   !< Offset along branch on which each mesh1d node lies.
      integer, optional, pointer, intent(in) :: edgebranchidx(:) !< Branch indexes for each mesh1d edge.
      double precision, optional, pointer, intent(in) :: edgeoffsets(:)   !< Offset along branch on which each mesh1d edge lies.
      integer, optional, intent(in) :: writeopts !< integer option, currently only: UG_WRITE_LATLON

      integer :: ierr !< Result status (UG_NOERR==NF90_NOERR) if successful.

      real(kind=dp), allocatable :: edgexbnd(:, :), edgeybnd(:, :), facexbnd(:, :), faceybnd(:, :)
      real(kind=dp), allocatable :: lonn(:), latn(:) !< lon,lat-coordinates of the mesh nodes.
      real(kind=dp), allocatable :: lone(:), late(:) !< representative lon,lat-coordinates of the mesh edges.
      real(kind=dp), allocatable :: lonf(:), latf(:) !< representative lon,lat-coordinates of the mesh faces.
      real(kind=dp), allocatable :: edgelonbnd(:, :), edgelatbnd(:, :), facelonbnd(:, :), facelatbnd(:, :)
      integer :: maxnv, k, n
      character(len=len_trim(meshName)) :: prefix
      integer :: wasInDefine
      logical :: add_edge_face_connectivity !< Specifies whether edge_face_connectivity should be added.
      logical :: add_face_edge_connectivity !< Specifies whether face_edge_connectivity should be added.
      logical :: add_face_face_connectivity !< Specifies whether face_face_connectivity should be added.
      logical :: add_layer_dim              !< Specifies whether layer and interface vertical dimensions should be added.
      logical :: add_layer_var              !< Specifies whether layer and interface vertical should be added as separate coordinate variables.
      logical :: add_latlon                 !< Specifies whether latlon coordinates should be added.
      integer :: offset
      logical :: is1dugridnetwork
      integer :: lengthofnetworkname
      integer :: nsigma
      double precision :: depth_c
      character(len=ug_idsLen) :: waterlevelname_, bldepthname_

      lengthofnetworkname = 0
      offset = 0
      if (start_index /= -1 .and. start_index == 0) then
         offset = 1
      end if

      ierr = UG_SOMEERR
      wasInDefine = 0

      ierr = nf90_redef(ncid)
      if (ierr == nf90_eindefine) wasInDefine = 1 ! Was still in define mode.

      prefix = trim(meshName)

      if (present(layerType)) then
         nsigma = 0
         if (layerType == LAYERTYPE_OCEAN_SIGMA_Z .and. present(nsigma_opt)) then
            nsigma = nsigma_opt
         else if (layerType == LAYERTYPE_OCEANSIGMA) then
            ! Code below depends on correctly set nsigma. For plain sigma layering: nsigma==num_layers.
            nsigma = num_layers
         end if
      end if

      add_edge_face_connectivity = associated(edge_faces)
      add_face_edge_connectivity = associated(face_edges)
      add_face_face_connectivity = associated(face_links)
      add_layer_dim = .false.

      if (present(num_layers) .and. present(layerType) .and. present(layer_zs) .and. present(interface_zs)) then
         add_layer_dim = num_layers >= 1
      end if

      if (present(writeopts)) then
         add_layer_var = add_layer_dim .and. (iand(writeopts, UG_WRITE_LYRVAR) > 0)
      else
         add_layer_var = add_layer_dim
      end if

#ifdef HAVE_PROJ
      if (present(writeopts)) then
         add_latlon = crs%epsg_code /= 4326 .and. iand(writeopts, UG_WRITE_LATLON) == UG_WRITE_LATLON
      else
         add_latlon = .false.
      end if
#else
      add_latlon = .false.
#endif

      ! This dimension might already be defined, check first if it is present
      ierr = nf90_inq_dimid(ncid, 'Two', meshids%dimids(mdim_two))
      if (ierr /= UG_NOERR) then
         ierr = nf90_def_dim(ncid, 'Two', 2, meshids%dimids(mdim_two))
      end if

      ierr = ug_add_coordmapping(ncid, crs)

      is1dugridnetwork = .false.
      lengthofnetworkname = 0
      if (present(network1dname)) then
         call remove_all_spaces(network1dname, lengthofnetworkname)
         if (lengthofnetworkname > 0) then
            is1dugridnetwork = .true.
         end if
      end if

      if (.not. is1dugridnetwork) then !2d/3d and 1d not UGRID 1.6
         ierr = ug_write_meshtopology(ncid, meshids, meshName, dim, dataLocs, add_edge_face_connectivity, add_face_edge_connectivity, add_face_face_connectivity, add_layer_dim, add_latlon)
         ! Dimensions
         ierr = nf90_def_dim(ncid, prefix//'_nEdges', numEdge, meshids%dimids(mdim_edge))
         ierr = nf90_def_dim(ncid, prefix//'_nNodes', numNode, meshids%dimids(mdim_node))

         if (dim == 2 .or. ug_checklocation(dataLocs, UG_LOC_FACE)) then
            ! TODO: AvD: the new maxNumNodesPerFace dummy variable overlaps with this nv here, but they may be different. Remove one.
            maxnv = size(face_nodes, 1)
            if (meshids%dimids(mdim_face) <= 0) then
               ierr = nf90_def_dim(ncid, prefix//'_nFaces', numFace, meshids%dimids(mdim_face))
            end if
            ierr = nf90_def_dim(ncid, prefix//'_nMax_face_nodes', maxnv, meshids%dimids(mdim_maxfacenodes))
         end if

         if (add_layer_dim) then
            if (dim >= 3) then
               ! Only 1D and 2D mesh topologies can be layered.
               ierr = UG_INVALID_LAYERS
               goto 888
            end if
            ierr = nf90_def_dim(ncid, prefix//'_nLayers', num_layers, meshids%dimids(mdim_layer))
            ierr = nf90_def_dim(ncid, prefix//'_nInterfaces', num_layers + 1, meshids%dimids(mdim_interface))
         end if

         ! Nodes
         ! node x,y-coordinates.
         ierr = ug_addcoordvars(ncid, meshids%varids(mid_nodex), meshids%varids(mid_nodey), (/meshids%dimids(mdim_node)/), prefix//'_node_x', prefix//'_node_y', &
                                'x-coordinate of mesh nodes', 'y-coordinate of mesh nodes', trim(meshName), 'node', crs)
#ifdef HAVE_PROJ
         if (add_latlon) then ! If x,y are not in WGS84 system, then add mandatory additional lon/lat coordinates.
            ierr = ug_addlonlatcoordvars(ncid, meshids%varids(mid_nodelon), meshids%varids(mid_nodelat), (/meshids%dimids(mdim_node)/), prefix//'_node_lon', prefix//'_node_lat', &
                                         'longitude coordinate of mesh nodes', 'latitude coordinate of mesh nodes', trim(meshName), 'node')
         end if
#endif

         if (present(writeopts)) then
            ierr = ug_def_var(ncid, meshids%varids(mid_nodez), (/meshids%dimids(mdim_node)/), nf90_double, UG_LOC_NODE, &
                              meshName, 'node_z', 'altitude', 'z-coordinate of mesh nodes', 'm', '', '', crs, dfill=dmiss, writeopts=writeopts)
         else
            ierr = ug_def_var(ncid, meshids%varids(mid_nodez), (/meshids%dimids(mdim_node)/), nf90_double, UG_LOC_NODE, &
                              meshName, 'node_z', 'altitude', 'z-coordinate of mesh nodes', 'm', '', '', crs, dfill=dmiss)
         end if

      else
         if (dim == 1) then
            if (present(ngeopointx)) then
               if (associated(ngeopointx)) then !1d UGRID 1.6
                  !some results might still be saved at the edges, also for 1d
                  ierr = ug_create_1d_network(ncid, networkids, network1dname, size(nnodex), nbranches, ngeometry)
               end if
            end if
            if (numNode > 0) then
               ierr = ug_create_1d_mesh_v2(ncid, network1dname, meshids, meshname, numNode, numEdge, 1, crs) ! Creates node and edge variables
               ierr = ug_def_mesh_ids(ncid, meshids, meshname, UG_LOC_NODE)
            end if
         end if
      end if

      if (ierr /= UG_NOERR) then
         goto 888
      end if

      if (ug_checklocation(dataLocs, UG_LOC_EDGE)) then
         ! edge x,y-coordinates.
         if (meshids%varids(mid_edgex) == -1) then
            ierr = ug_addcoordvars(ncid, meshids%varids(mid_edgex), meshids%varids(mid_edgey), (/meshids%dimids(mdim_edge)/), prefix//'_edge_x', prefix//'_edge_y', &
                                   'characteristic x-coordinate of the mesh edge (e.g. midpoint)', 'characteristic y-coordinate of the mesh edge (e.g. midpoint)', trim(meshName), 'edge', crs)
         end if

         ! Add bounds.
         ! UNST-2791: until further notice we will not write edge bounds anymore (at least not until we have a full polygon bounds shape for each edge, that is, with at least four points).
         !ierr = nf90_put_att(ncid, meshids%varids(mid_edgex), 'bounds',    prefix//'_edge_x_bnd')
         !ierr = nf90_put_att(ncid, meshids%varids(mid_edgey), 'bounds',    prefix//'_edge_y_bnd')
         !ierr = ug_addcoordvars(ncid, meshids%varids(mid_edgexbnd), meshids%varids(mid_edgeybnd), (/ meshids%dimids(mdim_two), meshids%dimids(mdim_edge) /), prefix//'_edge_x_bnd', prefix//'_edge_y_bnd', &
         !'x-coordinate bounds of 2D mesh edge (i.e. end point coordinates)', 'y-coordinate bounds of 2D mesh edge (i.e. end point coordinates)', trim(meshName), 'edge', crs)

#ifdef HAVE_PROJ
         if (add_latlon) then ! If x,y are not in WGS84 system, then add mandatory additional lon/lat coordinates.
            ierr = ug_addlonlatcoordvars(ncid, meshids%varids(mid_edgelon), meshids%varids(mid_edgelat), (/meshids%dimids(mdim_edge)/), prefix//'_edge_lon', prefix//'_edge_lat', &
                                         'characteristic longitude coordinate of the mesh edge (e.g. midpoint)', 'characteristic latitude coordinate of the mesh edge (e.g. midpoint)', trim(meshName), 'edge')
            ! Add bounds.
            ! UNST-2791: until further notice we will not write edge bounds anymore (at least not until we have a full polygon bounds shape for each edge, that is, with at least four points).
            !ierr = nf90_put_att(ncid, meshids%varids(mid_edgelon), 'bounds',    prefix//'_edge_lon_bnd')
            !ierr = nf90_put_att(ncid, meshids%varids(mid_edgelat), 'bounds',    prefix//'_edge_lat_bnd')
            !ierr = ug_addlonlatcoordvars(ncid, meshids%varids(mid_edgelonbnd), meshids%varids(mid_edgelatbnd), (/ meshids%dimids(mdim_two), meshids%dimids(mdim_edge) /), prefix//'_edge_lon_bnd', prefix//'_edge_lat_bnd', &
            !                             'longitude coordinate bounds of 2D mesh edge (i.e. end point coordinates)', 'latitude coordinate bounds of 2D mesh edge (i.e. end point coordinates)', trim(meshName), 'edge')
         end if
#endif
      end if

      !ierr = nf90_def_var(inetfile, 'NetLinkType', nf90_int, id_netlinkdim, id_netlinktype)
      !ierr = nf90_put_att(inetfile, id_netlinktype, 'long_name',    'type of netlink')
      !ierr = nf90_put_att(inetfile, id_netlinktype, 'valid_range',   (/ 0, 4 /))
      !ierr = nf90_put_att(inetfile, id_netlinktype, 'flag_values',   (/ 0, 1, 2, 3, 4 /))
      !ierr = nf90_put_att(inetfile, id_netlinktype, 'flag_meanings', 'closed_link_between_2D_nodes link_between_1D_nodes link_between_2D_nodes embedded_1D2D_link 1D2D_link')

      ! Edges
      if (dim == 1 .or. ug_checklocation(dataLocs, UG_LOC_EDGE)) then
         ierr = nf90_def_var(ncid, prefix//'_edge_nodes', nf90_int, (/meshids%dimids(mdim_two), meshids%dimids(mdim_edge)/), meshids%varids(mid_edgenodes))
         ierr = nf90_put_att(ncid, meshids%varids(mid_edgenodes), 'cf_role', 'edge_node_connectivity')
         ierr = nf90_put_att(ncid, meshids%varids(mid_edgenodes), 'long_name', 'Start and end nodes of mesh edges')
         if (start_index /= -1) then
            ierr = nf90_put_att(ncid, meshids%varids(mid_edgenodes), 'start_index', start_index)
         end if
      end if

      ! Faces
      if (dim == 2 .or. ug_checklocation(dataLocs, UG_LOC_FACE)) then
         ierr = nf90_def_var(ncid, prefix//'_face_nodes', nf90_int, (/meshids%dimids(mdim_maxfacenodes), meshids%dimids(mdim_face)/), meshids%varids(mid_facenodes))
         ierr = nf90_put_att(ncid, meshids%varids(mid_facenodes), 'cf_role', 'face_node_connectivity')
         ierr = nf90_put_att(ncid, meshids%varids(mid_facenodes), 'long_name', 'Vertex nodes of mesh faces (counterclockwise)')
         if (start_index /= -1) then
            ierr = nf90_put_att(ncid, meshids%varids(mid_facenodes), 'start_index', start_index)
         end if
         ierr = nf90_put_att(ncid, meshids%varids(mid_facenodes), '_FillValue', imiss)

         ! Face edge connectivity.
         if (add_face_edge_connectivity) then
            ierr = nf90_def_var(ncid, prefix//'_face_edges', nf90_int, (/meshids%dimids(mdim_maxfacenodes), meshids%dimids(mdim_face)/), meshids%varids(mid_faceedges))
            ierr = nf90_put_att(ncid, meshids%varids(mid_faceedges), 'cf_role', 'face_edge_connectivity')
            ierr = nf90_put_att(ncid, meshids%varids(mid_faceedges), 'long_name', 'Side edges of mesh faces (counterclockwise)')
            if (start_index /= -1) then
               ierr = nf90_put_att(ncid, meshids%varids(mid_faceedges), 'start_index', start_index)
            end if
            ierr = nf90_put_att(ncid, meshids%varids(mid_faceedges), '_FillValue', imiss)
         end if

         ! Face face connectivity.
         if (add_face_face_connectivity) then
            ierr = nf90_def_var(ncid, prefix//'_face_links', nf90_int, (/meshids%dimids(mdim_maxfacenodes), meshids%dimids(mdim_face)/), meshids%varids(mid_facelinks))
            ierr = nf90_put_att(ncid, meshids%varids(mid_facelinks), 'cf_role', 'face_face_connectivity')
            ierr = nf90_put_att(ncid, meshids%varids(mid_facelinks), 'long_name', 'Neighboring faces of mesh faces (counterclockwise)')
            if (start_index /= -1) then
               ierr = nf90_put_att(ncid, meshids%varids(mid_facelinks), 'start_index', start_index)
            end if
            ierr = nf90_put_att(ncid, meshids%varids(mid_facelinks), '_FillValue', imiss)
         end if

         ! Edge face connectivity.
         ! Note that edge_face_connectivity is not officially part of the UGRID conventions, however it is very similar to e.g. face_edge_connectivity, which is part of the UGRID conventions.
         if (add_edge_face_connectivity) then
            ierr = nf90_def_var(ncid, prefix//'_edge_faces', nf90_int, (/meshids%dimids(mdim_two), meshids%dimids(mdim_edge)/), meshids%varids(mid_edgefaces))
            ierr = nf90_put_att(ncid, meshids%varids(mid_edgefaces), 'cf_role', 'edge_face_connectivity')
            ierr = nf90_put_att(ncid, meshids%varids(mid_edgefaces), 'long_name', 'Neighboring faces of mesh edges')
            if (start_index /= -1) then
               ierr = nf90_put_att(ncid, meshids%varids(mid_edgefaces), 'start_index', start_index)
            end if
            ierr = nf90_put_att(ncid, meshids%varids(mid_edgefaces), '_FillValue', imiss)
         end if
      end if
      if (ug_checklocation(dataLocs, UG_LOC_FACE)) then
         ! face x,y-coordinates.
         ierr = ug_addcoordvars(ncid, meshids%varids(mid_facex), meshids%varids(mid_facey), (/meshids%dimids(mdim_face)/), prefix//'_face_x', prefix//'_face_y', &
                                'Characteristic x-coordinate of mesh face', 'Characteristic y-coordinate of mesh face', trim(meshName), 'face', crs)
         ierr = nf90_put_att(ncid, meshids%varids(mid_facex), 'bounds', prefix//'_face_x_bnd')
         ierr = nf90_put_att(ncid, meshids%varids(mid_facey), 'bounds', prefix//'_face_y_bnd')
         ! Add bounds.
         ierr = ug_addcoordvars(ncid, meshids%varids(mid_facexbnd), meshids%varids(mid_faceybnd), (/meshids%dimids(mdim_maxfacenodes), meshids%dimids(mdim_face)/), prefix//'_face_x_bnd', prefix//'_face_y_bnd', &
                                'x-coordinate bounds of mesh faces (i.e. corner coordinates)', 'y-coordinate bounds of mesh faces (i.e. corner coordinates)', trim(meshName), 'face', crs)
         ierr = nf90_put_att(ncid, meshids%varids(mid_facexbnd), '_FillValue', dmiss)
         ierr = nf90_put_att(ncid, meshids%varids(mid_faceybnd), '_FillValue', dmiss)

#ifdef HAVE_PROJ
         if (add_latlon) then ! If x,y are not in WGS84 system, then add mandatory additional lon/lat coordinates.
            ierr = ug_addlonlatcoordvars(ncid, meshids%varids(mid_facelon), meshids%varids(mid_facelat), (/meshids%dimids(mdim_face)/), prefix//'_face_lon', prefix//'_face_lat', &
                                         'Characteristic longitude coordinate of mesh faces', 'Characteristic latitude coordinate of mesh faces', trim(meshName), 'face')
            ierr = nf90_put_att(ncid, meshids%varids(mid_facelon), 'bounds', prefix//'_face_lon_bnd')
            ierr = nf90_put_att(ncid, meshids%varids(mid_facelat), 'bounds', prefix//'_face_lat_bnd')
            ! Add bounds.
            ierr = ug_addlonlatcoordvars(ncid, meshids%varids(mid_facelonbnd), meshids%varids(mid_facelatbnd), (/meshids%dimids(mdim_maxfacenodes), meshids%dimids(mdim_face)/), prefix//'_face_lon_bnd', prefix//'_face_lat_bnd', &
                                         'Longitude coordinate bounds of mesh faces (i.e. corner coordinates)', 'Latitude coordinate bounds of mesh faces (i.e. corner coordinates)', trim(meshName), 'face')
            ierr = nf90_put_att(ncid, meshids%varids(mid_facelonbnd), '_FillValue', dmiss)
            ierr = nf90_put_att(ncid, meshids%varids(mid_facelatbnd), '_FillValue', dmiss)
         end if
#endif
      end if

      ! Layers
      if (add_layer_var) then

         if (present(waterlevelname)) then
            waterlevelname_ = waterlevelname
         else
            waterlevelname_ = 's1' !default value
         end if
         if (present(bldepthname)) then
            bldepthname_ = bldepthname
         else
            bldepthname_ = 'bldepth' !default value
         end if

         ! Write mesh layer distribution (mesh-global, not per face)
!      select case(layerType)
         if (layerType == LAYERTYPE_OCEANSIGMA .or. layertype == LAYERTYPE_OCEAN_SIGMA_Z) then
            ierr = nf90_def_var(ncid, prefix//'_layer_sigma', nf90_double, meshids%dimids(mdim_layer), meshids%varids(mid_layersigma))
            ierr = nf90_def_var(ncid, prefix//'_interface_sigma', nf90_double, meshids%dimids(mdim_interface), meshids%varids(mid_interfacesigma))
            ierr = nf90_put_att(ncid, meshids%varids(mid_layersigma), 'standard_name', 'ocean_sigma_coordinate')
            ierr = nf90_put_att(ncid, meshids%varids(mid_interfacesigma), 'standard_name', 'ocean_sigma_coordinate')
            ierr = nf90_put_att(ncid, meshids%varids(mid_layersigma), 'long_name', 'Sigma coordinate of layer centres')
            ierr = nf90_put_att(ncid, meshids%varids(mid_interfacesigma), 'long_name', 'Sigma coordinate of layer interfaces')
            ! See http://cfconventions.org/cf-conventions/cf-conventions.html#dimensionless-vertical-coordinate
            ! and http://cfconventions.org/cf-conventions/cf-conventions.html#_ocean_sigma_coordinate for info about formula_terms attribute for sigma coordinates.

            if (layerType == LAYERTYPE_OCEANSIGMA) then
               ! Only for pure sigma coordinates: these vars contain the formula terms, for ocean_sigma_z, see further below.

               ierr = nf90_put_att(ncid, meshids%varids(mid_layersigma), 'formula_terms', 'sigma: '//prefix//'_layer_sigma eta: '//prefix//'_'//trim(waterlevelname_)//' depth: '//prefix//'_waterdepth') ! TODO: AvD: do we define this only on faces?
               ierr = nf90_put_att(ncid, meshids%varids(mid_interfacesigma), 'formula_terms', 'sigma: '//prefix//'_interface_sigma eta: '//prefix//'_'//trim(waterlevelname_)//' depth: '//prefix//'_waterdepth') ! TODO: AvD: do we define this only on faces?
               ! TODO: UNST-5711, Wrong waterdepth above to be replaced by correct bldepth variable, but then also taking time dependency of bldepth into account.
               !ierr = nf90_put_att(ncid, meshids%varids(mid_layersigma),     'formula_terms', 'sigma: '//prefix//'_layer_sigma eta: '//prefix//'_'//trim(waterlevelname_)//' depth: '//prefix//'_'/trim(bldepthname_)) ! TODO: AvD: do we define this only on faces?
               !ierr = nf90_put_att(ncid, meshids%varids(mid_interfacesigma), 'formula_terms', 'sigma: '//prefix//'_interface_sigma eta: '//prefix//'_'//trim(waterlevelname_)//' depth: '//prefix//'_'/trim(bldepthname_)) ! TODO: AvD: do we define this only on faces?
               ierr = nf90_put_att(ncid, meshids%varids(mid_layersigma), 'computed_standard_name', 'altitude')
               ierr = nf90_put_att(ncid, meshids%varids(mid_interfacesigma), 'computed_standard_name', 'altitude')
            end if
         end if
         if (layertype == LAYERTYPE_Z .or. layertype == LAYERTYPE_OCEAN_SIGMA_Z) then
            ierr = nf90_def_var(ncid, prefix//'_layer_z', nf90_double, meshids%dimids(mdim_layer), meshids%varids(mid_layerz))
            ierr = nf90_def_var(ncid, prefix//'_interface_z', nf90_double, meshids%dimids(mdim_interface), meshids%varids(mid_interfacez))
            ierr = nf90_put_att(ncid, meshids%varids(mid_layerz), 'standard_name', 'altitude')
            ierr = nf90_put_att(ncid, meshids%varids(mid_interfacez), 'standard_name', 'altitude')
            ierr = nf90_put_att(ncid, meshids%varids(mid_layerz), 'long_name', 'Vertical coordinate of layer centres')
            ierr = nf90_put_att(ncid, meshids%varids(mid_interfacez), 'long_name', 'Vertical coordinate of layer interfaces')
            ierr = nf90_put_att(ncid, meshids%varids(mid_layerz), 'units', 'm')
            ierr = nf90_put_att(ncid, meshids%varids(mid_interfacez), 'units', 'm')
         end if
         if (layertype == LAYERTYPE_OCEAN_SIGMA_Z) then
            ! See http://cfconventions.org/cf-conventions/cf-conventions.html#_ocean_sigma_over_z_coordinate

            ierr = nf90_def_var(ncid, prefix//'_sigmazdepth', nf90_double, meshids%varids(mid_sigmazdepth))
            ierr = nf90_put_att(ncid, meshids%varids(mid_sigmazdepth), 'standard_name', "Transition depth from sigma above to z below")
            ierr = nf90_put_att(ncid, meshids%varids(mid_sigmazdepth), 'units', "m")
            ierr = nf90_put_att(ncid, meshids%varids(mid_sigmazdepth), 'positive', "down")

            ierr = nf90_def_var(ncid, prefix//'_layer_sigma_z', nf90_double, meshids%dimids(mdim_layer), meshids%varids(mid_layerzs))
            ierr = nf90_def_var(ncid, prefix//'_interface_sigma_z', nf90_double, meshids%dimids(mdim_interface), meshids%varids(mid_interfacezs))
            ierr = nf90_put_att(ncid, meshids%varids(mid_layerzs), 'standard_name', 'ocean_sigma_z_coordinate')
            ierr = nf90_put_att(ncid, meshids%varids(mid_interfacezs), 'standard_name', 'ocean_sigma_z_coordinate')
            ierr = nf90_put_att(ncid, meshids%varids(mid_layerzs), 'long_name', 'Sigma/Z coordinate of layer centres')
            ierr = nf90_put_att(ncid, meshids%varids(mid_interfacezs), 'long_name', 'Sigma/Z coordinate of layer interfaces')

            ierr = nf90_put_att(ncid, meshids%varids(mid_interfacezs), 'formula_terms', 'sigma: '//prefix//'_interface_sigma eta: '//prefix//'_'//trim(waterlevelname_)//' depth: '//prefix//'_'//trim(bldepthname_)//' depth_c: '//prefix//'_sigmazdepth zlev: '//prefix//'_interface_z ') ! TODO: AvD: do we define this only on faces?
            ierr = nf90_put_att(ncid, meshids%varids(mid_layerzs), 'formula_terms', 'sigma: '//prefix//'_layer_sigma eta: '//prefix//'_'//trim(waterlevelname_)//' depth: '//prefix//'_'//trim(bldepthname_)//' depth_c: '//prefix//'_sigmazdepth zlev: '//prefix//'_layer_z ') ! TODO: AvD: do we define this only on faces?
            ierr = nf90_put_att(ncid, meshids%varids(mid_layerzs), 'units', 'm')
            ierr = nf90_put_att(ncid, meshids%varids(mid_interfacezs), 'units', 'm')
            ierr = nf90_put_att(ncid, meshids%varids(mid_layerzs), 'computed_standard_name', 'altitude')
            ierr = nf90_put_att(ncid, meshids%varids(mid_interfacezs), 'computed_standard_name', 'altitude')

         end if
         if (layertype /= LAYERTYPE_OCEAN_SIGMA_Z .and. layertype /= LAYERTYPE_Z .and. layerType /= LAYERTYPE_OCEANSIGMA) then
            ierr = UG_INVALID_LAYERS
            goto 888
         end if
      end if

! TODO: AvD: add the following (resolution may be difficult)
!>      :geospatial_lat_min = 52.9590188916822 ;
!>      :geospatial_lat_max = 53.8746171549558 ;
!>      :geospatial_lat_units = "degrees_north" ;
!>      :geospatial_lat_resolution = "on average     370.50 meters" ;
!>      :geospatial_lon_min = 6.37848435307356 ;
!>      :geospatial_lon_max = 7.68944972163126 ;
!>      :geospatial_lon_units = "degrees_east" ;
!>      :geospatial_lon_resolution = "on average     370.50 meters" ;

      ierr = nf90_enddef(ncid)

! -- end of header --

      ! Write the actual data
      ! Nodes:
      ierr = nf90_put_var(ncid, meshids%varids(mid_nodex), xn(1:numNode))
      ierr = nf90_put_var(ncid, meshids%varids(mid_nodey), yn(1:numNode))
      if (present(zn)) then
         if (associated(zn)) then
            ierr = nf90_put_var(ncid, meshids%varids(mid_nodez), zn(1:numNode))
         end if
      end if
#ifdef HAVE_PROJ
      if (add_latlon) then ! If x,y are not in WGS84 system, then add mandatory additional lon/lat coordinates.
         call realloc(lonn, size(xn), fill=dmiss, keepExisting=.false.)
         call realloc(latn, size(yn), fill=dmiss, keepExisting=.false.)
         call transform_coordinates(crs%proj_string, WGS84_PROJ_STRING, xn, yn, lonn, latn)
         ierr = nf90_put_var(ncid, meshids%varids(mid_nodelon), lonn(1:numNode))
         ierr = nf90_put_var(ncid, meshids%varids(mid_nodelat), latn(1:numNode))
      end if
#endif

      ! Edges:
      if (dim == 1 .or. ug_checklocation(dataLocs, UG_LOC_EDGE)) then
         !if is UGRID 1.0 network, we need to write the network here
         if (is1dugridnetwork .and. present(ngeopointx)) then
            if (associated(ngeopointx)) then
               ! write network
               ierr = ug_write_1d_network_nodes(ncid, networkids, nnodex, nnodey, nnodeids, nnodelongnames)
               ierr = ug_put_1d_network_branches(ncid, networkids, sourceNodeId, targetNodeId, nbranchids, nbranchlengths, nbranchlongnames, nbranchgeometrynodes, nbranches, start_index)
               ierr = ug_put_1d_network_branchorder(ncid, networkids, nbranchorder)
               ierr = ug_write_1d_network_branches_geometry(ncid, networkids, ngeopointx, ngeopointy)
            end if
         end if
         ! write mesh1d
         if (lengthofnetworkname > 0) then
            if (present(nodebranchidx)) then
               if (associated(nodebranchidx)) then
                  ierr = ug_put_1d_mesh_discretisation_points_v1(ncid, meshids, nodebranchidx, nodeoffsets, start_index, xn, yn)
               end if
            end if
            !write node ids and node long names
            if (present(nodeids)) then
               if (allocated(nodeids)) then
                  ierr = nf90_put_var(ncid, meshids%varids(mid_node_ids), nodeids)
               end if
            end if
            if (present(nodelongnames)) then
               if (allocated(nodelongnames)) then
                  ierr = nf90_put_var(ncid, meshids%varids(mid_node_longnames), nodelongnames)
               end if
            end if
            if (present(edgebranchidx)) then
               if (associated(edgebranchidx)) then
                  ierr = ug_put_1d_mesh_edges(ncid, meshids, edgebranchidx, edgeoffsets, start_index, xe, ye)
               end if
            end if
         end if
         ! always write edge nodes
         if (meshids%varids(mid_edgenodes) /= -1) then
            ierr = nf90_put_var(ncid, meshids%varids(mid_edgenodes), edge_nodes, count=(/2, numEdge/))
         end if
      end if

      if (ug_checklocation(dataLocs, UG_LOC_EDGE)) then
         if ((meshids%varids(mid_edgex) /= -1) .and. (size(xe) > 0)) then
            ierr = nf90_put_var(ncid, meshids%varids(mid_edgex), xe(1:numEdge))
         end if
         if ((meshids%varids(mid_edgey) /= -1) .and. (size(ye) > 0)) then
            ierr = nf90_put_var(ncid, meshids%varids(mid_edgey), ye(1:numEdge))
         end if
         ! end point coordinates:
         if (size(edge_nodes, 2) > 0) then
            allocate (edgexbnd(2, numEdge), edgeybnd(2, numEdge))
            edgexbnd = dmiss
            edgeybnd = dmiss
            do n = 1, numEdge
               edgexbnd(1:2, n) = xn(edge_nodes(1:2, n) + offset)
               edgeybnd(1:2, n) = yn(edge_nodes(1:2, n) + offset)
            end do
            if (meshids%varids(mid_edgexbnd) /= -1) then
               ierr = nf90_put_var(ncid, meshids%varids(mid_edgexbnd), edgexbnd)
            end if
            if (meshids%varids(mid_edgeybnd) /= -1) then
               ierr = nf90_put_var(ncid, meshids%varids(mid_edgeybnd), edgeybnd)
            end if
            deallocate (edgexbnd, edgeybnd)
         end if

#ifdef HAVE_PROJ
         if (add_latlon) then ! If x,y are not in WGS84 system, then add mandatory additional lon/lat coordinates.
            call realloc(lone, size(xe), fill=dmiss, keepExisting=.false.)
            call realloc(late, size(ye), fill=dmiss, keepExisting=.false.)
            call transform_coordinates(crs%proj_string, WGS84_PROJ_STRING, xe, ye, lone, late)
            if (meshids%varids(mid_edgelon) /= -1) then
               ierr = nf90_put_var(ncid, meshids%varids(mid_edgelon), lone(1:numEdge))
            end if
            if (meshids%varids(mid_edgelat) /= -1) then
               ierr = nf90_put_var(ncid, meshids%varids(mid_edgelat), late(1:numEdge))
            end if
            deallocate (lone)
            deallocate (late)

            ! end point coordinates:
            if (size(edge_nodes, 2) > 0) then
               allocate (edgelonbnd(2, numEdge), edgelatbnd(2, numEdge))
               edgelonbnd = dmiss
               edgelatbnd = dmiss
               do n = 1, numEdge
                  edgelonbnd(1:2, n) = lonn(edge_nodes(1:2, n) + offset)
                  edgelatbnd(1:2, n) = latn(edge_nodes(1:2, n) + offset)
               end do
               if (meshids%varids(mid_edgelonbnd) /= -1) then
                  ierr = nf90_put_var(ncid, meshids%varids(mid_edgelonbnd), edgelonbnd)
               end if
               if (meshids%varids(mid_edgelatbnd) /= -1) then
                  ierr = nf90_put_var(ncid, meshids%varids(mid_edgelatbnd), edgelatbnd)
               end if
               deallocate (edgelonbnd, edgelatbnd)
            end if
         end if
#endif
      end if

      ! Faces:
      if (dim == 2 .or. ug_checklocation(dataLocs, UG_LOC_FACE)) then
         ! Write mesh faces (2D cells)
         if (meshids%varids(mid_facenodes) /= -1) then
            ierr = nf90_put_var(ncid, meshids%varids(mid_facenodes), face_nodes)
         end if
         ! Face edge connectivity:
         if (add_face_edge_connectivity) then
            if (meshids%varids(mid_faceedges) /= -1) then
               ierr = nf90_put_var(ncid, meshids%varids(mid_faceedges), face_edges, count=(/maxnv, numFace/))
            end if
         end if
         ! Face face connectivity:
         if (add_face_face_connectivity) then
            if (meshids%varids(mid_facelinks) /= -1) then
               ierr = nf90_put_var(ncid, meshids%varids(mid_facelinks), face_links, count=(/maxnv, numFace/))
            end if
         end if
         ! Edge face connectivity:
         if (add_edge_face_connectivity) then
            if (meshids%varids(mid_edgefaces) /= -1) then
               ierr = nf90_put_var(ncid, meshids%varids(mid_edgefaces), edge_faces, count=(/2, numEdge/))
            end if
         end if

         ! corner point coordinates:
         if (size(face_nodes, 2) > 0) then
            allocate (facexbnd(maxnv, numFace), faceybnd(maxnv, numFace))
            facexbnd = dmiss
            faceybnd = dmiss

            do n = 1, numFace
               do k = 1, maxnv
                  if (face_nodes(k, n) == imiss) then
                     exit ! This face has less corners than maxnv, we're done for this one.
                  end if
                  facexbnd(k, n) = xn(face_nodes(k, n) + offset)
                  faceybnd(k, n) = yn(face_nodes(k, n) + offset)
               end do
            end do

            if (meshids%varids(mid_facexbnd) /= -1) then
               ierr = nf90_put_var(ncid, meshids%varids(mid_facexbnd), facexbnd)
            end if
            if (meshids%varids(mid_faceybnd) /= -1) then
               ierr = nf90_put_var(ncid, meshids%varids(mid_faceybnd), faceybnd)
            end if
            deallocate (facexbnd, faceybnd)
         end if

#ifdef HAVE_PROJ
         if (add_latlon) then ! If x,y are not in WGS84 system, then add mandatory additional lon/lat coordinates.
            ! corner point coordinates:
            allocate (facelonbnd(maxnv, numFace), facelatbnd(maxnv, numFace))
            facelonbnd = dmiss
            facelatbnd = dmiss

            do n = 1, numFace
               do k = 1, maxnv
                  if (face_nodes(k, n) == imiss) then
                     exit ! This face has less corners than maxnv, we're done for this one.
                  end if

                  facelonbnd(k, n) = lonn(face_nodes(k, n) + offset)
                  facelatbnd(k, n) = latn(face_nodes(k, n) + offset)
               end do
            end do
            if (meshids%varids(mid_facelonbnd) /= -1) then
               ierr = nf90_put_var(ncid, meshids%varids(mid_facelonbnd), facelonbnd)
            end if
            if (meshids%varids(mid_facelatbnd) /= -1) then
               ierr = nf90_put_var(ncid, meshids%varids(mid_facelatbnd), facelatbnd)
            end if
            deallocate (facelonbnd, facelatbnd)
         end if
#endif
      end if

      if (ug_checklocation(dataLocs, UG_LOC_FACE)) then

         if ((meshids%varids(mid_facex) /= -1) .and. (numFace > 0)) then
            ierr = nf90_put_var(ncid, meshids%varids(mid_facex), xf(1:numFace))
         end if
         if ((meshids%varids(mid_facey) /= -1) .and. (numFace > 0)) then
            ierr = nf90_put_var(ncid, meshids%varids(mid_facey), yf(1:numFace))
         end if

#ifdef HAVE_PROJ
         if (add_latlon) then ! If x,y are not in WGS84 system, then add mandatory additional lon/lat coordinates.
            call realloc(lonf, size(xf), fill=dmiss, keepExisting=.false.)
            call realloc(latf, size(yf), fill=dmiss, keepExisting=.false.)
            call transform_coordinates(crs%proj_string, WGS84_PROJ_STRING, xf, yf, lonf, latf)
            if ((meshids%varids(mid_facelon) /= -1) .and. (numFace > 0)) then
               ierr = nf90_put_var(ncid, meshids%varids(mid_facelon), lonf(1:numFace))
            end if
            if ((meshids%varids(mid_facelat) /= -1) .and. (numFace > 0)) then
               ierr = nf90_put_var(ncid, meshids%varids(mid_facelat), latf(1:numFace))
            end if
            deallocate (lonf)
            deallocate (latf)
         end if
#endif
      end if

      if (allocated(lonn)) deallocate (lonn)
      if (allocated(latn)) deallocate (latn)

      ! Layers
      if (add_layer_var) then

         if (associated(interface_zs) .and. layertype == LAYERTYPE_OCEAN_SIGMA_Z) then
            depth_c = -interface_zs(num_layers + 1 - nsigma) ! z pos -> depth pos down
         end if

         if (associated(interface_zs) .and. (meshids%varids(mid_sigmazdepth) /= -1) .and. (nsigma > 0)) then
            ierr = nf90_put_var(ncid, meshids%varids(mid_sigmazdepth), depth_c)
         end if

         ! Write mesh layer distribution (mesh-global, not per face)
         ! "[zs] variable should contain sigma(k)*depth_c for the layers where sigma is defined and zlev(k) for the other layers"
         ! "zlev(k) is defined for the nlayer - nsigma deeper layers"
         ! "The parameter sigma(k) is defined only for the nsigma layers nearest the ocean surface"

         if (num_layers > 0) then
            if (associated(layer_zs)) then
               if (meshids%varids(mid_layerzs) > 0) then
                  ierr = nf90_put_var(ncid, meshids%varids(mid_layerzs), layer_zs(1:num_layers - nsigma), start=(/1/))
                  ierr = nf90_put_var(ncid, meshids%varids(mid_layerzs), layer_zs(num_layers - nsigma + 1:num_layers) * depth_c, start=(/num_layers - nsigma + 1/))
               end if
               if (meshids%varids(mid_layerz) > 0) then
                  ierr = nf90_put_var(ncid, meshids%varids(mid_layerz), layer_zs(1:num_layers - nsigma))
               end if
               if (meshids%varids(mid_layersigma) > 0) then
                  ierr = nf90_put_var(ncid, meshids%varids(mid_layersigma), layer_zs(num_layers - nsigma + 1:num_layers), start=(/num_layers - nsigma + 1/))
               end if
            end if ! layer_zs

            if (associated(interface_zs)) then
               if (meshids%varids(mid_interfacezs) > 0) then
                  ! Combined z-sigma variable
                  ierr = nf90_put_var(ncid, meshids%varids(mid_interfacezs), interface_zs(1:num_layers - nsigma + 1), start=(/1/))
                  ierr = nf90_put_var(ncid, meshids%varids(mid_interfacezs), interface_zs(num_layers - nsigma + 2:num_layers + 1) * depth_c, start=(/num_layers - nsigma + 2/))
               end if
               if (meshids%varids(mid_interfacez) > 0) then
                  ! Z-layers (also partly, when in combined ocean_sigma_z)
                  ierr = nf90_put_var(ncid, meshids%varids(mid_interfacez), interface_zs(1:num_layers - nsigma + 1))
               end if
               if (meshids%varids(mid_interfacesigma) > 0) then
                  if (num_layers == nsigma) then ! only sigma-layers
                     ierr = nf90_put_var(ncid, meshids%varids(mid_interfacesigma), interface_zs(1:num_layers + 1), start=(/1/))
                  else ! sigma-layers (also partly, when in combined ocean_sigma_z)
                     ierr = nf90_put_var(ncid, meshids%varids(mid_interfacesigma), interface_zs(num_layers - nsigma + 2:num_layers + 1), start=(/num_layers - nsigma + 2/))
                  end if
               end if
            end if ! interface_zs
         end if ! num_layers

      end if

      ! Check for any remaining native NetCDF errors
      if (ierr /= nf90_noerr) then
         goto 888
      end if

      ! Leave the dataset in the same mode as we got it.
      if (wasInDefine == 1) then
         ierr = nf90_redef(ncid)
      end if

      ierr = UG_NOERR
      return ! Return with success

888   continue

   end function ug_write_mesh_arrays

!
! -- Reading-related routines ---------------------------------------------
!

!> This function just creates a new mesh with no topology information, these will be added later
   function ug_add_mesh(ncid, ug_file, meshid) result(ierr)

      integer, intent(in) :: ncid    !< ID of already opened data set.
      type(t_ug_file), intent(inout) :: ug_file !< UGRID file struct with cached meta information.
      integer :: ierr, i   !< Result status (UG_NOERR if successful).
      integer, intent(inout) :: meshid
      type(t_ug_mesh), allocatable :: newmeshids(:)
      character(len=256), allocatable :: newmeshnames(:)
      integer :: nmesh, npresentmeshes

      ! Count nr of meshes present in the file
      ierr = ug_get_mesh_count(ncid, npresentmeshes)
      if (ierr /= UG_NOERR) then
         ierr = UG_SOMEERR
         goto 999
      end if

      nmesh = npresentmeshes + 1
      ug_file%nummesh = nmesh

      allocate (newmeshids(nmesh), stat=ierr)
      if (ierr /= 0) then
         ierr = UG_SOMEERR
         goto 999
      end if

      allocate (newmeshnames(nmesh), stat=ierr)
      if (ierr /= 0) then
         ierr = UG_SOMEERR
         goto 999
      end if

      if (npresentmeshes > 0) then
         do i = 1, npresentmeshes
            newmeshids(i)%dimids = ug_file%meshids(i)%dimids
            newmeshids(i)%varids = ug_file%meshids(i)%varids
            newmeshnames(i) = ug_file%meshnames(i)
         end do
      end if

      ! here we pass the ownership of the allocated space from newmeshids to ug_file%meshidsvec
      call move_alloc(newmeshids, ug_file%meshids)
      call move_alloc(newmeshnames, ug_file%meshnames)

      meshid = nmesh

999   continue

   end function ug_add_mesh

!> This function creates new contactsids structure
   function ug_add_mesh_contact(ncid, ug_file, contactsmesh) result(ierr)

      integer, intent(in) :: ncid    !< ID of already opened data set.
      type(t_ug_file), intent(inout) :: ug_file !< UGRID file struct with cached meta information.
      integer :: ierr    !< Result status (UG_NOERR if successful).
      integer, intent(inout) :: contactsmesh
      type(t_ug_contacts), allocatable :: newcontacts(:)
      character(len=256), allocatable :: newcontactsnames(:)
      integer :: npresentcontactmeshes, ncontactmeshes, i

      ! Count nr of meshes present in the file
      ierr = ug_get_contact_topo_count(ncid, npresentcontactmeshes)
      if (ierr /= UG_NOERR) then
         ierr = UG_SOMEERR
         goto 999
      end if

      ncontactmeshes = npresentcontactmeshes + 1
      ug_file%numcontacts = ncontactmeshes

      allocate (newcontacts(ncontactmeshes), stat=ierr)
      if (ierr /= 0) then
         ierr = UG_SOMEERR
         goto 999
      end if

      allocate (newcontactsnames(ncontactmeshes), stat=ierr)
      if (ierr /= 0) then
         ierr = UG_SOMEERR
         goto 999
      end if
      if (npresentcontactmeshes > 0) then
         do i = 1, npresentcontactmeshes
            newcontacts(i)%dimids = ug_file%contactids(i)%dimids
            newcontacts(i)%varids = ug_file%contactids(i)%varids
            newcontactsnames(i) = ug_file%contactsnames(i)
         end do
      end if

      ! here we pass the ownership of the allocated space from newcontacts to ug_file%contactids
      call move_alloc(newcontacts, ug_file%contactids)
      call move_alloc(newcontactsnames, ug_file%contactsnames)
      contactsmesh = ncontactmeshes

999   continue

   end function ug_add_mesh_contact

!> This function creates a new network information, these will be added later
   function ug_add_network(ncid, ug_file, networkid) result(ierr)

      integer, intent(in) :: ncid    !< ID of already opened data set.
      type(t_ug_file), intent(inout) :: ug_file !< UGRID file struct with cached meta information.
      integer :: ierr, i   !< Result status (UG_NOERR if successful).
      integer, intent(inout) :: networkid
      type(t_ug_network), allocatable :: newnetids(:)
      character(len=256), allocatable :: newnetworksnames(:)
      integer :: nnet, npresentnet

      ! Count nr of meshes present in the file
      ierr = ug_get_network_count(ncid, npresentnet)
      if (ierr /= UG_NOERR) then
         ierr = UG_SOMEERR
         goto 999
      end if

      nnet = npresentnet + 1
      ug_file%numnet = nnet

      allocate (newnetids(nnet), stat=ierr)
      if (ierr /= 0) then
         ierr = UG_SOMEERR
         goto 999
      end if

      allocate (newnetworksnames(nnet), stat=ierr)
      if (ierr /= 0) then
         ierr = UG_SOMEERR
         goto 999
      end if
      if (npresentnet > 0) then
         do i = 1, npresentnet
            newnetids(i)%dimids = ug_file%netids(i)%dimids
            newnetids(i)%varids = ug_file%netids(i)%varids
            newnetworksnames(i) = ug_file%networksnames(i)
         end do
      end if

      ! here we pass the ownership of the allocated space from newnetids to ug_file%meshidsvec
      call move_alloc(newnetids, ug_file%netids)
      call move_alloc(newnetworksnames, ug_file%networksnames)
      networkid = nnet

999   continue

   end function ug_add_network

!> Initialized all UGRID-specific meta information present in an open data set.
   function ug_init_dataset(ncid, ug_file) result(ierr)
      integer, intent(in) :: ncid          !< ID of already opened data set.
      type(t_ug_file), intent(inout) :: ug_file       !< UGRID file struct with cached meta information.
      integer :: ierr          !< Result status (UG_NOERR if successful).

      integer :: im, nmesh, numvar, il, ncontacts, nnetworks, inet
      logical :: is_mesh_topo, is_link_topo, is_network_topo
      integer :: varid         !< NetCDF variable ID (1-based).

      ! Count nr of meshes present in the file
      ierr = ug_get_mesh_count(ncid, nmesh)
      if (ierr /= UG_NOERR) then
         goto 999
      end if
      ug_file%nummesh = nmesh

      allocate (ug_file%meshnames(nmesh)) ! TODO: LC: if allocated deallocate..
      allocate (ug_file%meshids(nmesh), stat=ierr)
      if (ierr /= 0) then
         ierr = UG_SOMEERR
         goto 999
      end if

      ! Count nr of networks
      ierr = ug_get_network_count(ncid, nnetworks)
      if (ierr /= UG_NOERR) then
         goto 999
      end if
      ug_file%numnet = nnetworks

      allocate (ug_file%networksnames(nnetworks))
      allocate (ug_file%netids(nnetworks), stat=ierr)
      if (ierr /= 0) then
         ierr = UG_SOMEERR
         goto 999
      end if

      ! Count nr of ncontacts present in the file
      ierr = ug_get_contact_topo_count(ncid, ncontacts)
      if (ierr /= UG_NOERR) then
         goto 999
      end if
      ug_file%numcontacts = ncontacts

      allocate (ug_file%contactsnames(ncontacts))
      allocate (ug_file%contactids(ncontacts))
      if (ierr /= 0) then
         ierr = UG_SOMEERR
         goto 999
      end if

      ! Now check all variables and if they're a mesh topology, read in all details.
      ierr = nf90_inquire(ncid, nVariables=numVar)

      im = 0
      il = 0
      inet = 0
      do varid = 1, numVar
         is_mesh_topo = ug_is_mesh_topology(ncid, varid)
         is_network_topo = ug_is_network_topology(ncid, varid)
         is_link_topo = ug_is_link_topology(ncid, varid)
         if ((.not. is_mesh_topo) .and. (.not. is_link_topo) .and. (.not. is_network_topo)) then
            cycle
         end if
         !mesh topology case
         if (is_mesh_topo) then
            im = im + 1
            ierr = nf90_inquire_variable(ncid, varid, name=ug_file%meshnames(im))
            ierr = ug_init_mesh_topology(ncid, varid, ug_file%meshids(im))
         end if
         !network topology case
         if (is_network_topo) then
            inet = inet + 1
            ierr = nf90_inquire_variable(ncid, varid, name=ug_file%networksnames(inet))
            ierr = ug_init_network_topology(ncid, varid, ug_file%netids(inet))
         end if
         !link topology case
         if (is_link_topo) then
            il = il + 1
            ierr = nf90_inquire_variable(ncid, varid, name=ug_file%contactsnames(il))
            ierr = ug_init_link_topology(ncid, varid, ug_file%contactids(il))
         end if

         if (ierr /= UG_NOERR) then
            goto 999
         end if
      end do

      ierr = UG_NOERR
      return ! Return with success

999   continue
      ! Some error (status was set earlier)

   end function ug_init_dataset

!> Reads the mesh contact attributes from a NetCDF variable.
   function ug_init_link_topology(ncid, varid, contactids) result(ierr)
      use netcdf_utils, only: ncu_att_to_dimid, ncu_att_to_varid
      use ionc_constants, only: IONC_NOERR

      integer, intent(in) :: ncid          !< ID of already opened data set.
      integer, intent(in) :: varid         !< NetCDF variable ID that contains the link topology information (1-based).
      type(t_ug_contacts), intent(inout) :: contactids    !< vector in which all link topology dimension and variables ids will be stored.
      integer :: ierr          !< Result status (UG_NOERR if successful).
      character(len=nf90_max_name) :: varname
      integer :: tabledims(2)

      ierr = UG_NOERR

      contactids%varids(cid_contacttopo) = varid

      ierr = nf90_inquire_variable(ncid, varid, dimids=tabledims)

      ierr = ncu_att_to_dimid(ncid, varid, 'link_dimension', contactids%dimids(cdim_ncontacts))
      if (ierr /= UG_NOERR) then
         ! The optional link_dimension attribute was not found, so auto-detect it from this contact variable directly.
         contactids%dimids(cdim_ncontacts) = tabledims(2)
         contactids%dimids(cdim_two) = tabledims(1)
      else
         if (contactids%dimids(cdim_ncontacts) == tabledims(1)) then
            contactids%dimids(cdim_two) = tabledims(2)
         else
            contactids%dimids(cdim_two) = tabledims(1)
         end if
      end if

      ierr = nf90_inquire_variable(ncid, varid, varname)

      ierr = ncu_att_to_varid(ncid, varid, 'contact_id', contactids%varids(cid_contactids))
      if (ierr /= IONC_NOERR) then
         ! Backwards compatible read of Deltares-0.9 plural-names.
         ierr = ncu_att_to_varid(ncid, varid, 'contact_ids', contactids%varids(cid_contactids))
      end if
      ! Detect string length dimension for Ids:
      if (ierr == IONC_NOERR) then
         ierr = nf90_inquire_variable(ncid, contactids%varids(cid_contactids), dimids=tabledims) ! string array dimensions
         if (ierr == nf90_noerr) then
            contactids%dimids(cdim_idstring) = tabledims(1)
         end if
      end if
      ! Fallback to old hardcoded name
      if (ierr /= IONC_NOERR) then
         ierr = nf90_inq_dimid(ncid, 'strLengthIds', contactids%dimids(cdim_idstring))
      end if

      ierr = ncu_att_to_varid(ncid, varid, 'contact_long_name', contactids%varids(cid_contactlongnames))
      if (ierr /= UG_NOERR) then
         ! Backwards compatible read of Deltares-0.9 plural-names.
         ierr = ncu_att_to_varid(ncid, varid, 'contact_long_names', contactids%varids(cid_contactlongnames))
      end if
      ! Detect string length dimension for longnames:
      if (ierr == IONC_NOERR) then
         ierr = nf90_inquire_variable(ncid, contactids%varids(cid_contactlongnames), dimids=tabledims) ! string array dimensions
         if (ierr == nf90_noerr) then
            contactids%dimids(cdim_longnamestring) = tabledims(1)
         end if
      end if
      ! Fallback to old hardcoded name
      if (ierr /= IONC_NOERR) then
         ierr = nf90_inq_dimid(ncid, 'strLengthIds', contactids%dimids(cdim_longnamestring))
      end if

      ierr = ncu_att_to_varid(ncid, varid, 'contact_type', contactids%varids(cid_contacttype))

      ierr = UG_NOERR

   end function ug_init_link_topology

   function ug_init_network_topology(ncid, varid, netids) result(ierr)
      use netcdf_utils, only: ncu_att_to_dimid, ncu_att_to_varid

      integer, intent(in) :: ncid          !< ID of already opened data set.
      integer, intent(in) :: varid         !< NetCDF variable ID that contains the network topology information (1-based).
      type(t_ug_network), intent(inout) :: netids        !< vector in which all mesh topology dimension and variables ids will be stored.
      character(len=nf90_max_name) :: varname       !< char array  to hold the network name
      integer :: ierr          !< Result status (UG_NOERR if successful).
      integer :: dimids(2)

      ierr = UG_NOERR

      netids%varids(ntid_1dtopo) = varid
      ierr = nf90_inquire_variable(ncid, varid, name=varname)

      ierr = ncu_att_to_dimid(ncid, varid, 'edge_dimension', netids%dimids(ntdim_1dedges))
      if (ierr /= UG_NOERR) then
         call SetMessage(LEVEL_WARN, 'ug_init_network_topology: Could not find dimension for network edges (see '//trim(varname)//':edge_dimension).')
      end if

      ierr = ncu_att_to_dimid(ncid, varid, 'node_dimension', netids%dimids(ntdim_1dnodes))
      if (ierr /= UG_NOERR) then
         call SetMessage(LEVEL_WARN, 'ug_init_network_topology: Could not find dimension for network nodes (see '//trim(varname)//':node_dimension).')
      end if
      !edge_geometry container
      ierr = ncu_att_to_varid(ncid, varid, 'edge_geometry', netids%varids(ntid_1dgeometry))
      if (ierr /= UG_NOERR) then
         call SetMessage(LEVEL_WARN, 'ug_init_network_topology: Could not find variable with network geometry (see '//trim(varname)//':edge_geometry).')
      end if
      !geometry x and  y
      ierr = att_to_coordvarids(ncid, netids%varids(ntid_1dgeometry), 'node_coordinates', netids%varids(ntid_1dgeox), netids%varids(ntid_1dgeoy))
      !ndim_1dgeopoints
      ierr = nf90_inquire_variable(ncid, netids%varids(ntid_1dgeox), dimids=netids%dimids(ntdim_1dgeopoints:ntdim_1dgeopoints))

      !node variables ids
      ierr = att_to_coordvarids(ncid, varid, 'node_coordinates', netids%varids(ntid_1dnodex), netids%varids(ntid_1dnodey))
      ierr = ncu_att_to_varid(ncid, varid, 'node_id', netids%varids(ntid_1dnodids))
      if (ierr /= UG_NOERR) then
         ! Backwards compatible read of Deltares-0.9 plural-names.
         ierr = ncu_att_to_varid(ncid, varid, 'node_ids', netids%varids(ntid_1dnodids))
      end if
      ierr = ncu_att_to_varid(ncid, varid, 'node_long_name', netids%varids(ntid_1dnodlongnames))
      if (ierr /= UG_NOERR) then
         ! Backwards compatible read of Deltares-0.9 plural-names.
         ierr = ncu_att_to_varid(ncid, varid, 'node_long_names', netids%varids(ntid_1dnodlongnames))
      end if
      !branch variables ids
      ierr = ncu_att_to_varid(ncid, varid, 'edge_node_connectivity', netids%varids(ntid_1dedgenodes))
      ierr = ncu_att_to_varid(ncid, varid, 'branch_id', netids%varids(ntid_1dbranchids))
      if (ierr /= UG_NOERR) then
         ! Backwards compatible read of Deltares-0.9 plural-names.
         ierr = ncu_att_to_varid(ncid, varid, 'branch_ids', netids%varids(ntid_1dbranchids))
      end if
      ierr = ncu_att_to_varid(ncid, varid, 'branch_long_name', netids%varids(ntid_1dbranchlongnames)) ! TODO: LC: error when not found
      if (ierr /= UG_NOERR) then
         ! Backwards compatible read of Deltares-0.9 plural-names.
         ierr = ncu_att_to_varid(ncid, varid, 'branch_long_names', netids%varids(ntid_1dbranchlongnames)) ! TODO: LC: error when not found
      end if

      ! branch_lengths/edge_length
      ierr = ncu_att_to_varid(ncid, varid, 'edge_length', netids%varids(ntid_1dbranchlengths))
      if (ierr /= UG_NOERR) then
         ! for backward compatibility: branch_lengths was used rather than edge_length. If there get the varid of the attribute
         ierr = ncu_att_to_varid(ncid, varid, 'branch_lengths', netids%varids(ntid_1dbranchlengths))
      end if

      ! get the number of geometric points for each branch
      ! try to get the node_count
      ierr = ncu_att_to_varid(ncid, netids%varids(ntid_1dgeometry), 'node_count', netids%varids(ntid_1dgeopointsperbranch))
      if (ierr /= UG_NOERR) then
         ! for backward compatibility: part_node_count was used rather than node_count. If there get the varid of the attribute
         ierr = ncu_att_to_varid(ncid, netids%varids(ntid_1dgeometry), 'part_node_count', netids%varids(ntid_1dgeopointsperbranch))
      end if

      !dim variables
      ierr = nf90_inquire_variable(ncid, netids%varids(ntid_1dedgenodes), dimids=dimids)
      netids%dimids(ntdim_Two) = dimids(1)
      ierr = nf90_inquire_variable(ncid, netids%varids(ntid_1dbranchids), dimids=dimids)
      netids%dimids(ntdim_idstring) = dimids(1)
      ierr = nf90_inquire_variable(ncid, netids%varids(ntid_1dbranchlongnames), dimids=dimids)
      netids%dimids(ntdim_longnamestring) = dimids(1)

      !read the branch order
      ierr = nf90_inquire_variable(ncid, varid, name=varname)
      ierr = nf90_inq_varid(ncid, trim(varname)//'_branch_order', netids%varids(ntid_1dbranchorder))

      !read the branch type
      ierr = nf90_inquire_variable(ncid, varid, name=varname)
      ierr = nf90_inq_varid(ncid, trim(varname)//'_branch_type', netids%varids(ntid_1dbranchtype))

      ierr = UG_NOERR

   end function ug_init_network_topology

!> Reads the mesh_topology attributes from a NetCDF variable.
   function ug_init_mesh_topology(ncid, varid, meshids) result(ierr)
      use netcdf_utils, only: ncu_get_att, ncu_att_to_dimid, ncu_att_to_varid

      integer, intent(in) :: ncid          !< ID of already opened data set.
      integer, intent(in) :: varid         !< NetCDF variable ID that contains the mesh topology information (1-based).
      type(t_ug_mesh), intent(inout) :: meshids          !< vector in which all mesh topology dimension and variables ids will be stored.
      integer :: ierr          !< Result status (UG_NOERR if successful).
      character(len=:), allocatable :: coordspaceind !< The name of the network used by the mesh
      character(len=nf90_max_name) :: varname
      integer :: dimids(2)
      integer :: isMappedMesh

      ierr = UG_NOERR
      allocate (character(len=0) :: coordspaceind)

      !< Top-level variable ID for mesh topology, collects all related variable names via attributes.

      isMappedMesh = -1
      meshids%varids(mid_meshtopo) = varid
      !
      ! Dimensions:
      !
      ierr = ncu_att_to_dimid(ncid, varid, 'node_dimension', meshids%dimids(mdim_node))
      ierr = ncu_att_to_dimid(ncid, varid, 'edge_dimension', meshids%dimids(mdim_edge))
      ierr = ncu_att_to_dimid(ncid, varid, 'face_dimension', meshids%dimids(mdim_face))
      ierr = ncu_att_to_dimid(ncid, varid, 'max_face_nodes_dimension', meshids%dimids(mdim_maxfacenodes))
      ! Dimension 2 might already be present
      ierr = nf90_inq_dimid(ncid, 'Two', meshids%dimids(mdim_two))
      ! Otherwise check another possible definitions
      if (ierr /= UG_NOERR) then
         ierr = nf90_inquire_variable(ncid, varid, name=varname)
         varname = 'n'//trim(varname)//'_Two'  ! varname has defined lenth of nf90_var_name
         ierr = nf90_inq_dimid(ncid, trim(varname), meshids%dimids(mdim_two))
      end if
      ! Dimensions for 3D variables
      ierr = ncu_att_to_dimid(ncid, varid, 'layer_dimension', meshids%dimids(mdim_layer))
      if (ierr /= UG_NOERR) then ! 3D dimension does not exist
         ierr = UG_NOERR
      else
         ierr = ncu_att_to_dimid(ncid, varid, 'interface_dimension', meshids%dimids(mdim_interface))
      end if

      !check here if this is a mapped mesh
      coordspaceind = ''
      isMappedMesh = ncu_get_att(ncid, varid, 'coordinate_space', coordspaceind)
      deallocate (coordspaceind)  ! not used anymore
      if (isMappedMesh == nf90_noerr) then
         !inquire the variable with that name
         ierr = ncu_att_to_varid(ncid, varid, 'coordinate_space', meshids%varids(mid_1dtopo))
         !read branch id and offsets
         ierr = att_to_coordvarids(ncid, meshids%varids(mid_meshtopo), 'node_coordinates', meshids%varids(mid_1dnodebranch), meshids%varids(mid_1dnodeoffset), meshids%varids(mid_nodex), meshids%varids(mid_nodey))
         ierr = att_to_coordvarids(ncid, meshids%varids(mid_meshtopo), 'edge_coordinates', meshids%varids(mid_1dedgebranch), meshids%varids(mid_1dedgeoffset), meshids%varids(mid_edgex), meshids%varids(mid_edgey))
      end if

      !
      ! Coordinate variables
      !
      if (isMappedMesh /= nf90_noerr) then
         ierr = att_to_coordvarids(ncid, varid, 'node_coordinates', meshids%varids(mid_nodex), meshids%varids(mid_nodey))
         ! The optional node_dimension attribute was not found, so auto-detect it from a node coordinates variable.
         if (ierr == nf90_noerr .and. meshids%dimids(mdim_node) == -1) then
            ierr = varid_to_dimid(ncid, meshids%varids(mid_nodex), meshids%dimids(mdim_node))
         end if
         ! TODO: UNST-2763: once we support both branchid/offset AND x/y, move the next line out of this IF again.
         ierr = att_to_coordvarids(ncid, varid, 'edge_coordinates', meshids%varids(mid_edgex), meshids%varids(mid_edgey))
      end if
      ierr = att_to_coordvarids(ncid, varid, 'face_coordinates', meshids%varids(mid_facex), meshids%varids(mid_facey))

      !
      ! Topology variables
      !

      ierr = ncu_att_to_varid(ncid, varid, 'edge_node_connectivity', meshids%varids(mid_edgenodes)) !< Variable ID for edge-to-node mapping table.
      if (ierr == nf90_noerr) then
         ! The optional edge_dimension attribute was not found, so auto-detect it from the edge_node topology.
         if (meshids%dimids(mdim_edge) == -1) then
            ierr = varid_to_dimid(ncid, meshids%varids(mid_edgenodes), meshids%dimids(mdim_edge), dimidx=2)
         end if
      end if

      ierr = ncu_att_to_varid(ncid, varid, 'face_node_connectivity', meshids%varids(mid_facenodes)) !< Variable ID for face-to-node mapping table.
      if (ierr == nf90_noerr) then
         ! The optional face_dimension attribute was not found, so auto-detect it from the face_node topology.
         if (meshids%dimids(mdim_face) == -1) then
            ierr = varid_to_dimid(ncid, meshids%varids(mid_facenodes), meshids%dimids(mdim_face), dimidx=2)
         end if

         ! The non-UGRID max_face_nodes_dimension was not found. Detect it ourselves.
         if (meshids%dimids(mdim_maxfacenodes) == -1) then
            ! Get the dimension ids from the face_nodes variable, and select the correct one from that.
            ierr = nf90_inquire_variable(ncid, meshids%varids(mid_facenodes), dimids=dimids)
            if (ierr == nf90_noerr) then
               if (dimids(1) == meshids%dimids(mdim_face)) then
                  meshids%dimids(mdim_maxfacenodes) = dimids(2) ! the other
               else
                  meshids%dimids(mdim_maxfacenodes) = dimids(1)
               end if
            end if
         end if
      end if

      ierr = ncu_att_to_varid(ncid, varid, 'edge_face_connectivity', meshids%varids(mid_edgefaces)) !< Variable ID for edge-to-face mapping table (optional, can be -1).
      ierr = ncu_att_to_varid(ncid, varid, 'face_edge_connectivity', meshids%varids(mid_faceedges)) !< Variable ID for face-to-edge mapping table (optional, can be -1).
      ierr = ncu_att_to_varid(ncid, varid, 'face_face_connectivity', meshids%varids(mid_facelinks)) !< Variable ID for face-to-face mapping table (optional, can be -1).

      !
      ! Get the ids defined in nodes/edges/faces
      !
      ierr = ncu_att_to_varid(ncid, varid, 'node_id', meshids%varids(mid_node_ids)) !< Variable ID for node ids
      if (ierr /= UG_NOERR) then
         ! Backwards compatible read of Deltares-0.9 plural-names.
         ierr = ncu_att_to_varid(ncid, varid, 'node_ids', meshids%varids(mid_node_ids)) !< Variable ID for node ids
      end if
      ierr = ncu_att_to_varid(ncid, varid, 'edge_id', meshids%varids(mid_edge_ids)) !< Variable ID for edge ids
      if (ierr /= UG_NOERR) then
         ! Backwards compatible read of Deltares-0.9 plural-names.
         ierr = ncu_att_to_varid(ncid, varid, 'edge_ids', meshids%varids(mid_edge_ids)) !< Variable ID for edge ids
      end if
      ierr = ncu_att_to_varid(ncid, varid, 'face_id', meshids%varids(mid_face_ids)) !< Variable ID for face ids
      if (ierr /= UG_NOERR) then
         ! Backwards compatible read of Deltares-0.9 plural-names.
         ierr = ncu_att_to_varid(ncid, varid, 'face_ids', meshids%varids(mid_face_ids)) !< Variable ID for face ids
      end if

      !
      ! Get the longnames defined in nodes/edges/faces
      !
      ierr = ncu_att_to_varid(ncid, varid, 'node_long_name', meshids%varids(mid_node_longnames)) !< Variable ID for node ids
      if (ierr /= UG_NOERR) then
         ! Backwards compatible read of Deltares-0.9 plural-names.
         ierr = ncu_att_to_varid(ncid, varid, 'node_long_names', meshids%varids(mid_node_longnames)) !< Variable ID for node ids
      end if
      ierr = ncu_att_to_varid(ncid, varid, 'edge_long_name', meshids%varids(mid_edge_longnames)) !< Variable ID for edge ids
      if (ierr /= UG_NOERR) then
         ! Backwards compatible read of Deltares-0.9 plural-names.
         ierr = ncu_att_to_varid(ncid, varid, 'edge_long_names', meshids%varids(mid_edge_longnames)) !< Variable ID for edge ids
      end if
      ierr = ncu_att_to_varid(ncid, varid, 'face_long_name', meshids%varids(mid_face_longnames)) !< Variable ID for face ids
      if (ierr /= UG_NOERR) then
         ! Backwards compatible read of Deltares-0.9 plural-names.
         ierr = ncu_att_to_varid(ncid, varid, 'face_long_names', meshids%varids(mid_face_longnames)) !< Variable ID for face ids
      end if

      ierr = UG_NOERR

   end function ug_init_mesh_topology

!> Inquire for NetCDF variable IDs based on some
!! coordinates attribute in a container variable.
!! For example: mesh1d:node_coordinates
   function att_to_coordvarids(ncid, varid, attname, idx, idy, idz, idw) result(ierr)
      use netcdf_utils, only: ncu_get_att

      integer, intent(in) :: ncid     !< NetCDF dataset ID
      integer, intent(in) :: varid    !< NetCDF variable ID from which the coordinate attribute will be gotten (1-based).
      character(len=*), intent(in) :: attname  !< Name of attribute in varid that contains the coordinate variable names.
      integer, intent(out) :: idx, idy !< NetCDF variable ID for x,y-coordinates.
      integer, optional, intent(out) :: idz      !< NetCDF variable ID for z-coordinates.
      integer, optional, intent(out) :: idw      !< NetCDF variable ID for additional coordinate
      integer :: ierr     !< Result status. NF90_NOERR if successful.
      character(len=:), allocatable :: varname

      integer :: i1, i2, n

      allocate (character(len=0) :: varname)
      varname = ''
      ierr = ncu_get_att(ncid, varid, attname, varname)
      if (ierr /= nf90_noerr) then
         goto 999
      end if

      i1 = 1
      n = len_trim(varname)

      ! TODO: AvD: I'd rather use a string tokenizer here.
      ! TODO: UNST-2408, support multiple sets of coordinates.
      i2 = index(varname(i1:n), ' ')
      if (i2 == 0) then
         i2 = n
      else
         i2 = i1 + i2 - 1
      end if
      ierr = nf90_inq_varid(ncid, varname(i1:i2 - 1), idx)
      i1 = i2 + 1

      i2 = index(varname(i1:n), ' ')
      if (i2 == 0) then
         i2 = n + 1
      else
         i2 = i1 + i2 - 1
      end if
      ierr = nf90_inq_varid(ncid, varname(i1:i2 - 1), idy)
      i1 = i2 + 1

      if (present(idz)) then
         i2 = index(varname(i1:n), ' ')
         if (i2 == 0) then
            i2 = n + 1
         else
            i2 = i1 + i2 - 1
         end if
         ierr = nf90_inq_varid(ncid, varname(i1:i2 - 1), idz)
         i1 = i2 + 1
      end if

      if (present(idw)) then
         i2 = index(varname(i1:n), ' ')
         if (i2 == 0) then
            i2 = n + 1
         else
            i2 = i1 + i2 - 1
         end if
         ierr = nf90_inq_varid(ncid, varname(i1:i2 - 1), idw)
         i1 = i2 + 1
      end if

      deallocate (varname)

      return

999   continue
      ! Some error
   end function att_to_coordvarids

!> Gets a single dimension ID for a given variable ID.
!! Used for example to determine node dimension id from a node coordinate variable.
   function varid_to_dimid(ncid, varid, dimid, dimidx) result(ierr)

      integer, intent(in) :: ncid   !< NetCDF dataset ID
      integer, intent(in) :: varid  !< NetCDF variable ID from which the dimension ID will be gotten (1-based).
      integer, intent(out) :: dimid  !< NetCDF dimension ID that was found.
      integer, optional, intent(in) :: dimidx !< Optional index of the desired dimension, in case of a multidimensional variable. Default: 1.
      integer :: ierr   !< Result status. UG_NOERR if successful.

      integer, dimension(nf90_max_dims) :: dimids
      integer :: dimidx_, ndims

      ierr = UG_NOERR

      if (present(dimidx)) then
         dimidx_ = dimidx
      else
         dimidx_ = 1
      end if

      ierr = nf90_inquire_variable(ncid, varid, ndims=ndims, dimids=dimids)
      if (ierr /= nf90_noerr .or. dimidx_ > ndims) then
         goto 999
      end if

      dimid = dimids(dimidx_)

      return

999   continue
      ! here we should return an error is the variable is undefined, following up actions are taken in ug_init_mesh_topology
      dimid = -1           ! undefined id
   end function varid_to_dimid

!> Returns whether a given variable is a mesh topology variable.
   function ug_is_mesh_topology(ncid, varid) result(is_mesh_topo)
      use netcdf_utils, only: ncu_get_att

      integer, intent(in) :: ncid         !< NetCDF dataset id
      integer, intent(in) :: varid        !< NetCDF variable id (1-based).
      logical :: is_mesh_topo !< Return value

      integer :: ierr, topology_dimension
      character(len=:), allocatable :: buffer
      integer :: ndum

      is_mesh_topo = .false.
      allocate (character(len=0) :: buffer)

      topology_dimension = -1
      buffer = ''
      ierr = ncu_get_att(ncid, varid, 'cf_role', buffer)

      if (trim(buffer) == 'mesh_topology') then
         ! Some data producers confusingly use 'mesh_topology' for parent meshes too (instead of 'parent_mesh_topology').
         ! We want to filter those out here: if the var has an attribute :meshes (with any value), then discard.
         ierr = nf90_inquire_attribute(ncid, varid, 'meshes', ndum)
         if (ierr /= nf90_enotatt) then
            is_mesh_topo = .false.
            return
         end if

         ierr = nf90_get_att(ncid, varid, 'topology_dimension', topology_dimension)

         if (topology_dimension == 1) then !1d case
            buffer = ''
            ierr = ncu_get_att(ncid, varid, 'edge_geometry', buffer)
            if (ierr /= nf90_noerr) then
               is_mesh_topo = .true. !true only if does not contain edge_geometry
            end if
         else if (topology_dimension > 1) then
            is_mesh_topo = .true.
         end if
      end if
      deallocate (buffer)

   end function ug_is_mesh_topology

!> Returns whether a given variable is a mesh topology variable.
   function ug_is_network_topology(ncid, varid) result(is_mesh_topo)
      use netcdf_utils, only: ncu_get_att

      integer, intent(in) :: ncid         !< NetCDF dataset id
      integer, intent(in) :: varid        !< NetCDF variable id (1-based).
      logical :: is_mesh_topo !< Return value

      character(len=:), allocatable :: buffer
      integer :: mesh_topo
      integer :: edge_geom

      is_mesh_topo = .false.
      allocate (character(len=0) :: buffer)

      buffer = ''
      mesh_topo = ncu_get_att(ncid, varid, 'cf_role', buffer); 
      if (mesh_topo == nf90_noerr) then
         buffer = ''
         edge_geom = ncu_get_att(ncid, varid, 'edge_geometry', buffer); 
         if (edge_geom == nf90_noerr) then
            is_mesh_topo = .true. ! new ugrid format detected
         end if
      end if
      deallocate (buffer)

   end function ug_is_network_topology

!> Returns whether a given variable is a link topology / mesh_topology_contact
   function ug_is_link_topology(ncid, varid) result(ug_is_link_topo)
      use netcdf_utils, only: ncu_get_att

      integer, intent(in) :: ncid         !< NetCDF dataset id
      integer, intent(in) :: varid        !< NetCDF variable id (1-based).
      logical :: ug_is_link_topo !< Return value
      integer :: cfrole
      character(len=:), allocatable :: buffer

      ug_is_link_topo = .false.
      allocate (character(len=0) :: buffer)

      buffer = ''
      cfrole = ncu_get_att(ncid, varid, 'cf_role', buffer)
      if (cfrole == nf90_noerr .and. trim(buffer) == 'mesh_topology_contact') then
         ug_is_link_topo = .true. !new link topology detected
      end if
      deallocate (buffer)
   end function ug_is_link_topology

!> Gets the number of mesh topologies in an open dataset.
!! use this to determine on how many different meshes, data is defined in the dataset.
!!
!! \see
   function ug_get_mesh_count(ncid, numMesh) result(ierr)
      integer, intent(in) :: ncid     !< NetCDF dataset id
      integer, intent(out) :: numMesh  !< Number of mesh topologies in the dataset (>= 0).
      integer :: ierr     !< Result status (UG_NOERR==NF90_NOERR) if successful.

      integer :: numVar
      logical :: is_mesh_topo
      integer :: varid         !< NetCDF variable ID (1-based).

      ierr = nf90_inquire(ncid, nVariables=numVar)

      numMesh = 0
      do varid = 1, numVar
         is_mesh_topo = ug_is_mesh_topology(ncid, varid)
         if (is_mesh_topo) then
            numMesh = numMesh + 1
         end if
      end do

   end function ug_get_mesh_count

   function ug_get_network_count(ncid, numNet) result(ierr)
      integer, intent(in) :: ncid     !< NetCDF dataset id
      integer, intent(out) :: numNet   !< Number of mesh topologies in the dataset (>= 0).
      integer :: ierr     !< Result status (UG_NOERR==NF90_NOERR) if successful.

      integer :: numVar
      logical :: is_net_topo
      integer :: iworkaround1
      integer :: varid         !< NetCDF variable ID (1-based).

      ! NOTE: AvD:
      ! Just like in ug_is_network_topology(), we suffer from a same Heisenbug in netcdf lib:
      ! Directly working with ncid does give correct numVar, but still numNet=0.
      ! Using this iworkaround1 strangely fixes it again.
      iworkaround1 = ncid
      ierr = nf90_inquire(iworkaround1, nVariables=numVar)

      numNet = 0
      do varid = 1, numVar
         is_net_topo = ug_is_network_topology(ncid, varid)
         if (is_net_topo) then
            numNet = numNet + 1
         end if
      end do

   end function ug_get_network_count

!> Gets the number of link topologies in an open dataset.
   function ug_get_contact_topo_count(ncid, ncontacts) result(ierr)
      integer, intent(in) :: ncid      !< NetCDF dataset id
      integer, intent(out) :: ncontacts !< Number of links (>= 0).
      integer :: ierr      !< Result status (UG_NOERR==NF90_NOERR) if successful.
      integer :: numVar    !< number of variables in the netCDF file.
      integer :: varid     !< NetCDF variable ID (1-based).
      logical :: is_link_topo

      ierr = nf90_inquire(ncid, nVariables=numVar)

      ncontacts = 0
      do varid = 1, numVar
         is_link_topo = ug_is_link_topology(ncid, varid)
         if (is_link_topo) then
            ncontacts = ncontacts + 1
         end if
      end do

   end function ug_get_contact_topo_count

!> Gets the name of the mesh topology variable in an open dataset.
!!
!! \see
   function ug_get_mesh_name(ncid, meshids, meshname) result(ierr)
      integer, intent(in) :: ncid     !< NetCDF dataset id, should be already open.
      type(t_ug_mesh), intent(in) :: meshids  !< Set of NetCDF-ids for all mesh geometry arrays.
      character(len=*), intent(out) :: meshname !< The name of the mesh topology variable.
      integer :: ierr     !< Result status, ug_noerr if successful.

      meshname = ''
      ierr = nf90_inquire_variable(ncid, meshids%varids(mid_meshtopo), name=meshname)
      if (ierr /= nf90_noerr) then
         write (ug_messagestr, '(a,i0)') 'ug_get_mesh_name: could not find meshname for topology var id ', meshids%varids(mid_meshtopo)
         ierr = UG_INVALID_MESHNAME
         call SetMessage(Level_Fatal, ug_messagestr)
      end if
   end function ug_get_mesh_name

!> Gets the name of the network topology variable in an open dataset.
!!
!! \see
   function ug_get_network_name(ncid, netids, networkname) result(ierr)
      integer, intent(in) :: ncid     !< NetCDF dataset id, should be already open.
      type(t_ug_network), intent(in) :: netids  !< Set of NetCDF-ids for all mesh geometry arrays.
      character(len=*), intent(out) :: networkname !< The name of the mesh topology variable.
      integer :: ierr     !< Result status, ug_noerr if successful.

      networkname = ''
      ierr = nf90_inquire_variable(ncid, netids%varids(ntid_1dtopo), name=networkname)
      if (ierr /= nf90_noerr) then
         write (ug_messagestr, '(a,i0)') 'ug_get_network_name: could not find networkname for topology var id ', netids%varids(ntid_1dtopo)
         ierr = UG_INVALID_NETNAME
         call SetMessage(Level_Fatal, ug_messagestr)
      end if
   end function ug_get_network_name

!> Gets the name of the network topology variable in an open dataset.
!!
!! \see
   function ug_get_mesh_network_name(ncid, meshids, networkname) result(ierr)
      integer, intent(in) :: ncid         !< NetCDF dataset id, should be already open.
      type(t_ug_mesh), intent(in) :: meshids      !< Set of NetCDF-ids for all mesh geometry arrays.
      character(len=*), intent(out) :: networkname  !< The name of the mesh topology variable.
      integer :: ierr         !< Result status, ug_noerr if successful.

      ierr = UG_NOERR
      networkname = ''
      if (meshids%varids(mid_1dtopo) /= -1) then
         ierr = nf90_inquire_variable(ncid, meshids%varids(mid_1dtopo), name=networkname)

         if (ierr /= nf90_noerr) then
            write (ug_messagestr, '(a,i0)') 'ug_get_network_name: could not find networkname for mesh ', meshids%varids(mid_1dtopo)
            ierr = UG_INVALID_NETNAME
            call SetMessage(Level_Fatal, ug_messagestr)
         end if
      end if

   end function ug_get_mesh_network_name

   function ug_get_network_name_from_mesh1d(ncid, meshids, network1d) result(ierr)

      integer, intent(in) :: ncid
      type(t_ug_mesh), intent(in) :: meshids
      character(len=nf90_max_name) :: att
      integer :: attlen, i, ierr
      character(len=*), intent(inout) :: network1d

      ierr = UG_SOMEERR
      ierr = nf90_inquire_attribute(ncid, meshids%varids(mid_meshtopo), 'coordinate_space', len=attlen)
      ierr = nf90_get_att(ncid, meshids%varids(mid_meshtopo), 'coordinate_space', att)

      network1d = ''
      do i = 1, attlen
         network1d = trim(network1d)//att(i:i)
      end do

   end function ug_get_network_name_from_mesh1d

!> Gets the size/count of items for the specified topological location.
!! Use this to get the number of nodes/edges/faces/volumes.
   function ug_inquire_dimension(ncid, meshids, idimtype, nitems) result(ierr)
      integer, intent(in) :: ncid     !< NetCDF dataset id, should be already open.
      type(t_ug_mesh), intent(in) :: meshids  !< Set of NetCDF-ids for all mesh geometry arrays.
      integer, intent(in) :: idimtype !< The location type to count (one of UG_LOC_NODE, UG_LOC_EDGE, UG_LOC_FACE, UG_LOC_VOL).
      integer, intent(out) :: nitems   !< The number of items for that location.
      integer :: ierr     !< Result status (UG_NOERR==NF90_NOERR if successful).

      integer :: idim

      select case (idimtype)
      case (UG_LOC_NODE)
         idim = meshids%dimids(mdim_node)
      case (UG_LOC_EDGE)
         idim = meshids%dimids(mdim_edge)
      case (UG_LOC_FACE)
         idim = meshids%dimids(mdim_face)
      case (UG_DIM_MAXFACENODES)
         idim = meshids%dimids(mdim_maxfacenodes)
      case default
         ierr = UG_NOTIMPLEMENTED
         goto 999
      end select

      if (idim > 0) then
         ierr = nf90_inquire_dimension(ncid, idim, len=nitems)
      else
         nitems = -1
         ierr = 0 !invalid location is not considered an error
      end if

      ! Success
      return

999   continue
      ! Some error
   end function ug_inquire_dimension

!> Gets the dimension of the mesh topology for the specified mesh in a UGRID data set.
   function ug_get_topology_dimension(ncid, meshids, dim) result(ierr)
      integer, intent(in) :: ncid     !< NetCDF dataset id, should be already open.
      type(t_ug_mesh), intent(in) :: meshids !< Set of NetCDF-ids for all mesh geometry arrays.
      integer, intent(out) :: dim      !< The topology dimension for the requested mesh.
      integer :: ierr     !< Result status (UG_NOERR==NF90_NOERR if successful).

      ierr = UG_NOERR
      ierr = nf90_get_att(ncid, meshids%varids(mid_meshtopo), 'topology_dimension', dim)

      ! Success
      return

999   continue
      ! Some error
   end function ug_get_topology_dimension

!> Reads the actual mesh geometry from the specified mesh in a UGRID dataset.
!! By default only reads in the dimensions (face/edge/node counts).
!! Optionally, also all coordinate arrays + connectivity tables can be read.
   function ug_get_meshgeom(ncid, meshgeom, start_index, meshids, netid, includeArrays, nbranchids, nbranchlongnames, nnodeids, nnodelongnames, nodeids, nodelongnames, network1dname, mesh1dname) result(ierr)

      use m_alloc

      integer, intent(in) :: ncid          !< ID of already opened data set.
      type(t_ug_meshgeom), intent(inout) :: meshgeom      !< Structure in which all mesh geometry will be stored.
      integer, intent(in) :: start_index   !< the base index of the meshgeom arrays

      ! Optional variables
      type(t_ug_mesh), optional, intent(in) :: meshids       !< Structure with all mesh topology variable ids (should be initialized already).
      type(t_ug_network), optional, intent(in) :: netid         !< (optional) The network associated with the mesh (for 1d Ugrid)
      logical, optional, intent(in) :: includeArrays !< (optional) Whether or not to include coordinate arrays and connectivity tables. Default: .false., i.e., dimension counts only.
      character(len=ug_idsLen), optional, allocatable, intent(inout) :: nbranchids(:), nnodeids(:), nodeids(:)
      character(len=ug_idsLongNamesLen), optional, allocatable, intent(inout) :: nbranchlongnames(:), nnodelongnames(:), nodelongnames(:)
      character(len=*), optional, intent(inout) :: network1dname, mesh1dname

      !locals
      integer :: ierr          !< Result status (UG_NOERR if successful).

      logical :: includeArrays_

      ierr = UG_NOERR

      !re-set meshgeom
      ierr = t_ug_meshgeom_destructor(meshgeom)

      !requested by the client
      meshgeom%start_index = start_index

      if (present(includeArrays)) then
         includeArrays_ = includeArrays
      else
         includeArrays_ = .false.
      end if

      !read the network
      if (present(netid)) then

         !We are in 1d ugrid, populate dimensions
         ierr = ug_get_1d_network_branches_count(ncid, netid, meshgeom%nbranches); write (*, '(a,i0)') 'ug_get_meshgeom, #1, ierr=', ierr
         ierr = ug_get_1d_network_branches_geometry_coordinate_count(ncid, netid, meshgeom%ngeometry); write (*, '(a,i0)') 'ug_get_meshgeom, #2, ierr=', ierr
         ierr = ug_get_1d_network_nodes_count(ncid, netid, meshgeom%nnodes); write (*, '(a,i0)') 'ug_get_meshgeom, #3, ierr=', ierr

         if (includeArrays_) then

            call reallocP(meshgeom%nbranchorder, meshgeom%nbranches, keepExisting=.false., fill=-999)
            call reallocP(meshgeom%ngeopointx, meshgeom%ngeometry, keepExisting=.false., fill=-999d0)
            call reallocP(meshgeom%ngeopointy, meshgeom%ngeometry, keepExisting=.false., fill=-999d0)
            ierr = ug_get_1d_network_branchorder(ncid, netid, meshgeom%nbranchorder); write (*, '(a,i0)') 'ug_get_meshgeom, #4, ierr=', ierr
            ierr = ug_read_1d_network_branches_geometry(ncid, netid, meshgeom%ngeopointx, meshgeom%ngeopointy); write (*, '(a,i0)') 'ug_get_meshgeom, #5, ierr=', ierr

            call reallocP(meshgeom%nbranchgeometrynodes, meshgeom%nbranches, keepExisting=.false., fill=-999)
            call reallocP(meshgeom%nedge_nodes, (/2, meshgeom%nbranches/), keepExisting=.false.)
            call reallocP(meshgeom%nbranchlengths, meshgeom%nbranches, keepExisting=.false., fill=-999d0)
            if (present(nbranchids) .and. present(nbranchlongnames)) then
               if (allocated(nbranchids)) deallocate (nbranchids)
               if (allocated(nbranchlongnames)) deallocate (nbranchlongnames)
               allocate (nbranchids(meshgeom%nbranches))
               allocate (nbranchlongnames(meshgeom%nbranches))
               ierr = ug_get_1d_network_branches(ncid, netid, meshgeom%nedge_nodes(1, :), meshgeom%nedge_nodes(2, :), meshgeom%nbranchlengths, meshgeom%nbranchgeometrynodes, meshgeom%start_index, nbranchids, nbranchlongnames); write (*, '(a,i0)') 'ug_get_meshgeom, #6, ierr=', ierr
            else
               ierr = ug_get_1d_network_branches(ncid, netid, meshgeom%nedge_nodes(1, :), meshgeom%nedge_nodes(2, :), meshgeom%nbranchlengths, meshgeom%nbranchgeometrynodes, meshgeom%start_index); write (*, '(a,i0)') 'ug_get_meshgeom, #7, ierr=', ierr
            end if

            call reallocP(meshgeom%nnodex, meshgeom%nnodes, keepExisting=.false., fill=-999d0)
            call reallocP(meshgeom%nnodey, meshgeom%nnodes, keepExisting=.false., fill=-999d0)
            if (present(nnodeids) .and. present(nnodelongnames)) then
               if (allocated(nnodeids)) deallocate (nnodeids)
               if (allocated(nnodelongnames)) deallocate (nnodelongnames)
               allocate (nnodeids(meshgeom%nnodes))
               allocate (nnodelongnames(meshgeom%nnodes))
               ierr = ug_read_1d_network_nodes(ncid, netid, meshgeom%nnodex, meshgeom%nnodey, nnodeids, nnodelongnames); write (*, '(a,i0)') 'ug_get_meshgeom, #8, ierr=', ierr
            else
               ierr = ug_read_1d_network_nodes(ncid, netid, meshgeom%nnodex, meshgeom%nnodey); write (*, '(a,i0)') 'ug_get_meshgeom, #9, ierr=', ierr
            end if

         end if
      end if

      !Read the mesh
      if (present(meshids)) then
         !
         ! Topology dimension:
         !
         ierr = nf90_inquire_variable(ncid, meshids%varids(mid_meshtopo), name=meshgeom%meshname)
         ierr = ug_get_topology_dimension(ncid, meshids, meshgeom%dim)

         !
         ! Dimensions/location counts:
         !
         if (meshgeom%dim == 1) then
            ierr = ug_get_1d_mesh_discretisation_points_count(ncid, meshids, meshgeom%numnode); write (*, '(a,i0)') 'ug_get_meshgeom, #10, ierr=', ierr
         else
            ierr = ug_inquire_dimension(ncid, meshids, UG_LOC_NODE, meshgeom%numnode)
         end if
         ierr = ug_inquire_dimension(ncid, meshids, UG_LOC_EDGE, meshgeom%numedge)
         ierr = ug_inquire_dimension(ncid, meshids, UG_LOC_FACE, meshgeom%numface)
         ierr = ug_inquire_dimension(ncid, meshids, UG_DIM_MAXFACENODES, meshgeom%maxnumfacenodes)

         ! TODO: AvD: extend to 3D
         if (includeArrays_) then
            ! TODO: AvD: inquire proper fillvalue as dmiss from file.

            !
            ! Nodes
            !
            call reallocP(meshgeom%nodex, meshgeom%numnode, keepExisting=.false., fill=-999d0)
            call reallocP(meshgeom%nodey, meshgeom%numnode, keepExisting=.false., fill=-999d0)
            call reallocP(meshgeom%nodez, meshgeom%numnode, keepExisting=.false., fill=-999d0)

            !
            ! read mesh edge nodes only if numedge is present
            !
            if (meshgeom%numedge /= -1) then
               call reallocP(meshgeom%edge_nodes, (/2, meshgeom%numedge/), keepExisting=.false.)
               ! Edge nodes
               ierr = ug_get_edge_nodes(ncid, meshids, meshgeom%edge_nodes, meshgeom%start_index); write (*, '(a,i0)') 'ug_get_meshgeom, #12, ierr=', ierr

            end if

            ! Get the node coordinates
            if (meshgeom%dim == 2 .or. .not. present(netid)) then
               ierr = ug_get_node_coordinates(ncid, meshids, meshgeom%nodex, meshgeom%nodey, meshgeom%nodez)
               ! TODO: AvD: include zk coordinates
            end if
            if (meshgeom%dim == 2) then
               if (meshgeom%numface /= -1) then
                  call reallocP(meshgeom%face_nodes, (/meshgeom%maxnumfacenodes, meshgeom%numface/), keepExisting=.false.)
                  call reallocP(meshgeom%facex, meshgeom%numface, keepExisting=.false.)
                  call reallocP(meshgeom%facey, meshgeom%numface, keepExisting=.false.)

                  ierr = ug_get_face_coordinates(ncid, meshids, meshgeom%facex, meshgeom%facey)
                  ierr = ug_get_face_nodes(ncid, meshids, meshgeom%face_nodes, startIndex=meshgeom%start_index)
               end if
               if (meshgeom%numedge /= -1) then
                  call reallocP(meshgeom%edge_faces, (/2, meshgeom%numedge/), keepExisting=.false.)
                  ierr = ug_get_edge_faces(ncid, meshids, meshgeom%edge_faces, startIndex=meshgeom%start_index)
               end if
            end if

            if (meshgeom%dim == 1 .and. present(netid)) then
               !Mesh variables
               call reallocP(meshgeom%nodebranchidx, meshgeom%numnode, keepExisting=.false., fill=-999)
               call reallocP(meshgeom%nodeoffsets, meshgeom%numnode, keepExisting=.false., fill=-999d0)
               ierr = ug_get_1d_mesh_discretisation_points(ncid, meshids, meshgeom%nodebranchidx, meshgeom%nodeoffsets, meshgeom%start_index); write (*, '(a,i0)') 'ug_get_meshgeom, #13, ierr=', ierr
               !Here i can not use gridgeom to get xy-coordinates of the mesh1d (gridgeom depends on io_netcdf)

               call reallocP(meshgeom%edgebranchidx, meshgeom%numedge, keepExisting=.false., fill=-999)
               call reallocP(meshgeom%edgeoffsets, meshgeom%numedge, keepExisting=.false., fill=-999d0)
               ierr = ug_get_1d_mesh_edge_coordinates(ncid, meshids, meshgeom%edgebranchidx, meshgeom%edgeoffsets, meshgeom%start_index); write (*, '(a,i0)') 'ug_get_meshgeom, #14, ierr=', ierr

               if (present(network1dname)) then
                  ierr = ug_get_network_name_from_mesh1d(ncid, meshids, network1dname)
               end if
               if (present(nodeids) .and. present(nodelongnames)) then
                  if (allocated(nodeids)) deallocate (nodeids)
                  if (allocated(nodelongnames)) deallocate (nodelongnames)
                  allocate (nodeids(meshgeom%numnode))
                  allocate (nodelongnames(meshgeom%numnode))
                  ierr = nf90_get_var(ncid, meshids%varids(mid_node_ids), nodeids)
                  ierr = nf90_get_var(ncid, meshids%varids(mid_node_longnames), nodelongnames)
               end if
            end if

            ! TODO: AvD: introduce ug_read_mesh_arrays( .. intent out arrays ..)
         end if
      end if
   end function ug_get_meshgeom

!> Gets the x,y-coordinates for all nodes in the specified mesh.
!! The output x,y,z arrays are supposed to be of exact correct length already.
   function ug_get_node_coordinates(ncid, meshids, xn, yn, zn) result(ierr)
      integer, intent(in) :: ncid         !< NetCDF dataset id, should be already open.
      type(t_ug_mesh), intent(in) :: meshids      !< Set of NetCDF-ids for all mesh geometry arrays.
      real(kind=dp), intent(out) :: xn(:), yn(:) !< Arrays to store x,y-coordinates of the mesh nodes.
      real(kind=dp), intent(out), optional :: zn(:)        !< Array to store z-coordinates of the mesh nodes.
      !  (AM: Optional to make sure other using code needs no update)
      integer :: ierr         !< Result status (UG_NOERR==NF90_NOERR if successful).

      character(len=NF90_MAX_NAME) :: nodecrdname
      integer :: k, vid

      ierr = nf90_get_var(ncid, meshids%varids(mid_nodex), xn)
      if (ierr /= UG_NOERR) then
         call SetMessage(LEVEL_WARN, 'Could not read mesh node x-coordinates. Check any previous warnings.') ! low level lib may not throw fatal errors
      end if
      ierr = nf90_get_var(ncid, meshids%varids(mid_nodey), yn)
      if (ierr /= UG_NOERR) then
         call SetMessage(LEVEL_WARN, 'Could not read mesh node y-coordinates. Check any previous warnings.') ! low level lib may not throw fatal errors
      end if

      !
      ! AM: Terrible hack, but not all waqgeom files refer to the node_z element
      !
      if (present(zn)) then
         vid = meshids%varids(mid_nodez)
         if (vid == -1) then
            ierr = nf90_inquire_variable(ncid, meshids%varids(mid_nodex), name=nodecrdname)
            k = len_trim(nodecrdname)
            nodecrdname(k:k) = 'z'
            ierr = nf90_inq_varid(ncid, nodecrdname, vid)
         end if

         ierr = nf90_get_var(ncid, vid, zn)
         if (ierr /= UG_NOERR) then
            call SetMessage(LEVEL_WARN, 'Could not read mesh node z-coordinates. Check any previous warnings.') ! low level lib may not throw fatal errors
         end if
      end if

      ! TODO: AvD: some more careful error handling
   end function ug_get_node_coordinates

!> Puts the x,y-coordinates for all nodes in the specified mesh.
!! The input x,y arrays are supposed to be of exact correct length already.
   function ug_put_node_coordinates(ncid, meshids, xn, yn) result(ierr)
      integer, intent(in) :: ncid    !< NetCDF dataset id, should be already open.
      type(t_ug_mesh), intent(in) :: meshids !< Set of NetCDF-ids for all mesh geometry arrays.
      real(kind=dp), intent(in) :: xn(:), yn(:) !< Arrays to store x,y-coordinates of the mesh nodes.
      integer :: ierr     !< Result status (UG_NOERR==NF90_NOERR if successful).

      ierr = nf90_put_var(ncid, meshids%varids(mid_nodex), xn)
      if (ierr /= NF90_NOERR) then
         call SetMessage(LEVEL_WARN, 'Could not put mesh node x-coordinates. Check any previous warnings.') ! low level lib may not throw fatal errors
      end if
      ierr = nf90_put_var(ncid, meshids%varids(mid_nodey), yn)
      ! TODO: AvD: some more careful error handling

   end function ug_put_node_coordinates

!> Gets the edge-face connectivity table for all edges in the specified mesh.
!! The output edge_faces array is supposed to be of exact correct size already.
   function ug_get_edge_faces(ncid, meshids, edge_faces, ifill, startIndex) result(ierr)
      use array_module
      integer, intent(in) :: ncid              !< NetCDF dataset id, should be already open.
      type(t_ug_mesh), intent(in) :: meshids           !< Set of NetCDF-ids for all mesh geometry arrays.
      integer, intent(out) :: edge_faces(:, :)   !< Array to the edge-node connectivity table.
      integer, optional, intent(out) :: ifill             !< (Optional) Integer fill value.
      integer :: ierr              !< Result status (UG_NOERR==NF90_NOERRif successful).
      integer, optional, intent(in) :: startIndex        !< The start index the caller asks for
      integer :: varStartIndex  !< Temp variables

      ierr = nf90_get_var(ncid, meshids%varids(mid_edgefaces), edge_faces)
      if (ierr /= nf90_noerr) then
         goto 999
      end if

      if (present(ifill)) then
         ierr = nf90_get_att(ncid, meshids%varids(mid_edgefaces), '_FillValue', ifill)
      end if

      if (present(startIndex)) then
         !we check for the start_index, we do not know if the variable was written as 0 based
         ierr = nf90_get_att(ncid, meshids%varids(mid_edgefaces), 'start_index', varStartIndex)
         if (ierr == UG_NOERR) then
            ierr = convert_start_index(edge_faces(1, :), imiss, varStartIndex, startIndex)
            ierr = convert_start_index(edge_faces(2, :), imiss, varStartIndex, startIndex)
         else
            ierr = convert_start_index(edge_faces(1, :), imiss, 0, startIndex)
            ierr = convert_start_index(edge_faces(2, :), imiss, 0, startIndex)
         end if
      end if

      ierr = UG_NOERR
      return ! Return with success

999   continue
      ! Some error (status was set earlier)

   end function ug_get_edge_faces

!> Gets the edge-node connectivity table for all edges in the specified mesh.
!! The output edge_nodes array is supposed to be of exact correct size already.
   function ug_get_edge_nodes(ncid, meshids, edge_nodes, startIndex) result(ierr)
      use array_module
      use netcdf_utils, only: ncu_inq_var_fill

      integer, intent(in) :: ncid             !< NetCDF dataset id, should be already open.
      type(t_ug_mesh), intent(in) :: meshids          !< Set of NetCDF-ids for all mesh geometry arrays.
      integer, intent(out) :: edge_nodes(:, :)  !< Array to the edge-node connectivity table.
      integer :: ierr             !< Result status (UG_NOERR==NF90_NOERRif successful).
      integer, optional, intent(in) :: startIndex      !< The requested index
      integer :: varStartIndex    !< The index stored in the netCDF file

      integer :: ifill, no_fill

      ierr = nf90_get_var(ncid, meshids%varids(mid_edgenodes), edge_nodes)
      if (ierr /= nf90_noerr) then
         goto 999
      end if

      ierr = ncu_inq_var_fill(ncid, meshids%varids(mid_edgenodes), no_fill, ifill)

      if (present(startIndex)) then
         !we check for the start_index, we do not know if the variable was written as 0 based
         ierr = nf90_get_att(ncid, meshids%varids(mid_edgenodes), 'start_index', varStartIndex)
         if (ierr == UG_NOERR) then
            ierr = convert_start_index(edge_nodes(1, :), ifill, varStartIndex, startIndex)
            ierr = convert_start_index(edge_nodes(2, :), ifill, varStartIndex, startIndex)
         else
            ierr = convert_start_index(edge_nodes(1, :), ifill, 0, startIndex)
            ierr = convert_start_index(edge_nodes(2, :), ifill, 0, startIndex)
         end if
      end if

      ierr = UG_NOERR
      return ! Return with success

999   continue
      ! Some error (status was set earlier)

   end function ug_get_edge_nodes

!> Gets the x,y-coordinates (representative centre) for all faces in the specified mesh.
!! The output x,y arrays are supposed to be of exact correct length already.
   function ug_get_face_coordinates(ncid, meshids, xf, yf) result(ierr)
      integer, intent(in) :: ncid    !< NetCDF dataset id, should be already open.
      type(t_ug_mesh), intent(in) :: meshids !< Set of NetCDF-ids for all mesh geometry arrays.
      real(kind=dp), intent(out) :: xf(:), yf(:) !< Arrays to store x,y-coordinates of the mesh face centres.
      integer :: ierr     !< Result status (UG_NOERR==NF90_NOERR if successful).

      ierr = nf90_get_var(ncid, meshids%varids(mid_facex), xf)
      if (ierr /= UG_NOERR) then
         call SetMessage(LEVEL_WARN, 'Could not read mesh face x-coordinates. Check any previous warnings.') ! low level lib may not throw fatal errors
      end if
      ierr = nf90_get_var(ncid, meshids%varids(mid_facey), yf)
      ! TODO: AvD: some more careful error handling

   end function ug_get_face_coordinates

!> Puts the x,y-coordinates (representative centre) for all faces in the specified mesh.
!! The input x,y arrays are supposed to be of exact correct length already.
   function ug_put_face_coordinates(ncid, meshids, xf, yf) result(ierr)
      integer, intent(in) :: ncid    !< NetCDF dataset id, should be already open.
      type(t_ug_mesh), intent(in) :: meshids !< Set of NetCDF-ids for all mesh geometry arrays.
      real(kind=dp), intent(in) :: xf(:), yf(:) !< Arrays containing the x,y-coordinates of the mesh face centres.
      integer :: ierr     !< Result status (UG_NOERR==NF90_NOERR if successful).

      ierr = nf90_put_var(ncid, meshids%varids(mid_facex), xf)
      if (ierr /= NF90_NOERR) then
         call SetMessage(LEVEL_WARN, 'Could not put mesh face x-coordinates. Check any previous warnings.') ! low level lib may not throw fatal errors
      end if
      ierr = nf90_put_var(ncid, meshids%varids(mid_facey), yf)
      ! TODO: AvD: some more careful error handling

   end function ug_put_face_coordinates

!> Gets the face-edge connectivity table for all faces in the specified mesh.
!! The output face_edges array is supposed to be of exact correct size already.
   function ug_get_face_edges(ncid, meshids, face_edges, ifill, startIndex) result(ierr)
      use array_module
      use netcdf_utils, only: ncu_inq_var_fill

      integer, intent(in) :: ncid            !< NetCDF dataset id, should be already open.
      type(t_ug_mesh), intent(in) :: meshids         !< Set of NetCDF-ids for all mesh geometry arrays.
      integer, intent(out) :: face_edges(:, :) !< Array to the face-node connectivity table.
      integer, optional, intent(out) :: ifill           !< (Optional) Integer fill value.
      integer, optional, intent(in) :: startIndex      !< The start index the caller asks for
      integer :: k, varStartIndex   !< Temp variables
      integer :: ierr            !< Result status (UG_NOERR==NF90_NOERRif successful).

      integer :: ifill_, no_fill

      ierr = nf90_get_var(ncid, meshids%varids(mid_faceedges), face_edges)
      if (ierr /= nf90_noerr) then
         goto 999
      end if

      ierr = ncu_inq_var_fill(ncid, meshids%varids(mid_faceedges), no_fill, ifill_)
      if (present(ifill)) then
         ifill = ifill_
      end if

      if (present(startIndex)) then
         !we check for the start_index, we do not know if the variable was written as 0 based
         ierr = nf90_get_att(ncid, meshids%varids(mid_faceedges), 'start_index', varStartIndex)
         if (ierr == UG_NOERR) then
            do k = 1, size(face_edges, 1)
               ierr = convert_start_index(face_edges(k, :), ifill_, varStartIndex, startIndex)
            end do
         else
            do k = 1, size(face_edges, 1)
               ierr = convert_start_index(face_edges(k, :), ifill_, 0, startIndex)
            end do
         end if
      end if

      ierr = UG_NOERR
      return ! Return with success

999   continue
      ! Some error (status was set earlier)

   end function ug_get_face_edges

!> Gets the face-node connectivity table for all faces in the specified mesh.
!! The output face_nodes array is supposed to be of exact correct size already.
   function ug_get_face_nodes(ncid, meshids, face_nodes, ifill, startIndex) result(ierr)
      use array_module
      use netcdf_utils, only: ncu_inq_var_fill

      integer, intent(in) :: ncid            !< NetCDF dataset id, should be already open.
      type(t_ug_mesh), intent(in) :: meshids         !< Set of NetCDF-ids for all mesh geometry arrays.
      integer, intent(out) :: face_nodes(:, :) !< Array to the face-node connectivity table.
      integer, optional, intent(out) :: ifill           !< (Optional) Integer fill value.
      integer, optional, intent(in) :: startIndex      !< The start index the caller asks for
      integer :: k, varStartIndex   !< Temp variables
      integer :: ierr            !< Result status (UG_NOERR==NF90_NOERRif successful).

      integer :: ifill_, no_fill

      ierr = nf90_get_var(ncid, meshids%varids(mid_facenodes), face_nodes)
      if (ierr /= nf90_noerr) then
         goto 999
      end if

      ierr = ncu_inq_var_fill(ncid, meshids%varids(mid_facenodes), no_fill, ifill_)
      if (present(ifill)) then
         ifill = ifill_
      end if

      if (present(startIndex)) then
         !we check for the start_index, we do not know if the variable was written as 0 based
         ierr = nf90_get_att(ncid, meshids%varids(mid_facenodes), 'start_index', varStartIndex)
         if (ierr == UG_NOERR) then
            do k = 1, size(face_nodes, 1)
               ierr = convert_start_index(face_nodes(k, :), ifill_, varStartIndex, startIndex)
            end do
         else
            do k = 1, size(face_nodes, 1)
               ierr = convert_start_index(face_nodes(k, :), ifill_, 0, startIndex)
            end do
         end if
      end if

      ierr = UG_NOERR
      return ! Return with success

999   continue
      ! Some error (status was set earlier)

   end function ug_get_face_nodes

!> Returns the number of variables that are available in the specified dataset on the specified mesh.
!! The location type allows to select on specific topological mesh locations
!! (UGRID-compliant, so UG_LOC_FACE/EDGE/NODE/ALL2D).
   function ug_get_var_count(ncid, meshids, iloctype, nvar) result(ierr)
      use string_module
      integer, intent(in) :: ncid     !< NetCDF dataset id, should be already open.
      type(t_ug_mesh), intent(in) :: meshids  !< Set of NetCDF-ids for all mesh geometry arrays.
      integer, intent(in) :: iloctype !< The topological location on which to select data (one of UG_LOC_FACE/EDGE/NODE/ALL2D).
      integer, intent(out) :: nvar     !< Number of variables defined on the requested location type+mesh+dataset.
      integer :: ierr     !< Result status, ug_noerr if successful.

      integer :: numVar, ivarloc
      character(len=nf90_max_name) :: str, meshname
      integer :: varid         !< NetCDF variable ID (1-based).
      str = ''
      meshname = ''
      ierr = nf90_inquire_variable(ncid, meshids%varids(mid_meshtopo), name=meshname)
      if (ierr /= nf90_noerr) then
         ierr = UG_INVALID_MESHNAME
         goto 999
      end if

      ! Now check all variables and if they're data variables on the right mesh+location.
      ierr = nf90_inquire(ncid, nVariables=numVar)

      nvar = 0
      do varid = 1, numVar
         ! Step 1 of 2: check mesh name
         str = ''
         ierr = nf90_get_att(ncid, varid, 'mesh', str)
         if (ierr /= nf90_noerr) then
            ! No UGRID :mesh attribute, ignore this var.
            cycle
         end if

         if (.not. strcmpi(str, meshname)) then
            ! Mesh names do not match
            cycle
         end if

         ! Step 2 of 2: check location name
         str = ''
         ierr = nf90_get_att(ncid, varid, 'location', str)
         if (ierr /= nf90_noerr) then
            ! No UGRID :location attribute, ignore this var.
            cycle
         end if
         call ug_location_to_loctype(str, ivarloc)
         if (ug_checklocation(iloctype, ivarloc)) then
            ! This variable is ok. Mesh matched, and now the location also matched.
            nvar = nvar + 1
         end if
      end do

      ierr = UG_NOERR
      return ! Return with success

999   continue
      ! Some error (status was set earlier)

   end function ug_get_var_count

!> Gets a list of variable IDs that are available in the specified dataset on the specified mesh.
!! The location type allows to select on specific topological mesh locations
!! (UGRID-compliant, so UG_LOC_FACE/EDGE/NODE/ALL2D)
   function ug_inq_varids(ncid, meshids, iloctype, varids, nvar) result(ierr)
      use netcdf_utils, only: ncu_get_att
      use string_module

      integer, intent(in) :: ncid      !< NetCDF dataset id, should be already open.
      type(t_ug_mesh), intent(in) :: meshids   !< Set of NetCDF-ids for all mesh geometry arrays.
      integer, intent(in) :: iloctype  !< The topological location on which to select data variables (one of UG_LOC_FACE/EDGE/NODE/ALL2D).
      integer, intent(out) :: varids(:) !< Array to store the variable ids in.
      integer, intent(out) :: nvar      !< Number of variables found/stored in array.
      integer :: ierr      !< Result status, ug_noerr if successful.

      integer :: numVar, ivarloc, maxvar
      character(len=nf90_max_name) :: meshname
      character(len=:), allocatable :: str
      integer :: varid         !< NetCDF variable ID (1-based).
      allocate (character(len=0) :: str)

      meshname = ''

      ierr = nf90_inquire_variable(ncid, meshids%varids(mid_meshtopo), name=meshname)
      if (ierr /= nf90_noerr) then
         ierr = UG_INVALID_MESHNAME
         goto 999
      end if

      ! Now check all variables and if they're data variables on the right mesh+location.
      ierr = nf90_inquire(ncid, nVariables=numVar)

      maxvar = size(varids)
      nvar = 0
      do varid = 1, numVar
         ! Step 1 of 2: check mesh name
         str = ''
         ierr = ncu_get_att(ncid, varid, 'mesh', str)

         if (ierr /= nf90_noerr) then
            ! No UGRID :mesh attribute, ignore this var.
            cycle
         end if

         if (.not. strcmpi(str, meshname)) then
            ! Mesh names do not match
            cycle
         end if

         ! Step 2 of 2: check location name
         str = ''
         ierr = ncu_get_att(ncid, varid, 'location', str)
         if (ierr /= nf90_noerr) then
            ! No UGRID :location attribute, ignore this var.
            cycle
         end if
         call ug_location_to_loctype(str, ivarloc)
         if (ug_checklocation(iloctype, ivarloc)) then
            ! This variable is ok. Mesh matched, and now the location also matched.
            if (nvar >= maxvar) then
               ierr = UG_ARRAY_TOOSMALL
               goto 999
            end if

            nvar = nvar + 1
            varids(nvar) = varid
         end if
      end do

      deallocate (str)
      ierr = UG_NOERR
      return ! Return with success

999   continue
      ! Some error (status was set earlier)

   end function ug_inq_varids

!> Gets the variable ID for a data variable that is defined in the specified dataset on the specified mesh.
!! The variable is searched based on variable name (without any "meshnd_" prefix), and which :mesh it is defined on.
   function ug_inq_varid(ncid, meshids, varname, varid) result(ierr)
      use netcdf_utils, only: ncu_get_att
      use string_module

      integer, intent(in) :: ncid     !< NetCDF dataset id, should be already open.
      type(t_ug_mesh), intent(in) :: meshids  !< Set of NetCDF-ids for all mesh geometry arrays.
      character(len=*), intent(in) :: varname  !< The name of the variable to be found. Should be without any "meshnd_" prefix.
      integer, intent(out) :: varid    !< The resulting variable id, if found (1-based).
      integer :: ierr     !< Result status, ug_noerr if successful.

      character(len=nf90_max_name) :: meshname
      character(len=:), allocatable :: str
      integer :: loc_varid     !< NetCDF variable ID (1-based).

      allocate (character(len=0) :: str)
      meshname = ''

      ierr = nf90_inquire_variable(ncid, meshids%varids(mid_meshtopo), name=meshname)
      if (ierr /= nf90_noerr) then
         ierr = UG_INVALID_MESHNAME
         goto 999
      end if

      ! Now check variable with user-specified name and if it's a data variable on the right mesh.
      ierr = nf90_inq_varid(ncid, trim(meshname)//'_'//trim(varname), loc_varid)
      if (ierr /= nf90_noerr) then
         ! Do a second try with the varname without the meshname prefix.
         ierr = nf90_inq_varid(ncid, trim(varname), loc_varid)
      end if
      if (ierr /= nf90_noerr) then
         ug_messagestr = 'ug_inc_varid: no candidate variable could be found for name'''//trim(varname)//'''.'
         ierr = UG_ENOTVAR
         goto 999
      end if

      str = ''
      ierr = ncu_get_att(ncid, loc_varid, 'mesh', str)
      if (ierr /= nf90_noerr) then
         ! No UGRID :mesh attribute, discard this var.
         !ug_messagestr = 'ug_inc_varid: candidate variable for name '''//trim(varname)//''' has no :mesh attribute.'
         !ierr = UG_INVALID_MESHNAME
         !goto 999
         !
         ! NOTE: not all variables have a :mesh attribute anymore (e.g., <prefix>_node_id),
         ! so don't fail on this check here. Continue with given meshname.
         !
         str = trim(meshname)
      end if

      if (.not. strcmpi(str, meshname)) then
         ! Mesh names do not match
         ug_messagestr = 'ug_inc_varid: candidate variable for name '''//trim(varname)//''' on mesh '''//trim(meshname)//''' has different :mesh attribute '''//trim(str)//'''.'
         ierr = UG_INVALID_MESHNAME
         goto 999
      end if

      varid = loc_varid
      ierr = UG_NOERR
      deallocate (str)
      return ! Return with success

999   continue
      ! Some error (status was set earlier)
      deallocate (str)
   end function ug_inq_varid

!> Gets the variable ID for the variable in the specified dataset on the specified mesh,
!! that also has the specified value for its ':standard_name' attribute, and
!! is defined on the specified topological mesh location (UGRID-compliant, so UG_LOC_FACE/EDGE/NODE/ALL2D)
   function ug_inq_varid_by_standard_name(ncid, meshids, iloctype, stdname, varid) result(ierr)
      use netcdf_utils, only: ncu_get_att
      use string_module

      integer, intent(in) :: ncid     !< NetCDF dataset id, should be already open.
      type(t_ug_mesh), intent(in) :: meshids  !< Set of NetCDF-ids for all mesh geometry arrays.
      integer, intent(in) :: iloctype !< The topological location on which to check for the data variable (one of UG_LOC_FACE/EDGE/NODE/ALL2D).
      character(len=*), intent(in) :: stdname  !< The standard_name value that is searched for.
      integer, intent(out) :: varid    !< The netCDF variable id (when found) (1-based).
      integer :: ierr     !< Result status, ug_noerr if successful.

      integer, allocatable :: varids(:) !< Array to store the candidate variable ids in.
      integer :: numVar, maxvar
      character(len=:), allocatable :: str
      integer :: nlen
      integer :: iv            !< Loop index cycling over the number of variables (1 to numVar).

      allocate (character(len=0) :: str)
      ! Get all candidate data variables on the right mesh+location
      ierr = nf90_inquire(ncid, nVariables=maxvar)
      allocate (varids(maxvar))
      ierr = ug_inq_varids(ncid, meshids, iloctype, varids, numVar)
      if (ierr /= ug_noerr) then
         goto 999
      end if

      nlen = len_trim(stdname)

      varid = -1 ! dummy
      do iv = 1, numVar

         str = ''
         ierr = ncu_get_att(ncid, varids(iv), 'standard_name', str)
         if (ierr /= nf90_noerr) then
            ! No CF :standard_name attribute, ignore this var.
            cycle
         end if

         if (strcmpi(str, stdname(1:nlen))) then
            if (varid /= -1) then
               ! More than one variable matches all criteria, whereas only a single one was intended/requested.
               ierr = UG_VAR_TOOMANYFOUND
               goto 999
            else
               ! standard_name matches
               varid = varids(iv)
            end if
         end if
      end do

      if (varid == -1) then
         ierr = UG_ENOTVAR
         goto 999
      end if

      ierr = UG_NOERR
      goto 888 ! Return with success

999   continue
      ! Some error (status was set earlier)

888   continue
      deallocate (varids)
      deallocate (str)

   end function ug_inq_varid_by_standard_name

!> Writes the given edge type variable to the given netcdf file.
   subroutine write_edge_type_variable(igeomfile, meshids, meshName, edge_type)
      implicit none

      integer, intent(in) :: igeomfile    !< file pointer to netcdf file to write to.
      type(t_ug_mesh), intent(inout) :: meshids!< Set of NetCDF-ids for all mesh geometry variables.
      character(len=*), intent(in) :: meshName     !< Name of the mesh.
      integer, intent(in) :: edge_type(:) !< Edge type variable to be written to the NetCDF file.

      integer :: id_edgetype !< Variable ID for edge type variable.
      integer :: was_in_define_mode
      integer :: ierr !< Result status (UG_NOERR==NF90_NOERR if successful).

      ierr = UG_NOERR

      ! Put netcdf file in define mode.
      was_in_define_mode = 0
      ierr = nf90_redef(igeomfile)
      if (ierr == nf90_eindefine) then
         was_in_define_mode = 1 ! If was still in define mode.
      end if
      ierr = UG_NOERR

      ! Define edge type variable.
      ierr = ug_def_var(igeomfile, id_edgetype, (/meshids%dimids(mdim_edge)/), nf90_int, UG_LOC_EDGE, &
                        meshName, 'edge_type', '', 'edge type (relation between edge and flow geometry)', '', '', '', ifill=-999)
      ierr = nf90_put_att(igeomfile, id_edgetype, 'flag_values', (/UG_EDGETYPE_INTERNAL_CLOSED, UG_EDGETYPE_INTERNAL, UG_EDGETYPE_BND, UG_EDGETYPE_BND_CLOSED/))
      ierr = nf90_put_att(igeomfile, id_edgetype, 'flag_meanings', 'internal_closed internal boundary boundary_closed')

      ! Put netcdf file in write mode.
      ierr = nf90_enddef(igeomfile)

      ! Write edge type variable.
      ierr = nf90_put_var(igeomfile, id_edgetype, edge_type)

      ! Leave the dataset in the same mode as we got it.
      if (was_in_define_mode == 1) then
         ierr = nf90_redef(igeomfile)
      end if

   end subroutine write_edge_type_variable

! TODO: LC: all routines below are for testing only. Probably better to move thenm to a different file.

!> Creates and initializes mesh geometry that contains the 2D unstructured network and edge type array.
!! NOTE: this routine is currently only a TEST GEOMETRY CREATOR
!!
!! NOTE: do not pass already filled mesh geometries to this function,
!! since array pointers will become disassociated, possibly causing memory leaks.
   function ug_create_ugrid_geometry(meshgeom, crs) result(ierr)
      type(t_ug_meshgeom), intent(out) :: meshgeom     !< The mesh geometry that is to be created and filled.
      type(t_crs), intent(inout) :: crs
      ! TODO why need save here?
      integer, allocatable, target, save :: edge_nodes(:, :), edge_faces(:, :), face_nodes(:, :), face_edges(:, :), face_links(:, :) !< Output arrays.

      type(t_face), pointer :: netcell(:)
      real(kind=dp), allocatable, target, save :: edgex(:), edgey(:) !< Output coordinate arrays.
      integer :: edge, face, maxNodesPerFace, nodesPerFace, nump !< Counters.
      integer, parameter :: missing_value = -999
      double precision, parameter :: dmiss = -999.0
      integer :: ierr !< Result status (UG_NOERR==NF90_NOERR if successful).

      ierr = UG_NOERR

      ! Create 2D mesh geometry that contains all 2D faces, edges and nodes.
      ierr = t_ug_meshgeom_destructor(meshgeom)

      meshgeom%meshName = 'mesh2d'
      meshgeom%dim = 2

      meshgeom%epsg = 4326

      crs%varname = 'wgs84'
      crs%epsg_code = 4326

      ! Nodes.
      meshgeom%numNode = 5

      ! Get node coordinates.
      allocate (meshgeom%nodex(5))
      meshgeom%nodex(1) = 0.
      meshgeom%nodex(2) = 10.
      meshgeom%nodex(3) = 15.
      meshgeom%nodex(4) = 10.
      meshgeom%nodex(5) = 5.

      allocate (meshgeom%nodey(5))
      meshgeom%nodey(1) = 0.
      meshgeom%nodey(2) = 0.
      meshgeom%nodey(3) = 5.
      meshgeom%nodey(4) = 10.
      meshgeom%nodey(5) = 5.

      ! Edges.
      ! Use only 2D net links (= edges).
      meshgeom%numEdge = 6

      ! Get edge nodes connectivity, edge types and edge coordinates (ordered as follows: first flow links, then closed edges).
      allocate (edge_nodes(2, meshgeom%numEdge))
      edge_nodes = missing_value

      allocate (edgex(meshgeom%numEdge))
      allocate (edgey(meshgeom%numEdge))
      edgex = dmiss
      edgey = dmiss

      edge_nodes(1, 1) = 5
      edge_nodes(2, 1) = 2
      edge_nodes(1, 2) = 2
      edge_nodes(2, 2) = 1
      edge_nodes(1, 3) = 1
      edge_nodes(2, 3) = 5
      edge_nodes(1, 4) = 5
      edge_nodes(2, 4) = 4
      edge_nodes(1, 5) = 4
      edge_nodes(2, 5) = 3
      edge_nodes(1, 6) = 3
      edge_nodes(2, 6) = 2

      meshgeom%edge_nodes => edge_nodes

      meshgeom%edgex => edgex
      meshgeom%edgey => edgey
      ! Edge z coordinates are unknown.

      ! Get edge faces connectivity.
      allocate (edge_faces(2, meshgeom%numEdge))
      ! Here need to use reverse_edge_mapping_table to map edges to net links, because edges are ordered as follows: first flow links, then closed edges.
      edge_faces(1:2, 1) = (/1, 2/)
      edge_faces(1:2, 2) = (/1, 0/)
      edge_faces(1:2, 3) = (/1, 0/)
      edge_faces(1:2, 4) = (/2, 0/)
      edge_faces(1:2, 5) = (/2, 0/)
      edge_faces(1:2, 6) = (/2, 0/)

      do edge = 1, meshgeom%numEdge
         ! 0 means no face, i.e. edge is on the boundary of the mesh.
         ! Replace zeroes with missing values.
         if (edge_faces(1, edge) == 0) edge_faces(1, edge) = missing_value
         if (edge_faces(2, edge) == 0) edge_faces(2, edge) = missing_value
      end do
      meshgeom%edge_faces => edge_faces

      ! Faces.
      ! Use only 2D internal net cells = 2D internal flow nodes (= faces).
      nump = 2
      meshgeom%numFace = nump

      ! Get face coordinates.
      allocate (meshgeom%facex(2))
      meshgeom%facex(1) = 5
      meshgeom%facex(2) = 10

      allocate (meshgeom%facey(2))
      meshgeom%facey(1) = 2.5
      meshgeom%facey(2) = 5

      ! Determine max nr of net nodes per 2D net cell = face.
      maxNodesPerFace = 4
      meshgeom%maxNumFaceNodes = maxNodesPerFace

      ! Get face nodes connectivity, face edges connectivity and face-face connectivity.
      allocate (face_nodes(maxNodesPerFace, meshgeom%numFace))
      face_nodes = missing_value

      allocate (face_edges(maxNodesPerFace, meshgeom%numFace))
      face_edges = missing_value

      allocate (face_links(maxNodesPerFace, meshgeom%numFace))
      face_links = missing_value

      allocate (netcell(2))
      netcell(1)%n = 3
      netcell(2)%n = 4

      allocate (netcell(1)%nod(3))
      netcell(1)%nod(1) = 1
      netcell(1)%nod(2) = 2
      netcell(1)%nod(3) = 5

      allocate (netcell(2)%nod(4))
      netcell(2)%nod(1) = 2
      netcell(2)%nod(2) = 3
      netcell(2)%nod(3) = 4
      netcell(2)%nod(4) = 5

      do face = 1, nump
         nodesPerFace = netcell(face)%n
         face_nodes(1:nodesPerFace, face) = netcell(face)%nod
      end do

      meshgeom%face_nodes => face_nodes
      meshgeom%face_edges => face_edges
      meshgeom%face_links => face_links

   end function ug_create_ugrid_geometry

   subroutine ug_create_ugrid_meta(meta)
      type(t_ug_meta) :: meta  !< Meta information on file.

      meta%institution = "Deltares"
      meta%source = "DeltaShell"
      meta%references = "geen idee"
      meta%version = "0"
      meta%modelname = "manual"

   end subroutine ug_create_ugrid_meta

!> Writes the unstructured network and edge type to an already opened netCDF dataset.
   function ug_write_geom_filepointer_ugrid(ncid, meshgeom, crs, meshids, networkids) result(ierr)
      integer, intent(in) :: ncid !< file pointer to netcdf file to write to.
      type(t_ug_meshgeom), intent(in) :: meshgeom !< Mesh geometry to be written to the NetCDF file.
      type(t_crs), intent(in) :: crs !< Mesh geometry to be written to the NetCDF file.
      type(t_ug_mesh), intent(inout) :: meshids  !< Set of NetCDF-ids for all mesh geometry variables.
      type(t_ug_network), intent(inout) :: networkids
      integer :: ierr     !< Result status (UG_NOERR==NF90_NOERR if successful).
      type(t_ug_meta) :: meta  !< Meta information on file. ! TODO: later also input arg

      ierr = UG_NOERR

      ! create default meta
      call ug_create_ugrid_meta(meta)

      ! Add global attributes to NetCDF file.
      ierr = ug_addglobalatts(ncid, meta)

      ! Write mesh geometry.
      ierr = ug_write_mesh_struct(ncid, meshids, networkids, crs, meshgeom)

   end function ug_write_geom_filepointer_ugrid

!> Writes the unstructured network and edge type to a netCDF file.
!! If file exists, it will be overwritten.
   function ug_write_geom_ugrid(filename) result(ierr)

      character(len=*), intent(in) :: filename
      integer :: ierr

      type(t_ug_meshgeom) :: meshgeom !< Mesh geometry to be written to the NetCDF file.
      type(t_ug_mesh) :: meshids          !< Set of NetCDF-ids for all mesh geometry variables.
      type(t_ug_network) :: networkid        !< Set of NetCDF-ids for all network variables.
      type(t_crs) :: crs              !< Set of NetCDF-ids for all mesh geometry variables.
      integer :: ncid

      ierr = nf90_create(filename, 0, ncid)
      if (ierr /= nf90_noerr) then
         return
      end if

      ! create mesh geometry
      ierr = ug_create_ugrid_geometry(meshgeom, crs)

      ierr = ug_write_geom_filepointer_ugrid(ncid, meshgeom, crs, meshids, networkid)

      ierr = nf90_close(ncid)

   end function ug_write_geom_ugrid

!> Writes the unstructured network and edge type AND time-dep output data to a netCDF file.
!! If file exists, it will be overwritten.
   function ug_write_map_ugrid(filename) result(ierr)

      character(len=*), intent(in) :: filename
      integer :: ierr

      type(t_ug_meshgeom) :: meshgeom    !< Mesh geometry to be written to the NetCDF file.
      type(t_ug_mesh) :: meshids     !< Set of NetCDF-ids for all mesh geometry variables.
      type(t_ug_network) :: networkid   !< Set of NetCDF-ids for all mesh geometry variables.
      type(t_crs) :: crs     !< Set of NetCDF-ids for all mesh geometry variables.
      integer :: id_s1, id_s2, id_u1, id_zk, id_time, itim ! example: water levels, water depth, edge speed, bed level and a timer
      integer :: ncid, id_timedim
      double precision, allocatable :: workf(:), worke(:), workn(:)

      ! NOTE: this routine is currently only a TEST FILE WRITER

      ! TODO: some if, to only do this at first time step
      ierr = nf90_create(filename, 0, ncid)
      if (ierr /= nf90_noerr) then
         return
      end if

      ! create mesh geometry
      ierr = ug_create_ugrid_geometry(meshgeom, crs)

      ierr = ug_write_geom_filepointer_ugrid(ncid, meshgeom, crs, meshids, networkid)

      ierr = nf90_def_dim(ncid, 'time', nf90_unlimited, id_timedim)

      ierr = nf90_def_var(ncid, 'time', nf90_double, id_timedim, id_time)
      ierr = nf90_put_att(ncid, id_time, 'standard_name', 'time')
      ierr = nf90_put_att(ncid, id_time, 'units', 'seconds since 2008-01-09 00:00:00')

      ierr = ug_def_var(ncid, id_s1, (/meshids%dimids(mdim_face), id_timedim/), nf90_double, UG_LOC_FACE, meshgeom%meshname, "s1", "sea_surface_level_above_geoid", "Water level on cell centres", &
                        "m", "average", '', crs, -1, -999d0)

      ierr = ug_def_var(ncid, id_s2, (/meshids%dimids(mdim_face), id_timedim/), nf90_double, UG_LOC_FACE, meshgeom%meshname, "s2", "sea_floor_depth_below_geoid", "Water depth on cell centres", &
                        "m", "average", '', crs, -1, -999d0)

      ierr = ug_def_var(ncid, id_u1, (/meshids%dimids(mdim_edge), id_timedim/), nf90_double, UG_LOC_EDGE, meshgeom%meshname, "u1", "", "Normal velocity on cell edges", &
                        "m s-1", "average", '', crs, -1, -999d0)

      ierr = ug_def_var(ncid, id_zk, (/meshids%dimids(mdim_node), id_timedim/), nf90_double, UG_LOC_NODE, meshgeom%meshname, "zk", "", "Bed level on cell corners", &
                        "m", "point", '', crs, -1, -999d0)
      ! NOTE: zk is rarely time-dependent, but just as an example

      ierr = nf90_enddef(ncid)

      allocate (workf(meshgeom%numface))
      workf = 1.23d0 ! TODO: make this hardcoded spatially varying.
      allocate (worke(meshgeom%numedge))
      worke = 3.45d-2
      allocate (workn(meshgeom%numnode))
      workn = -7.68d0
      do itim = 1, 10

         workf(:) = workf(:) + itim ! Dummy data time-dependent
         ierr = nf90_put_var(ncid, id_time, dble(itim))

         ierr = nf90_put_var(ncid, id_s1, workf, count=(/meshgeom%numface, 1/), start=(/1, itim/))
         ierr = nf90_put_var(ncid, id_s2, workf + 5d0, count=(/meshgeom%numface, 1/), start=(/1, itim/))

         worke(:) = worke(:) + itim*.01d0 ! Dummy data time-dependent
         ierr = nf90_put_var(ncid, id_u1, worke, count=(/meshgeom%numedge, 1/), start=(/1, itim/))

         workn(:) = workn(:) + itim*.1d0 ! Dummy data time-dependent
         ierr = nf90_put_var(ncid, id_zk, workn, count=(/meshgeom%numnode, 1/), start=(/1, itim/))

      end do

      ! ..
      deallocate (workn, worke, workf)
      ierr = nf90_close(ncid)

   end function ug_write_map_ugrid

! UGRID mesh and network1d functions

!> This function creates a 1d network accordingly to the new 1d format.
!> This version of the interface is kept for backward compatibility.
   function ug_create_1d_network(ncid, netids, networkName, nNodes, nBranches, nGeometry) result(ierr)

      integer, intent(in) :: ncid
      character(len=*), intent(in) :: networkName
      integer, intent(in) :: nNodes
      integer, intent(in) :: nBranches
      integer, intent(in) :: nGeometry
      type(t_ug_network), intent(inout) :: netids
      integer :: ierr

      !locals
      type(t_crs) :: crs           !< Coordinate reference system for the x/y-coordinates variables.

      crs%varname = 'Unknown projected'
      crs%epsg_code = 0

      ierr = ug_create_1d_network_v1(ncid, netids, networkName, nNodes, nBranches, nGeometry, crs)
   end function ug_create_1d_network

!> This function creates a 1d network accordingly to the new 1d format.
   function ug_create_1d_network_v1(ncid, netids, networkName, nNodes, nBranches, nGeometry, crs) result(ierr)

      integer, intent(in) :: ncid
      character(len=*), intent(in) :: networkName
      integer, intent(in) :: nNodes
      integer, intent(in) :: nBranches
      integer, intent(in) :: nGeometry
      type(t_ug_network), intent(inout) :: netids
      type(t_crs), intent(in) :: crs           !< Coordinate reference system for the x/y-coordinates variables.
      integer :: ierr

      !locals
      integer :: wasInDefine
      character(len=len_trim(networkName)) :: prefix
      character(len=256) :: buffer

      prefix = trim(networkName)

      ierr = UG_SOMEERR
      wasInDefine = 0
      ierr = nf90_redef(ncid)
      if (ierr == nf90_eindefine) then
         wasInDefine = 1 ! Was still in define mode.
      end if

      !Dimensions
      ierr = nf90_def_dim(ncid, prefix//'_nEdges', nBranches, netids%dimids(ntdim_1dedges))
      ierr = nf90_def_dim(ncid, prefix//'_nNodes', nNodes, netids%dimids(ntdim_1dnodes))
      ierr = nf90_def_dim(ncid, prefix//'_nGeometryNodes', nGeometry, netids%dimids(ntdim_1dgeopoints))
      !These dimensions might already be defined, check first if they are present
      ierr = nf90_inq_dimid(ncid, 'strLengthIds', netids%dimids(ntdim_idstring))
      if (ierr /= UG_NOERR) then
         ierr = nf90_def_dim(ncid, 'strLengthIds', ug_idsLen, netids%dimids(ntdim_idstring))
      end if
      ierr = nf90_inq_dimid(ncid, 'strLengthLongNames', netids%dimids(ntdim_longnamestring))
      if (ierr /= UG_NOERR) then
         ierr = nf90_def_dim(ncid, 'strLengthLongNames', ug_idsLongNamesLen, netids%dimids(ntdim_longnamestring))
      end if

      ! Dimension 2 might already be present
      ierr = nf90_inq_dimid(ncid, 'Two', netids%dimids(ntdim_two))
      if (ierr /= UG_NOERR) then
         ierr = nf90_def_dim(ncid, 'Two', 2, netids%dimids(ntdim_two))
      end if

      !Variable declarations: Network1d
      !1. Network1D
      ierr = nf90_def_var(ncid, prefix, nf90_int, netids%varids(ntid_1dtopo))
      ierr = nf90_put_att(ncid, netids%varids(ntid_1dtopo), 'cf_role', 'mesh_topology')
      write (buffer, '(a,i0,a)') 'Topology data of 1D network'
      ierr = nf90_put_att(ncid, netids%varids(ntid_1dtopo), 'long_name', trim(buffer))
      ierr = nf90_put_att(ncid, netids%varids(ntid_1dtopo), 'edge_dimension', prefix//'_nEdges')
      ierr = nf90_put_att(ncid, netids%varids(ntid_1dtopo), 'edge_geometry', prefix//'_geometry')
      ierr = nf90_put_att(ncid, netids%varids(ntid_1dtopo), 'edge_node_connectivity', prefix//'_edge_nodes')
      ierr = nf90_put_att(ncid, netids%varids(ntid_1dtopo), 'node_coordinates', prefix//'_node_x '//prefix//'_node_y')
      ierr = nf90_put_att(ncid, netids%varids(ntid_1dtopo), 'node_dimension', prefix//'_nNodes')
      ierr = nf90_put_att(ncid, netids%varids(ntid_1dtopo), 'topology_dimension', 1)
      !nodes attributes
      ierr = nf90_put_att(ncid, netids%varids(ntid_1dtopo), 'node_id', prefix//'_node_id')
      ierr = nf90_put_att(ncid, netids%varids(ntid_1dtopo), 'node_long_name', prefix//'_node_long_name')
      !branches attrubutes
      ierr = nf90_put_att(ncid, netids%varids(ntid_1dtopo), 'branch_id', prefix//'_branch_id')
      ierr = nf90_put_att(ncid, netids%varids(ntid_1dtopo), 'branch_long_name', prefix//'_branch_long_name')
      ierr = nf90_put_att(ncid, netids%varids(ntid_1dtopo), 'edge_length', prefix//'_edge_length')

      !2. Branch: the start and the end nodes of each branch
      ierr = nf90_def_var(ncid, prefix//'_edge_nodes', nf90_int, (/netids%dimids(ntdim_two), netids%dimids(ntdim_1dedges)/), netids%varids(ntid_1dedgenodes))
      ierr = nf90_put_att(ncid, netids%varids(ntid_1dedgenodes), 'cf_role', 'edge_node_connectivity')
      ierr = nf90_put_att(ncid, netids%varids(ntid_1dedgenodes), 'long_name', 'Start and end nodes of network edges')
      !2. Branch: the branch ids
      ierr = nf90_def_var(ncid, prefix//'_branch_id', nf90_char, (/netids%dimids(ntdim_idstring), netids%dimids(ntdim_1dedges)/), netids%varids(ntid_1dbranchids))
      ierr = nf90_put_att(ncid, netids%varids(ntid_1dbranchids), 'long_name', 'ID of branch geometries')
      !2. Branch: the long names of the branches
      ierr = nf90_def_var(ncid, prefix//'_branch_long_name', nf90_char, (/netids%dimids(ntdim_longnamestring), netids%dimids(ntdim_1dedges)/), netids%varids(ntid_1dbranchlongnames))
      ierr = nf90_put_att(ncid, netids%varids(ntid_1dbranchlongnames), 'long_name', 'Long name of branch geometries')
      !2. Branch: the branch lengths
      ierr = nf90_def_var(ncid, prefix//'_edge_length', nf90_double, (/netids%dimids(ntdim_1dedges)/), netids%varids(ntid_1dbranchlengths))
      ierr = nf90_put_att(ncid, netids%varids(ntid_1dbranchlengths), 'long_name', 'Real length of branch geometries')
      ierr = nf90_put_att(ncid, netids%varids(ntid_1dbranchlengths), 'units', 'm')

      !3. Nodes: the ids of the nodes
      ierr = nf90_def_var(ncid, prefix//'_node_id', nf90_char, (/netids%dimids(ntdim_idstring), netids%dimids(ntdim_1dnodes)/), netids%varids(ntid_1dnodids))
      ierr = nf90_put_att(ncid, netids%varids(ntid_1dnodids), 'long_name', 'ID of network nodes')
      !3. Nodes: the long names of the nodes
      ierr = nf90_def_var(ncid, prefix//'_node_long_name', nf90_char, (/netids%dimids(ntdim_longnamestring), netids%dimids(ntdim_1dnodes)/), netids%varids(ntid_1dnodlongnames))
      ierr = nf90_put_att(ncid, netids%varids(ntid_1dnodlongnames), 'long_name', 'Long name of network nodes')
      !3. Nodes: x+y coord
      ierr = nf90_def_var(ncid, prefix//'_node_x', nf90_double, (/netids%dimids(ntdim_1dnodes)/), netids%varids(ntid_1dnodex))
      ierr = nf90_def_var(ncid, prefix//'_node_y', nf90_double, (/netids%dimids(ntdim_1dnodes)/), netids%varids(ntid_1dnodey))
      ierr = ug_addcoordatts(ncid, netids%varids(ntid_1dnodex), netids%varids(ntid_1dnodey), crs)
      ierr = nf90_put_att(ncid, netids%varids(ntid_1dnodex), 'long_name', 'x-coordinate of network nodes')
      ierr = nf90_put_att(ncid, netids%varids(ntid_1dnodey), 'long_name', 'y-coordinate of network nodes')

      !4. Geometry
      ierr = nf90_def_var(ncid, prefix//'_geometry', nf90_int, netids%varids(ntid_1dgeometry))
      ierr = nf90_put_att(ncid, netids%varids(ntid_1dgeometry), 'geometry_type', 'line')
      ierr = nf90_put_att(ncid, netids%varids(ntid_1dgeometry), 'long_name', '1D Geometry')
      ierr = nf90_put_att(ncid, netids%varids(ntid_1dgeometry), 'node_count', prefix//'_geom_node_count')
      ierr = nf90_put_att(ncid, netids%varids(ntid_1dgeometry), 'node_coordinates', prefix//'_geom_x '//prefix//'_geom_y')
      !4. Geometry: number of geometry points per each branch
      ierr = nf90_def_var(ncid, prefix//'_geom_node_count', nf90_int, (/netids%dimids(ntdim_1dedges)/), netids%varids(ntid_1dgeopointsperbranch))
      ierr = nf90_put_att(ncid, netids%varids(ntid_1dgeopointsperbranch), 'long_name', 'Number of geometry nodes per branch')
      !4. Geometry points x-coordinates
      !4. Geometry points y-coordinates
      ierr = nf90_def_var(ncid, prefix//'_geom_x', nf90_double, (/netids%dimids(ntdim_1dgeopoints)/), netids%varids(ntid_1dgeox))
      ierr = nf90_def_var(ncid, prefix//'_geom_y', nf90_double, (/netids%dimids(ntdim_1dgeopoints)/), netids%varids(ntid_1dgeoy))
      ierr = ug_addcoordatts(ncid, netids%varids(ntid_1dgeox), netids%varids(ntid_1dgeoy), crs)
      ierr = nf90_put_att(ncid, netids%varids(ntid_1dgeox), 'long_name', 'x-coordinate of branch geometry nodes')
      ierr = nf90_put_att(ncid, netids%varids(ntid_1dgeoy), 'long_name', 'y-coordinate of branch geometry nodes')

      !5 Branch order : might be temporary, could be defined, written and retrived using ug_def_var, ug_put_var, ug_get_var
      ierr = nf90_def_var(ncid, prefix//'_branch_order', nf90_int, (/netids%dimids(ntdim_1dedges)/), netids%varids(ntid_1dbranchorder))
      ierr = nf90_put_att(ncid, netids%varids(ntid_1dbranchorder), 'long_name', 'Order of branches for interpolation')
      ierr = nf90_put_att(ncid, netids%varids(ntid_1dbranchorder), 'mesh', prefix)
      ierr = nf90_put_att(ncid, netids%varids(ntid_1dbranchorder), 'location', 'edge')

      !6 Branch type
      ierr = nf90_def_var(ncid, prefix//'_branch_type', nf90_int, (/netids%dimids(ntdim_1dedges)/), netids%varids(ntid_1dbranchtype))
      ierr = nf90_put_att(ncid, netids%varids(ntid_1dbranchtype), 'long_name', 'Type of branches')
      ierr = nf90_put_att(ncid, netids%varids(ntid_1dbranchtype), 'mesh', prefix)
      ierr = nf90_put_att(ncid, netids%varids(ntid_1dbranchtype), 'location', 'edge')

      if (wasInDefine == 0) then
         ierr = nf90_enddef(ncid)
      end if

   end function ug_create_1d_network_v1

!> This function is included for backward compatibility
   function ug_create_1d_mesh(ncid, networkname, meshids, meshname, nmeshpoints) result(ierr)

      integer, intent(in) :: ncid, nmeshpoints
      type(t_ug_mesh), intent(inout) :: meshids
      character(len=*), intent(in) :: meshname, networkname
      integer :: ierr

      ierr = -1
      ierr = ug_create_1d_mesh_v1(ncid, networkname, meshids, meshname, nmeshpoints, 0)
   end function ug_create_1d_mesh

!> This function creates a 1d mesh accordingly to the new 1d format.
   function ug_create_1d_mesh_v1(ncid, networkname, meshids, meshname, nmeshpoints, writexy) result(ierr)

      integer, intent(in) :: ncid
      integer, intent(in) :: nmeshpoints
      type(t_ug_mesh), intent(inout) :: meshids
      character(len=*), intent(in) :: networkname
      character(len=*), intent(in) :: meshname
      integer, intent(in) :: writexy
      integer :: ierr

      !locals
      type(t_crs) :: crs           !< Coordinate reference system for the x/y-coordinates variables.

      crs%varname = 'Unknown projected'
      crs%epsg_code = 0

      ierr = -1
      ierr = ug_create_1d_mesh_v2(ncid, networkname, meshids, meshname, nmeshpoints, 0, writexy, crs)
   end function ug_create_1d_mesh_v1

!> This function creates a 1d mesh accordingly to the new 1d format.
!> Including correct names for x and y-coordinates.
   function ug_create_1d_mesh_v2(ncid, networkname, meshids, meshname, nmeshpoints, nmeshedges, writexy, crs) result(ierr)

      integer, intent(in) :: ncid
      integer, intent(in) :: nmeshpoints
      integer, intent(in) :: nmeshedges
      type(t_ug_mesh), intent(inout) :: meshids
      character(len=*), intent(in) :: networkname
      character(len=*), intent(in) :: meshname
      integer, intent(in) :: writexy
      type(t_crs), intent(in) :: crs           !< Coordinate reference system for the x/y-coordinates variables.
      integer :: ierr

      !locals
      integer :: wasInDefine
      character(len=len_trim(meshname)) :: prefix
      character(len=nf90_max_name) :: buffer

      prefix = trim(meshname)

      ierr = UG_SOMEERR
      wasInDefine = 0
      ierr = nf90_redef(ncid)
      if (ierr == nf90_eindefine) then
         wasInDefine = 1 ! Was still in define mode.
      end if

      !define dim
      ! This dimension might already be defined, check first if it is present
      ierr = nf90_inq_dimid(ncid, 'Two', meshids%dimids(mdim_two))
      if (ierr /= UG_NOERR) then
         ierr = nf90_def_dim(ncid, 'Two', 2, meshids%dimids(mdim_two))
      end if
      ierr = nf90_inq_dimid(ncid, prefix//'_nNodes', meshids%dimids(mdim_node))
      if (ierr /= UG_NOERR) then
         ierr = nf90_def_dim(ncid, prefix//'_nNodes', nmeshpoints, meshids%dimids(mdim_node))
      end if
      if (nmeshedges > 0) then
         ierr = nf90_inq_dimid(ncid, prefix//'_nEdges', meshids%dimids(mdim_edge))
         if (ierr /= UG_NOERR) then
            ierr = nf90_def_dim(ncid, prefix//'_nEdges', nmeshedges, meshids%dimids(mdim_edge))
         end if
      end if

      !define mesh1d accordingly to the UGRID format
      ierr = nf90_def_var(ncid, prefix, nf90_int, meshids%varids(mid_meshtopo))
      ierr = nf90_put_att(ncid, meshids%varids(mid_meshtopo), 'cf_role', 'mesh_topology')
      write (buffer, '(a,i0,a)') 'Topology data of 1D mesh'
      ierr = nf90_put_att(ncid, meshids%varids(mid_meshtopo), 'long_name', trim(buffer))
      ierr = nf90_put_att(ncid, meshids%varids(mid_meshtopo), 'topology_dimension', 1)
      ierr = nf90_put_att(ncid, meshids%varids(mid_meshtopo), 'coordinate_space', trim(networkname))
      ierr = nf90_put_att(ncid, meshids%varids(mid_meshtopo), 'edge_node_connectivity', prefix//'_edge_nodes')
      ierr = nf90_put_att(ncid, meshids%varids(mid_meshtopo), 'node_dimension', prefix//'_nNodes')
      if (nmeshedges > 0) then
         ierr = nf90_put_att(ncid, meshids%varids(mid_meshtopo), 'edge_dimension', prefix//'_nEdges')
      end if
      if (writexy == 1) then
         ierr = nf90_put_att(ncid, meshids%varids(mid_meshtopo), 'node_coordinates', prefix//'_node_branch '//prefix//'_node_offset '//prefix//'_node_x '//prefix//'_node_y')
         ierr = nf90_put_att(ncid, meshids%varids(mid_meshtopo), 'edge_coordinates', prefix//'_edge_branch '//prefix//'_edge_offset '//prefix//'_edge_x '//prefix//'_edge_y')
      end if
      if (writexy == 0) then
         ierr = nf90_put_att(ncid, meshids%varids(mid_meshtopo), 'node_coordinates', prefix//'_node_branch '//prefix//'_node_offset')
         ierr = nf90_put_att(ncid, meshids%varids(mid_meshtopo), 'edge_coordinates', prefix//'_edge_branch '//prefix//'_edge_offset')
      end if

      !
      ! Nodes
      !
      ! 1. mesh1D :assign the branch number to each node
      ierr = nf90_def_var(ncid, prefix//'_node_branch', nf90_int, (/meshids%dimids(mdim_node)/), meshids%varids(mid_1dnodebranch))
      ierr = nf90_put_att(ncid, meshids%varids(mid_1dnodebranch), 'long_name', 'Index of branch on which mesh nodes are located')
      ! NOTE: currently the only write/put command is in ug_put_1d_mesh_discretisation_points_v1, which states hardcoded start_index=0
      ! Quote: we have not defined the start_index, so when we put the variable it must be zero based
      ierr = nf90_put_att(ncid, meshids%varids(mid_1dnodebranch), 'start_index', 0)

      ! 2. mesh1D :assign the the offset from the starting node
      ierr = nf90_def_var(ncid, prefix//'_node_offset', nf90_double, (/meshids%dimids(mdim_node)/), meshids%varids(mid_1dnodeoffset))
      ierr = nf90_put_att(ncid, meshids%varids(mid_1dnodeoffset), 'long_name', 'Offset along branch of mesh nodes')
      ierr = nf90_put_att(ncid, meshids%varids(mid_1dnodeoffset), 'units', 'm')

      if (writexy == 1) then
         ierr = nf90_def_var(ncid, prefix//'_node_x', nf90_double, (/meshids%dimids(mdim_node)/), meshids%varids(mid_nodex))
         ierr = nf90_def_var(ncid, prefix//'_node_y', nf90_double, (/meshids%dimids(mdim_node)/), meshids%varids(mid_nodey))
         ierr = ug_addcoordatts(ncid, meshids%varids(mid_nodex), meshids%varids(mid_nodey), crs)
         ierr = nf90_put_att(ncid, meshids%varids(mid_nodex), 'long_name', 'x-coordinate of mesh nodes')
         ierr = nf90_put_att(ncid, meshids%varids(mid_nodey), 'long_name', 'y-coordinate of mesh nodes')
      end if

      !
      ! Edges
      !
      if (nmeshedges > 0) then
         ! 1. mesh1D :assign the branch number to each edge
         ierr = nf90_def_var(ncid, prefix//'_edge_branch', nf90_int, (/meshids%dimids(mdim_edge)/), meshids%varids(mid_1dedgebranch))
         ierr = nf90_put_att(ncid, meshids%varids(mid_1dedgebranch), 'long_name', 'Index of branch on which mesh edges are located')
         ! NOTE: currently the only write/put command is in ug_put_1d_mesh_discretisation_points_v1, which states hardcoded start_index=0
         ! Quote: we have not defined the start_index, so when we put the variable it must be zero based
         ierr = nf90_put_att(ncid, meshids%varids(mid_1dedgebranch), 'start_index', 0)

         ! 2. mesh1D :assign the the offset along the branch for each edge
         ierr = nf90_def_var(ncid, prefix//'_edge_offset', nf90_double, (/meshids%dimids(mdim_edge)/), meshids%varids(mid_1dedgeoffset))
         ierr = nf90_put_att(ncid, meshids%varids(mid_1dedgeoffset), 'long_name', 'Offset along branch of mesh edges')
         ierr = nf90_put_att(ncid, meshids%varids(mid_1dedgeoffset), 'units', 'm')

         if (writexy == 1) then
            ierr = nf90_def_var(ncid, prefix//'_edge_x', nf90_double, (/meshids%dimids(mdim_edge)/), meshids%varids(mid_edgex))
            ierr = nf90_def_var(ncid, prefix//'_edge_y', nf90_double, (/meshids%dimids(mdim_edge)/), meshids%varids(mid_edgey))
            ierr = ug_addcoordatts(ncid, meshids%varids(mid_edgex), meshids%varids(mid_edgey), crs)
            ierr = nf90_put_att(ncid, meshids%varids(mid_edgex), 'long_name', 'Characteristic x-coordinate of the mesh edge (e.g. midpoint)')
            ierr = nf90_put_att(ncid, meshids%varids(mid_edgey), 'long_name', 'Characteristic y-coordinate of the mesh edge (e.g. midpoint)')
         end if
      end if

      if (wasInDefine == 0) then
         ierr = nf90_enddef(ncid)
      end if

   end function ug_create_1d_mesh_v2

!> This function defines the ids of a specific entity (node/edge/face) on the current mesh and creates the variable to store the ids
   function ug_def_mesh_ids(ncid, meshids, meshname, locationType) result(ierr)

      integer, intent(in) :: ncid, locationType
      type(t_ug_mesh), intent(inout) :: meshids
      character(len=*), intent(in) :: meshname
      character(len=len_trim(meshname)) :: prefix
      integer :: ierr, wasInDefine

      prefix = trim(meshname)

      ierr = UG_SOMEERR

      wasInDefine = 0
      ierr = nf90_redef(ncid)
      if (ierr == nf90_eindefine) then
         wasInDefine = 1 ! Was still in define mode.
      end if

      ierr = nf90_inq_dimid(ncid, 'strLengthIds', meshids%dimids(mdim_idstring))
      if (ierr /= UG_NOERR) then
         ierr = nf90_def_dim(ncid, 'strLengthIds', ug_idsLen, meshids%dimids(mdim_idstring))
      end if
      ierr = nf90_inq_dimid(ncid, 'strLengthLongNames', meshids%dimids(mdim_longnamestring))
      if (ierr /= UG_NOERR) then
         ierr = nf90_def_dim(ncid, 'strLengthLongNames', ug_idsLongNamesLen, meshids%dimids(mdim_longnamestring))
      end if

      if (locationType == UG_LOC_NODE) then
         !ids
         ierr = nf90_put_att(ncid, meshids%varids(mid_meshtopo), 'node_id', prefix//'_node_id')
         ierr = nf90_def_var(ncid, prefix//'_node_id', nf90_char, (/meshids%dimids(mdim_idstring), meshids%dimids(mdim_node)/), meshids%varids(mid_node_ids))
         ierr = nf90_put_att(ncid, meshids%varids(mid_node_ids), 'long_name', 'ID of mesh nodes')
         !long_names
         ierr = nf90_put_att(ncid, meshids%varids(mid_meshtopo), 'node_long_name', prefix//'_node_long_name')
         ierr = nf90_def_var(ncid, prefix//'_node_long_name', nf90_char, (/meshids%dimids(mdim_longnamestring), meshids%dimids(mdim_node)/), meshids%varids(mid_node_longnames))
         ierr = nf90_put_att(ncid, meshids%varids(mid_node_longnames), 'long_name', 'Long name of mesh nodes')
      else if (locationType == UG_LOC_EDGE) then
         !ids
         ierr = nf90_put_att(ncid, meshids%varids(mid_meshtopo), 'edge_id', prefix//'_edge_id')
         ierr = nf90_def_var(ncid, prefix//'_edge_id', nf90_char, (/meshids%dimids(mdim_idstring), meshids%dimids(mdim_edge)/), meshids%varids(mid_edge_ids))
         ierr = nf90_put_att(ncid, meshids%varids(mid_edge_ids), 'long_name', 'ID of mesh edges')
         !long names
         ierr = nf90_put_att(ncid, meshids%varids(mid_meshtopo), 'edge_long_name', prefix//'_edge_long_name')
         ierr = nf90_def_var(ncid, prefix//'_edge_long_name', nf90_char, (/meshids%dimids(mdim_longnamestring), meshids%dimids(mdim_edge)/), meshids%varids(mid_edge_longnames))
         ierr = nf90_put_att(ncid, meshids%varids(mid_edge_longnames), 'long_name', 'Long name of mesh edges')
      else if (locationType == UG_LOC_FACE) then
         !ids
         ierr = nf90_put_att(ncid, meshids%varids(mid_meshtopo), 'face_id', prefix//'_face_id')
         ierr = nf90_def_var(ncid, prefix//'_face_id', nf90_char, (/meshids%dimids(mdim_idstring), meshids%dimids(mdim_face)/), meshids%varids(mid_face_ids))
         ierr = nf90_put_att(ncid, meshids%varids(mid_face_ids), 'long_name', 'ID of mesh faces')
         !long names
         ierr = nf90_put_att(ncid, meshids%varids(mid_meshtopo), 'face_long_name', prefix//'_face_long_name')
         ierr = nf90_def_var(ncid, prefix//'_face_long_name', nf90_char, (/meshids%dimids(mdim_longnamestring), meshids%dimids(mdim_face)/), meshids%varids(mid_face_longnames))
         ierr = nf90_put_att(ncid, meshids%varids(mid_face_longnames), 'long_name', 'Long name of mesh faces')
      end if

      if (wasInDefine == 0) then
         ierr = nf90_enddef(ncid)
      end if

   end function ug_def_mesh_ids

! Creates a mesh_topology_contact variable for storing contacts between meshes.
   function ug_def_mesh_contact(ncid, contactids, linkmeshname, ncontacts, meshidfrom, meshidto, locationType1Id, locationType2Id, start_index) result(ierr)

      integer, intent(in) :: ncid, locationType1Id, locationType2Id, ncontacts
      type(t_ug_mesh), intent(in) :: meshidfrom, meshidto
      character(len=*), intent(in) :: linkmeshname
      character(len=len_trim(linkmeshname)) :: prefix
      type(t_ug_contacts), intent(inout) :: contactids
      character(len=nf90_max_name) :: locationType1, locationType2, mesh1, mesh2
      integer :: ierr, wasInDefine
      integer, optional :: start_index

      ierr = UG_SOMEERR
      wasInDefine = 0
      ierr = nf90_redef(ncid)
      if (ierr == nf90_eindefine) then
         wasInDefine = 1 ! Was still in define mode.
      end if

      prefix = trim(linkmeshname)
      !define dim
      ierr = nf90_def_dim(ncid, prefix//'_nContacts', ncontacts, contactids%dimids(cdim_ncontacts))
      !These dimensions might already be defined, check first if they are present
      ierr = nf90_inq_dimid(ncid, 'strLengthIds', contactids%dimids(cdim_idstring))
      if (ierr /= UG_NOERR) then
         ierr = nf90_def_dim(ncid, 'strLengthIds', ug_idsLen, contactids%dimids(cdim_idstring))
      end if
      ierr = nf90_inq_dimid(ncid, 'strLengthLongNames', contactids%dimids(cdim_longnamestring))
      if (ierr /= UG_NOERR) then
         ierr = nf90_def_dim(ncid, 'strLengthLongNames', ug_idsLongNamesLen, contactids%dimids(cdim_longnamestring))
      end if
      ierr = nf90_inq_dimid(ncid, 'Two', contactids%dimids(cdim_two))
      if (ierr /= UG_NOERR) then
         ierr = nf90_def_dim(ncid, 'Two', 2, contactids%dimids(cdim_two))
      end if

      !select the location type
      call ug_loctype_to_location(locationType1Id, locationType1)
      if (ierr /= UG_NOERR) then
         call SetMessage(Level_Fatal, 'Could not select locationType1 for mesh contact '''//trim(linkmeshname)//'''.')
      end if
      call ug_loctype_to_location(locationType2Id, locationType2)
      if (ierr /= UG_NOERR) then
         call SetMessage(Level_Fatal, 'Could not select locationType2 for mesh contact '''//trim(linkmeshname)//'''.')
      end if

      !get the mesh names
      ierr = ug_get_mesh_name(ncid, meshidfrom, meshname=mesh1)
      ierr = ug_get_mesh_name(ncid, meshidto, meshname=mesh2)

      !define the variable contacts and its attributes
      ierr = nf90_def_var(ncid, prefix, nf90_int, (/contactids%dimids(cdim_two), contactids%dimids(cdim_ncontacts)/), contactids%varids(cid_contacttopo))
      ierr = nf90_put_att(ncid, contactids%varids(cid_contacttopo), 'cf_role', 'mesh_topology_contact')
      ierr = nf90_put_att(ncid, contactids%varids(cid_contacttopo), 'contact', trim(mesh1)//': '//trim(locationType1)//' '//trim(mesh2)//': '//trim(locationType2))
      ierr = nf90_put_att(ncid, contactids%varids(cid_contacttopo), 'contact_type', prefix//'_contact_type')
      ierr = nf90_put_att(ncid, contactids%varids(cid_contacttopo), 'contact_id', prefix//'_contact_id')
      ierr = nf90_put_att(ncid, contactids%varids(cid_contacttopo), 'contact_long_name', prefix//'_contact_long_name')
      if (present(start_index)) then
         if (start_index /= 0) ierr = nf90_put_att(ncid, contactids%varids(cid_contacttopo), 'start_index', start_index)
      end if
      ierr = nf90_put_att(ncid, contactids%varids(cid_contacttopo), '_FillValue', imiss)

      !define the variable and attributes contacts id
      ierr = nf90_def_var(ncid, prefix//'_contact_id', nf90_char, (/contactids%dimids(cdim_idstring), contactids%dimids(cdim_ncontacts)/), contactids%varids(cid_contactids))
      ierr = nf90_put_att(ncid, contactids%varids(cid_contactids), 'long_name', ' ID of mesh contacts')

      !define the variable and attributes contact long name
      ierr = nf90_def_var(ncid, prefix//'_contact_long_name', nf90_char, (/contactids%dimids(cdim_longnamestring), contactids%dimids(cdim_ncontacts)/), contactids%varids(cid_contactlongnames))
      ierr = nf90_put_att(ncid, contactids%varids(cid_contactlongnames), 'long_name', 'Long name of mesh contacts')

      !define the variable and attributes long names
      ierr = nf90_def_var(ncid, prefix//'_contact_type', nf90_int, (/contactids%dimids(cdim_ncontacts)/), contactids%varids(cid_contacttype))
      ierr = nf90_put_att(ncid, contactids%varids(cid_contacttype), '_FillValue', -1)
      ierr = nf90_put_att(ncid, contactids%varids(cid_contacttype), 'valid_range', (/3, 4, 5, 7/))
      ierr = nf90_put_att(ncid, contactids%varids(cid_contacttype), 'flag_values', (/3, 4, 5, 7/))
      ierr = nf90_put_att(ncid, contactids%varids(cid_contacttype), 'flag_meanings', 'lateral_1d2d_link longitudinal_1d2d_link street_inlet_1d2d_link roof_gutter_1d2d_link')

      !define the variable contacts and its attributes
      ierr = nf90_def_var(ncid, "composite_mesh", nf90_int, contactids%varids(cid_compositemesh))
      ierr = nf90_put_att(ncid, contactids%varids(cid_compositemesh), 'cf_role', 'parent_mesh_topology')
      ierr = nf90_put_att(ncid, contactids%varids(cid_compositemesh), 'meshes', trim(mesh1)//' '//trim(mesh2))
      ierr = nf90_put_att(ncid, contactids%varids(cid_compositemesh), 'mesh_contact', prefix)

      if (wasInDefine == 0) then
         ierr = nf90_enddef(ncid)
      end if

   end function ug_def_mesh_contact
!> Writes edge nodes
   function ug_write_mesh_1d_edge_nodes(ncid, meshids, meshName, numEdge, mesh_1d_edge_nodes, start_index) result(ierr)
      integer, intent(in) :: ncid                     !< NetCDF dataset id, should be already open and ready for writing.
      type(t_ug_mesh), intent(inout) :: meshids                  !< Set of NetCDF-ids for all mesh geometry arrays.
      character(len=*), intent(in) :: meshName                 !< Name for the mesh variable, also used as prefix for all related entities.
      integer, intent(in) :: numEdge                  !< Number of edges in the mesh.
      integer, intent(in) :: mesh_1d_edge_nodes(:, :)  !< Edge-to-node mapping array.
      integer :: start_index              !< The base index of the provided arrays (0 if this function writes array from C/C++/C#, 1 for Fortran)
      integer :: ierr                     !< Result status (UG_NOERR==NF90_NOERR) if successful.

      character(len=len_trim(meshName)) :: prefix
      integer :: wasInDefine

      ierr = UG_SOMEERR
      wasInDefine = 0

      ierr = nf90_redef(ncid)
      if (ierr == nf90_eindefine) wasInDefine = 1 ! Was still in define mod

      prefix = trim(meshName)

      ! Edges
      ierr = nf90_inq_dimid(ncid, prefix//'_edge_nodes', meshids%dimids(mdim_1dedgenodes))
      if (ierr /= UG_NOERR) then
         ierr = nf90_def_var(ncid, prefix//'_edge_nodes', nf90_int, (/meshids%dimids(mdim_two), meshids%dimids(mdim_edge)/), meshids%varids(mdim_1dedgenodes))
         ierr = nf90_put_att(ncid, meshids%varids(mdim_1dedgenodes), 'cf_role', 'edge_node_connectivity')
         ierr = nf90_put_att(ncid, meshids%varids(mdim_1dedgenodes), 'long_name', 'Start and end nodes of mesh edges')
         if (start_index /= -1) then
            ierr = nf90_put_att(ncid, meshids%varids(mdim_1dedgenodes), 'start_index', start_index)
         end if
      end if

      ierr = nf90_enddef(ncid)

! -- end of header --

      ! Write the actual data

      ! Edges:
      ! always write edge nodes
      if (meshids%varids(mdim_1dedgenodes) /= -1) then
         ierr = nf90_put_var(ncid, meshids%varids(mdim_1dedgenodes), mesh_1d_edge_nodes, count=(/2, numEdge/))
      end if

      ! Check for any remaining native NetCDF errors
      if (ierr /= nf90_noerr) then
         goto 801
      end if

      ! Leave the dataset in the same mode as we got it.
      if (wasInDefine == 1) then
         ierr = nf90_redef(ncid)
      end if

      ierr = UG_NOERR
      return ! Return with success

801   continue

   end function ug_write_mesh_1d_edge_nodes

!> Get the number of contact links in a given meshcontact set.
   function ug_get_contacts_count(ncid, contactids, ncontacts) result(ierr)

      integer, intent(in) :: ncid       !< NetCDF data set id
      type(t_ug_contacts), intent(in) :: contactids !< Mesh contact set
      integer, intent(out) :: ncontacts  !< Number of contact links in the given meshcontact set.
      integer :: ierr       !< Result status (IONC_NOERR if successful).

      integer :: xtype, ndims, nAtts, ncontactsDim1, ncontactsDim2
      character(len=nf90_max_name) :: name
      integer, dimension(nf90_max_dims) :: dimids

      ierr = nf90_inquire_variable(ncid, contactids%varids(cid_contacttopo), name=name, xtype=xtype, ndims=ndims, dimids=dimids, nAtts=nAtts)
      if (ierr /= UG_NOERR) then
         call SetMessage(Level_Fatal, 'Could not inquire the number of contacts for mesh contact '''//trim(name)//'''.')
      end if

      ierr = nf90_inquire_dimension(ncid, dimids(1), len=ncontactsDim1)
      if (ierr /= UG_NOERR) then
         call SetMessage(Level_Fatal, 'Could not read the first dimension for mesh contact '''//trim(name)//'''.')
      end if

      ierr = nf90_inquire_dimension(ncid, dimids(2), len=ncontactsDim2)
      if (ierr /= UG_NOERR) then
         call SetMessage(Level_Fatal, 'Could not read the second dimension for mesh contact '''//trim(name)//'''.')
      end if

      ! ncontacts = max(ncontactsDim1,ncontactsDim2)
      ncontacts = ncontactsDim2

   end function ug_get_contacts_count

! Writes the mesh_topology_contact mesh.
   function ug_put_mesh_contact(ncid, contactids, mesh1indexes, mesh2indexes, contacttype, contactsids, contactslongnames, startIndex) result(ierr)
      use array_module
      integer, intent(in) :: ncid
      type(t_ug_contacts), intent(in) :: contactids
      integer, intent(in) :: mesh1indexes(:), mesh2indexes(:), contacttype(:)
      integer, allocatable :: contacts(:, :)
      character(len=*), optional, intent(in) :: contactsids(:), contactslongnames(:)
      integer, intent(in), optional :: startIndex
      integer :: ierr

      ierr = UG_SOMEERR
      ierr = nf90_enddef(ncid) !Put the NetCDF in write mode

      allocate (contacts(2, size(mesh1indexes)))

      contacts(1, :) = mesh1indexes(:)
      contacts(2, :) = mesh2indexes(:)

      !we have not defined the start_index, so when we put the variable it must be zero based
      if (present(startIndex)) then
         if (startIndex /= -1) then
            ierr = convert_start_index(contacts(1, :), imiss, startIndex, 0)
            ierr = convert_start_index(contacts(2, :), imiss, startIndex, 0)
         end if
      end if

      ierr = nf90_put_var(ncid, contactids%varids(cid_contacttopo), abs(contacts))

      if (present(contactsids)) then
         ierr = nf90_put_var(ncid, contactids%varids(cid_contactids), contactsids)
      end if
      if (present(contactslongnames)) then
         ierr = nf90_put_var(ncid, contactids%varids(cid_contactlongnames), contactslongnames)
      end if

      ierr = nf90_put_var(ncid, contactids%varids(cid_contacttype), contacttype)

   end function ug_put_mesh_contact

! Gets the indexes of the contacts and the ids and the descriptions of each link
   function ug_get_mesh_contact(ncid, contactids, mesh1indexes, mesh2indexes, contactsids, contactslongnames, contacttype, startIndex) result(ierr)
      use array_module
      integer, intent(in) :: ncid, startIndex
      type(t_ug_contacts), intent(in) :: contactids
      integer, intent(out) :: mesh1indexes(:), mesh2indexes(:), contacttype(:)
      character(len=*), intent(out) :: contactsids(:), contactslongnames(:)
      integer, allocatable :: contacts(:, :)
      integer :: ierr, i
      integer :: ncontacts_file
      integer :: idstrlen_file, idstrlen_local, longnamestrlen_file, longnamestrlen_local
      character(len=:), allocatable :: tmpstring(:)

      allocate (contacts(2, size(mesh1indexes)))

      ierr = ug_get_mesh_contact_links(ncid, contactids, contacts, contacttype, startIndex)

      ! The string length on the net file can be different from the internal ID-length of D-FlowFM
      ! Retrieve the dimensions of the contact ids.
      ierr = nf90_inquire_dimension(ncid, contactids%dimids(cdim_ncontacts), len=ncontacts_file)
      ierr = nf90_inquire_dimension(ncid, contactids%dimids(cdim_idstring), len=idstrlen_file)
      ierr = nf90_inquire_dimension(ncid, contactids%dimids(cdim_longnamestring), len=longnamestrlen_file)

      if (ncontacts_file /= size(contactsids)) then
         call setmessage(LEVEL_ERROR, 'The number of contacts in the network file is different from the number of contacts in the kernel')
      end if
      if (ncontacts_file == 0) then
         return
      end if

      idstrlen_local = len(contactsids(1))

      if (idstrlen_file > idstrlen_local) then
         write (msgbuf, '(''The number of characters in the id string for the contacts (= '', i0,''), exceeds the maximum string length (= '', i0,'')'')') idstrlen_file, idstrlen_local
         call err_flush()
      end if

      ! Use a temporary array for reading the id's
      allocate (character(len=idstrlen_file) :: tmpstring(ncontacts_file))
      ierr = nf90_get_var(ncid, contactids%varids(cid_contactids), tmpstring)

      do i = 1, ncontacts_file
         contactsids(i) = tmpstring(i)
      end do

      deallocate (tmpstring)

      longnamestrlen_local = len(contactslongnames(1))

      if (longnamestrlen_file > longnamestrlen_local) then
         write (msgbuf, '(''The number of characters in the longname string for the contacts (= '', i0,''), exceeds the maximum string length (= '', i0,'')'')') longnamestrlen_file, longnamestrlen_local
         call err_flush()
      end if

      ! Use a temporary array for reading the longnames
      allocate (character(len=longnamestrlen_file) :: tmpstring(ncontacts_file))
      ierr = nf90_get_var(ncid, contactids%varids(cid_contactlongnames), tmpstring)

      do i = 1, ncontacts_file
         contactslongnames(i) = tmpstring(i)
      end do

      deallocate (tmpstring)

      do i = 1, size(mesh1indexes)
         mesh1indexes(i) = contacts(1, i)
         mesh2indexes(i) = contacts(2, i)
      end do

   end function ug_get_mesh_contact

!> Gets the index table of the links in a given meshcontact set.
!! The index table is renumbered automatically when the given.
!! desired startIndex is different from the startIndex in the file.
!! The arrays should be allocated at the correct size already.
   function ug_get_mesh_contact_links(ncid, contactids, contactlinksfromto, contacttype, startIndex) result(ierr)
      use array_module
      integer, intent(in) :: ncid           !< NetCDF dataset id
      type(t_ug_contacts), intent(in) :: contactids     !< Mesh contact topology set
      integer, intent(out) :: contactlinksfromto(:, :) !< (2,numcontactlinks) table with from-to indices
      integer, intent(out) :: contacttype(:) !< Contact type for each link
      integer, intent(in) :: startIndex     !< Desired start index for the table.
      integer :: ierr           !< Result status (UG_NOERR if succesful)

      integer :: varStartIndex
      integer :: ifill

      ierr = nf90_get_var(ncid, contactids%varids(cid_contacttopo), contactlinksfromto)
      ierr = nf90_get_var(ncid, contactids%varids(cid_contacttype), contacttype)

      ierr = nf90_get_att(ncid, contactids%varids(cid_contacttopo), '_FillValue', ifill)
      if (ierr /= nf90_noerr) then
         ! UNST-6932: only for mesh contacts, backwards compatibility for the files when _FillValue
         !            was not written yet, and data was based on global imiss.
         ifill = imiss
      end if

      !we check for the start_index, we do not know if the variable was written as 0 based
      ierr = nf90_get_att(ncid, contactids%varids(cid_contacttopo), 'start_index', varStartIndex)
      if (ierr == UG_NOERR) then
         ierr = convert_start_index(contactlinksfromto(1, :), ifill, varStartIndex, startIndex)
         ierr = convert_start_index(contactlinksfromto(2, :), ifill, varStartIndex, startIndex)
      else
         ierr = convert_start_index(contactlinksfromto(1, :), ifill, 0, startIndex)
         ierr = convert_start_index(contactlinksfromto(2, :), ifill, 0, startIndex)
      end if

   end function ug_get_mesh_contact_links

!> This function writes the nodes of the 1d network
   function ug_write_1d_network_nodes(ncid, netids, nodesX, nodesY, nodeids, nodelongnames) result(ierr)

      integer, intent(in) :: ncid
      type(t_ug_network), intent(in) :: netids !< Set of NetCDF-ids for all mesh geometry arrays
      double precision, intent(in) :: nodesX(:), nodesY(:)
      character(len=*), optional, intent(in) :: nodeids(:), nodelongnames(:)
      integer :: ierr

      ierr = UG_SOMEERR
      !Put the NetCDF in write mode
      ierr = nf90_enddef(ncid)

      ierr = nf90_put_var(ncid, netids%varids(ntid_1dnodex), nodesX)
      ierr = nf90_put_var(ncid, netids%varids(ntid_1dnodey), nodesY)
      if (present(nodeids)) then
         ierr = nf90_put_var(ncid, netids%varids(ntid_1dnodids), nodeids)
      end if
      if (present(nodelongnames)) then
         ierr = nf90_put_var(ncid, netids%varids(ntid_1dnodlongnames), nodelongnames)
      end if

   end function ug_write_1d_network_nodes

!> This function writes the branches information
!> This function writes the branches information
   function ug_put_1d_network_branches(ncid, netids, sourceNodeId, targetNodeId, branchids, branchlengths, branchlongnames, nbranchgeometrynodes, nBranches, startIndex) result(ierr)
      use array_module
      integer, intent(in) :: ncid, nBranches, startIndex
      type(t_ug_network), intent(in) :: netids !< Set of NetCDF-ids for all mesh geometry arrays
      integer, intent(in) :: sourceNodeId(:), targetNodeId(:)
      integer, allocatable :: sourcestargets(:, :)
      double precision, intent(in) :: branchlengths(:)
      character(len=*), intent(in) :: branchids(:), branchlongnames(:)
      integer, intent(in) :: nbranchgeometrynodes(:)
      integer :: ierr, n, k

      ierr = UG_SOMEERR
      ierr = nf90_enddef(ncid)

      allocate (sourcestargets(2, nBranches))
      k = 0
      do n = 1, nBranches
         k = k + 1
         sourcestargets(1, k) = sourceNodeId(n)
         sourcestargets(2, k) = targetNodeId(n)
      end do

      !we have not defined the start_index, so when we put the variable it must be zero based
      if (startIndex /= -1) then
         ierr = convert_start_index(sourcestargets(1, :), imiss, startIndex, 0)
         ierr = convert_start_index(sourcestargets(2, :), imiss, startIndex, 0)
      end if

      ierr = nf90_put_var(ncid, netids%varids(ntid_1dedgenodes), sourcestargets)
      ierr = nf90_put_var(ncid, netids%varids(ntid_1dbranchids), branchids)
      if (size(branchlongnames) > 0) then
         ierr = nf90_put_var(ncid, netids%varids(ntid_1dbranchlongnames), branchlongnames)
      end if
      ierr = nf90_put_var(ncid, netids%varids(ntid_1dbranchlengths), branchlengths)
      ierr = nf90_put_var(ncid, netids%varids(ntid_1dgeopointsperbranch), nbranchgeometrynodes)

   end function ug_put_1d_network_branches

!> This function writes the branch order array
   function ug_put_1d_network_branchorder(ncid, netids, branchorder) result(ierr)

      integer, intent(in) :: ncid
      type(t_ug_network), intent(in) :: netids !< Set of NetCDF-ids for network
      integer, intent(in) :: branchorder(:)
      integer :: ierr

      ierr = nf90_put_var(ncid, netids%varids(ntid_1dbranchorder), branchorder)

   end function ug_put_1d_network_branchorder

   !> This function writes the branch order array
   function ug_put_1d_network_branchtype(ncid, netids, branchtypes) result(ierr)

      integer, intent(in) :: ncid
      type(t_ug_network), intent(in) :: netids !< Set of NetCDF-ids for network
      integer, intent(in) :: branchtypes(:)
      integer :: ierr

      ierr = nf90_put_var(ncid, netids%varids(ntid_1dbranchtype), branchtypes)

   end function ug_put_1d_network_branchtype

!> This function writes the geometry points
   function ug_write_1d_network_branches_geometry(ncid, netids, geopointsX, geopointsY) result(ierr)

      integer, intent(in) :: ncid
      type(t_ug_network), intent(in) :: netids
      double precision, intent(in) :: geopointsX(:), geopointsY(:)
      integer :: ierr

      ierr = UG_SOMEERR
      ierr = nf90_enddef(ncid)

      ierr = nf90_put_var(ncid, netids%varids(ntid_1dgeox), geopointsX)
      ierr = nf90_put_var(ncid, netids%varids(ntid_1dgeoy), geopointsY)

   end function ug_write_1d_network_branches_geometry

!> This function writes the mesh points
   function ug_put_1d_mesh_discretisation_points(ncid, meshids, nodebranchidx, nodeoffset, startIndex) result(ierr)

      integer, intent(in) :: ncid, nodebranchidx(:), startIndex
      double precision, intent(in) :: nodeoffset(:)
      type(t_ug_mesh), intent(in) :: meshids

      integer :: ierr

      ierr = UG_SOMEERR
      ierr = ug_put_1d_mesh_discretisation_points_v1(ncid, meshids, nodebranchidx, nodeoffset, startIndex)

   end function ug_put_1d_mesh_discretisation_points

!> This function writes the mesh points
   function ug_put_1d_mesh_discretisation_points_v1(ncid, meshids, nodebranchidx, nodeoffset, startIndex, coordx, coordy) result(ierr)
      use array_module
      integer, intent(in) :: ncid, nodebranchidx(:), startIndex
      double precision, intent(in) :: nodeoffset(:)
      type(t_ug_mesh), intent(in) :: meshids
      double precision, dimension(:), optional, intent(in) :: coordx, coordy
      integer, allocatable :: shiftednodebranchidx(:)
      integer :: ierr, nmeshpoints

      ierr = UG_SOMEERR
      ierr = nf90_enddef(ncid)

      ierr = nf90_inquire_dimension(ncid, meshids%dimids(mdim_node), len=nmeshpoints)
      if (ierr /= UG_NOERR) then
         call SetMessage(Level_Fatal, 'Could not read the 1D mesh node dimension. Check any previous warnings.')
      end if

      if (nmeshpoints < size(nodebranchidx) .or. nmeshpoints < size(nodeoffset)) then
         write (msgbuf, '(a,i0,a,i0,a)') 'Cannot write 1D mesh points: number of points in mesh definition (', nmeshpoints, &
            ') differs from input array size (', max(size(nodebranchidx), size(nodeoffset)), ').'
         call err_flush()
         return
      end if

      !we have not defined the start_index, so when we put the variable it must be zero based
      allocate (shiftednodebranchidx(size(nodebranchidx)))
      shiftednodebranchidx = nodebranchidx
      if (startIndex /= -1) then
         ierr = convert_start_index(shiftednodebranchidx, imiss, startIndex, 0)
      end if

      ierr = nf90_put_var(ncid, meshids%varids(mid_1dnodebranch), shiftednodebranchidx)
      ierr = nf90_put_var(ncid, meshids%varids(mid_1dnodeoffset), nodeoffset)

      if (present(coordx) .and. present(coordy)) then
         ierr = nf90_put_var(ncid, meshids%varids(mid_nodex), coordx)
         ierr = nf90_put_var(ncid, meshids%varids(mid_nodey), coordy)
      end if

   end function ug_put_1d_mesh_discretisation_points_v1

!> Write the mesh1d edge coordinates.
!! The edge coordinates are representative coordinates for quantities defined on the edge,
!! typically the middle of each edge. Default form for a 1d mesh is by branch index+offset,
!! but x/y may be given as well.
   function ug_put_1d_mesh_edges(ncid, meshids, edgebranchidx, edgeoffset, startIndex, coordx, coordy) result(ierr)
      use array_module
      integer, intent(in) :: ncid             !< NetCDF dataset id, should be already open and ready for writing.
      type(t_ug_mesh), intent(in) :: meshids          !< Set of NetCDF-ids for all mesh geometry arrays.
      integer, intent(in) :: edgebranchidx(:) !< Branch index for each mesh1d edge.
      double precision, intent(in) :: edgeoffset(:)    !< Offset along branch for each mesh1d edge.
      integer, intent(in) :: startIndex       !< Start index convention used in input edge branch indexes (0 or 1).
      double precision, optional, intent(in) :: coordx(:)        !< (Optional) representative x-coordinate of each mesh1d edge.
      double precision, optional, intent(in) :: coordy(:)        !< (Optional) representative y-coordinate of each mesh1d edge.
      integer :: ierr             !< Result status (UG_NOERR if successful).

      integer, allocatable :: shiftededgebranchidx(:)
      integer :: nmeshedges
      integer :: jaInData

      ierr = UG_SOMEERR

      ! Put dataset in data mode (possibly it is already) to write variables.
      jaInData = 0
      ierr = nf90_enddef(ncid)
      if (ierr == nf90_enotindefine) jaInData = 1 ! Was already in data mode.

      ierr = nf90_inquire_dimension(ncid, meshids%dimids(mdim_edge), len=nmeshedges)
      if (ierr /= UG_NOERR) then
         call SetMessage(Level_Fatal, 'Could not read the 1D mesh edge dimension. Check any previous warnings.')
      end if

      !we have not defined the start_index, so when we put the variable it must be zero based
      allocate (shiftededgebranchidx(size(edgebranchidx)))
      shiftededgebranchidx = edgebranchidx
      if (startIndex /= -1) then
         ierr = convert_start_index(shiftededgebranchidx, imiss, startIndex, 0)
      end if

      if (ierr == UG_NOERR) ierr = nf90_put_var(ncid, meshids%varids(mid_1dedgebranch), shiftededgebranchidx)
      if (ierr == UG_NOERR) ierr = nf90_put_var(ncid, meshids%varids(mid_1dedgeoffset), edgeoffset)

      if (present(coordx) .and. present(coordy)) then
         if (ierr == UG_NOERR) ierr = nf90_put_var(ncid, meshids%varids(mid_edgex), coordx)
         if (ierr == UG_NOERR) ierr = nf90_put_var(ncid, meshids%varids(mid_edgey), coordy)
      end if

      ! Leave the dataset in the mode we got it in.
      if (jaInData == 0) then
         if (ierr == UG_NOERR) ierr = nf90_redef(ncid)
      end if

      if (ierr /= UG_NOERR) then
         call SetMessage(Level_Fatal, 'Could not write the 1d mesh edges. Check any previous warnings.')
      end if

   end function ug_put_1d_mesh_edges

!> This function gets the number of network nodes
   function ug_get_1d_network_nodes_count(ncid, netids, nNodes) result(ierr)

      integer, intent(in) :: ncid
      type(t_ug_network), intent(in) :: netids
      integer, intent(out) :: nNodes
      integer :: ierr

      ierr = nf90_inquire_dimension(ncid, netids%dimids(ntdim_1dnodes), len=nNodes)
      if (ierr /= UG_NOERR) then
         call SetMessage(Level_Fatal, 'Could not read the 1D network nodes count. Check previous warnings.')
      end if

   end function ug_get_1d_network_nodes_count

!> This function gets the number of branches
   function ug_get_1d_network_branches_count(ncid, netids, nbranches) result(ierr)

      integer, intent(in) :: ncid
      type(t_ug_network), intent(in) :: netids
      integer, intent(out) :: nbranches
      integer :: ierr

      ierr = nf90_inquire_dimension(ncid, netids%dimids(ntdim_1dedges), len=nbranches)
      if (ierr /= UG_NOERR) then
         call SetMessage(Level_Fatal, 'Could not read the 1D network number of branches. Check any previous warnings.')
      end if

   end function ug_get_1d_network_branches_count

!> This function gets the number of geometry points
   function ug_get_1d_network_branches_geometry_coordinate_count(ncid, netids, ngeometry) result(ierr)

      integer, intent(in) :: ncid
      type(t_ug_network), intent(in) :: netids
      integer, intent(out) :: ngeometry
      integer :: ierr

      ierr = nf90_inquire_dimension(ncid, netids%dimids(ntdim_1dgeopoints), len=ngeometry)
      if (ierr /= UG_NOERR) then
         call SetMessage(Level_Fatal, 'Could not read the 1D network number of geometry points. Check any previous warnings.')
      end if

   end function ug_get_1d_network_branches_geometry_coordinate_count

!> This function gets the coordinates of the network nodes
   function ug_read_1d_network_nodes(ncid, netids, nodesX, nodesY, nodeids, nodelongnames) result(ierr)

      integer, intent(in) :: ncid
      type(t_ug_network), intent(in) :: netids
      double precision, intent(out) :: nodesX(:), nodesY(:)
      character(len=*), optional, intent(out) :: nodeids(:), nodelongnames(:)
      integer :: ierr

      ierr = nf90_get_var(ncid, netids%varids(ntid_1dnodex), nodesX)
      if (ierr /= UG_NOERR) then
         call SetMessage(Level_Fatal, 'Could not read 1D network node x-coordinates. Check any previous warnings.')
      end if

      ierr = nf90_get_var(ncid, netids%varids(ntid_1dnodey), nodesY)
      if (ierr /= UG_NOERR) then
         call SetMessage(Level_Fatal, 'Could not read 1D network node y-coordinates. Check any previous warnings.')
      end if

      if (present(nodeids)) then
         ierr = nf90_get_var(ncid, netids%varids(ntid_1dnodids), nodeids)
         if (ierr /= UG_NOERR) then
            call SetMessage(Level_Fatal, 'Could not read 1D network node ids. Check any previous warnings.')
         end if
      end if

      if (present(nodelongnames)) then
         ierr = nf90_get_var(ncid, netids%varids(ntid_1dnodlongnames), nodelongnames)
         if (ierr /= UG_NOERR) then
            call SetMessage(Level_Fatal, 'Could not read 1D network node longnames. Check any previous warnings.')
         end if
      end if

   end function ug_read_1d_network_nodes

!> This function reads the network branches
   function ug_get_1d_network_branches(ncid, netids, sourcenodeid, targetnodeid, branchlengths, nbranchgeometrypoints, startIndex, nbranchid, nbranchlongnames) result(ierr)
      use array_module
      integer, intent(in) :: ncid, startIndex
      type(t_ug_network), intent(in) :: netids
      integer, intent(out) :: sourcenodeid(:), targetnodeid(:), nbranchgeometrypoints(:)
      real(kind=dp), intent(out) :: branchlengths(:)
      character(len=*), intent(out), optional :: nbranchid(:), nbranchlongnames(:)
      integer :: ierr, n, k, nbranches, varStartIndex
      integer, allocatable :: sourcestargets(:, :)

      nbranches = size(sourceNodeId)
      allocate (sourcestargets(2, nbranches))

      ierr = nf90_get_var(ncid, netids%varids(ntid_1dedgenodes), sourcestargets)
      if (ierr /= UG_NOERR) then
         call SetMessage(Level_Fatal, 'Could not read the 1D network branches''s start and end nodes. Check any previous warnings.')
      end if

      !we check for the start_index, we do not know if the variable was written as 0 based
      ierr = nf90_get_att(ncid, netids%varids(ntid_1dedgenodes), 'start_index', varStartIndex)
      if (ierr == UG_NOERR) then
         ierr = convert_start_index(sourcestargets(1, :), imiss, varStartIndex, startIndex)
         ierr = convert_start_index(sourcestargets(2, :), imiss, varStartIndex, startIndex)
      else
         ierr = convert_start_index(sourcestargets(1, :), imiss, 0, startIndex)
         ierr = convert_start_index(sourcestargets(2, :), imiss, 0, startIndex)
      end if

      k = 0
      do n = 1, nBranches
         k = k + 1
         sourceNodeId(n) = sourcestargets(1, k)
         targetNodeId(n) = sourcestargets(2, k)
      end do

      ierr = nf90_get_var(ncid, netids%varids(ntid_1dbranchlengths), branchlengths)
      if (ierr /= UG_NOERR) then
         call SetMessage(Level_Fatal, 'Could not read the 1D network branch lengths. Check any previous warnings.')
      end if

      ierr = nf90_get_var(ncid, netids%varids(ntid_1dgeopointsperbranch), nbranchgeometrypoints)
      if (ierr /= UG_NOERR) then
         call SetMessage(Level_Fatal, 'Could not read the 1D network branches geometry points. Check any previous warnings.')
      end if

      if (present(nbranchid) .and. present(nbranchlongnames)) then
         ierr = nf90_get_var(ncid, netids%varids(ntid_1dbranchids), nbranchid)
         if (ierr /= UG_NOERR) then
            call SetMessage(Level_Fatal, 'Could not read the 1D network branch ids. Check any previous warnings.')
         end if

         ierr = nf90_get_var(ncid, netids%varids(ntid_1dbranchlongnames), nbranchlongnames)
         if (ierr /= UG_NOERR) then
            call SetMessage(Level_Fatal, 'Could not read the 1D network branch longnames. Check any previous warnings.')
         end if
      end if

   end function ug_get_1d_network_branches

!> This function writes the branch order array
   function ug_get_1d_network_branchorder(ncid, netids, branchorder) result(ierr)

      integer, intent(in) :: ncid
      type(t_ug_network), intent(in) :: netids !< Set of NetCDF-ids for network
      integer, intent(out) :: branchorder(:)
      integer :: ierr

      ierr = nf90_get_var(ncid, netids%varids(ntid_1dbranchorder), branchorder)
      if (ierr /= UG_NOERR) then
         call SetMessage(Level_Fatal, 'Could not read the 1D network branch order. Check any previous warnings.')
      end if

   end function ug_get_1d_network_branchorder

   function ug_get_1d_network_branchtype(ncid, netids, branchtype) result(ierr)

      integer, intent(in) :: ncid
      type(t_ug_network), intent(in) :: netids !< Set of NetCDF-ids for network
      integer, intent(out) :: branchtype(:)
      integer :: ierr

      ierr = nf90_get_var(ncid, netids%varids(ntid_1dbranchtype), branchtype)
      if (ierr /= UG_NOERR) then
         call SetMessage(Level_Fatal, 'Could not read the 1D network branch type. Check any previous warnings.')
      end if

   end function ug_get_1d_network_branchtype

!> This function reads the coordinates of the geometry points
   function ug_read_1d_network_branches_geometry(ncid, netids, geopointsX, geopointsY) result(ierr)

      integer, intent(in) :: ncid
      type(t_ug_network), intent(in) :: netids
      real(kind=dp), intent(out) :: geopointsX(:), geopointsY(:)
      integer :: ierr

      ierr = nf90_get_var(ncid, netids%varids(ntid_1dgeox), geopointsX)
      if (ierr /= UG_NOERR) then
         call SetMessage(Level_Fatal, 'Could not read the network 1D geometry x-coordinates. Check any previous warnings.')
      end if

      ierr = nf90_get_var(ncid, netids%varids(ntid_1dgeoy), geopointsY)
      if (ierr /= UG_NOERR) then
         call SetMessage(Level_Fatal, 'Could not read the network 1D geometry y-coordinates. Check any previous warnings.')
      end if

   end function ug_read_1d_network_branches_geometry

!> This function gets the number of mesh points (i.e., the nodes).
   function ug_get_1d_mesh_discretisation_points_count(ncid, meshids, nmeshpoints) result(ierr)

      integer, intent(in) :: ncid
      type(t_ug_mesh), intent(in) :: meshids
      integer, intent(out) :: nmeshpoints
      integer :: ierr

      ierr = nf90_inquire_dimension(ncid, meshids%dimids(mdim_node), len=nmeshpoints)
      if (ierr /= UG_NOERR) then
         call SetMessage(Level_Fatal, 'Could not read the number of 1D mesh nodes. Check any previous warnings.')
      end if

   end function ug_get_1d_mesh_discretisation_points_count

!> This function reads the geometry information for the mesh points
   function ug_get_1d_mesh_discretisation_points(ncid, meshids, nodebranchidx, nodeoffsets, startIndex, coordx, coordy) result(ierr)
      use array_module
      integer, intent(in) :: ncid, startIndex
      type(t_ug_mesh), intent(in) :: meshids
      real(kind=dp), intent(out) :: nodeoffsets(:)
      integer, intent(out) :: nodebranchidx(:)

      real(kind=dp), intent(out), optional :: coordx(:), coordy(:)
      integer :: ierr, varStartIndex

      ierr = nf90_get_var(ncid, meshids%varids(mid_1dnodebranch), nodebranchidx)

      !we check for the start_index, we do not know if the variable was written as 0 based
      ierr = nf90_get_att(ncid, meshids%varids(mid_1dnodebranch), 'start_index', varStartIndex)
      if (ierr == UG_NOERR) then
         ierr = convert_start_index(nodebranchidx, imiss, varStartIndex, startIndex)
      else
         ierr = convert_start_index(nodebranchidx, imiss, 0, startIndex)
      end if

      !define dim
      if (ierr /= UG_NOERR) then
         call SetMessage(Level_Fatal, 'Could not read the 1D mesh node branch ids. Check any previous warnings.')
      end if
      ierr = nf90_get_var(ncid, meshids%varids(mid_1dnodeoffset), nodeoffsets)
      if (ierr /= UG_NOERR) then
         call SetMessage(Level_Fatal, 'Could not read the 1D mesh node offsets. Check any previous warnings.')
      end if

      if (present(coordx)) then
         ierr = nf90_get_var(ncid, meshids%varids(mid_nodex), coordx)
         if (ierr /= UG_NOERR) then
            call SetMessage(Level_Fatal, 'Could not read the 1D mesh node x-coordinates. Check any previous warnings.')
         end if
      end if

      if (present(coordy)) then
         ierr = nf90_get_var(ncid, meshids%varids(mid_nodey), coordy)
         if (ierr /= UG_NOERR) then
            call SetMessage(Level_Fatal, 'Could not read the 1D mesh nodes y-coordinates. Check any previous warnings.')
         end if
      end if

   end function ug_get_1d_mesh_discretisation_points

!> This function reads the coordinate values for the mesh edges.
!! That is, branch index and offsets, and optionally the x/y-coordinates.
!! For x/y-coordinates: these are only read from file when present, not calculated from offsets.
   function ug_get_1d_mesh_edge_coordinates(ncid, meshids, edgebranchidx, edgeoffsets, startIndex, edgex, edgey, requireOptionals) result(ierr)
      use array_module
      integer, intent(in) :: ncid               !< NetCDF dataset id, should be already open.
      integer, intent(in) :: startIndex         !< Desired startIndex in output arrays. May be different from startIndex in the file's variables.
      type(t_ug_mesh), intent(in) :: meshids            !< Set of NetCDF-ids for all mesh geometry arrays.
      integer, intent(out) :: edgebranchidx(:)   !< Array in which the branch index for all edges will be stored.
      real(kind=dp), intent(out) :: edgeoffsets(:)     !< Array in which the offset for all edges will be stored.

      real(kind=dp), intent(out), optional :: edgex(:), edgey(:) !< The array in which the x and y-coordinates for all edges will be stored (if present).
      logical, intent(in), optional :: requireOptionals   !< The array in which the x and y-coordinates for all edges will be stored (if present).
      integer :: ierr               !< Result status, ug_noerr if successful. Nonzero if some arrays could not be read from file.
      integer :: ierrloc, numerr
      logical :: optionalsRequired ! local version of req.optionals (to avoid many if(present(...))s)

      integer :: varStartIndex, ierrOptional
      character(len=ug_nameLen) :: meshname, varname

      optionalsRequired = .true.     ! .true. to make the behaviour of the method backwards compatible.
      if (present(requireOptionals)) then
         optionalsRequired = requireOptionals
      end if

      numerr = 0
      ierrOptional = 0
      ierr = nf90_inquire_variable(ncid, meshids%varids(mid_meshtopo), name=meshname)

      ierr = nf90_get_var(ncid, meshids%varids(mid_1dedgebranch), edgebranchidx)
      if (ierr /= nf90_noerr) then
         edgebranchidx(:) = imiss ! UNST-2795: Protect against a bug in NetCDF lib: when variable does not exist, returned array may be polluted.
         if (meshids%varids(mid_1dedgebranch) <= 0) then
            call SetMessage(LEVEL_WARN, 'ug_get_1d_mesh_edge_coordinates: could not find the 1D mesh edge branch ids variable for ''' &
                            //trim(meshname)//'''. Is the :edge_coordinates attribute complete?')
         else
            ierrloc = nf90_inquire_variable(ncid, meshids%varids(mid_1dedgebranch), name=varname)
            call SetMessage(LEVEL_WARN, 'ug_get_1d_mesh_edge_coordinates: could not read the 1D mesh edge branch ids for ''' &
                            //trim(meshname)//''' from '''//trim(varname)//'''. Check any previous warnings.')
         end if
         numerr = numerr + 1
      end if

      !we check for the start_index, we do not know if the variable was written as 0 based
      ierr = nf90_get_att(ncid, meshids%varids(mid_1dedgebranch), 'start_index', varStartIndex)
      if (ierr == nf90_noerr) then
         ierr = convert_start_index(edgebranchidx, imiss, varStartIndex, startIndex)
      else
         ierr = convert_start_index(edgebranchidx, imiss, 0, startIndex)
      end if

      ierr = nf90_get_var(ncid, meshids%varids(mid_1dedgeoffset), edgeoffsets)
      if (ierr /= nf90_noerr) then
         if (meshids%varids(mid_1dedgeoffset) <= 0) then
            call SetMessage(LEVEL_WARN, 'ug_get_1d_mesh_edge_coordinates: could not find the 1D mesh edge branch offsets variable for ''' &
                            //trim(meshname)//'''. Is the :edge_coordinates attribute complete?')
         else
            ierrloc = nf90_inquire_variable(ncid, meshids%varids(mid_1dedgeoffset), name=varname)
            call SetMessage(LEVEL_WARN, 'ug_get_1d_mesh_edge_coordinates: could not read the 1D mesh edge branch offsets for ''' &
                            //trim(meshname)//''' from '''//trim(varname)//'''. Check any previous warnings.')
         end if
         numerr = numerr + 1
      end if

      if (present(edgex)) then
         ierrOptional = nf90_get_var(ncid, meshids%varids(mid_edgex), edgex)
         if (ierrOptional /= nf90_noerr) then
            if (optionalsRequired) then
               if (meshids%varids(mid_edgex) <= 0) then
                  call SetMessage(LEVEL_WARN, 'ug_get_1d_mesh_edge_coordinates: could not find the 1D mesh edge x-coordinates variable for ''' &
                                  //trim(meshname)//'''. Is the :edge_coordinates attribute complete?')
               else
                  ierrloc = nf90_inquire_variable(ncid, meshids%varids(mid_edgex), name=varname)
                  call SetMessage(LEVEL_WARN, 'ug_get_1d_mesh_edge_coordinates: could not read the 1D mesh edge x-coordinates for ''' &
                                  //trim(meshname)//''' from '''//trim(varname)//'''. Check any previous warnings.')
               end if
               numerr = numerr + 1
               ierr = ierrOptional
            end if
         end if
      end if

      if (present(edgey)) then
         ierrOptional = nf90_get_var(ncid, meshids%varids(mid_edgey), edgey)
         if (ierrOptional /= nf90_noerr) then
            if (optionalsRequired) then
               if (meshids%varids(mid_edgey) <= 0) then
                  call SetMessage(LEVEL_WARN, 'ug_get_1d_mesh_edge_coordinates: could not find the 1D mesh edge y-coordinates variable for ''' &
                                  //trim(meshname)//'''. Is the :edge_coordinates attribute complete?')
               else
                  ierrloc = nf90_inquire_variable(ncid, meshids%varids(mid_edgey), name=varname)
                  call SetMessage(LEVEL_WARN, 'ug_get_1d_mesh_edge_coordinates: could not read the 1D mesh edge y-coordinates for ''' &
                                  //trim(meshname)//''' from '''//trim(varname)//'''. Check any previous warnings.')
               end if
               numerr = numerr + 1
               ierr = ierrOptional
            end if
         end if
      end if

      if (numerr > 0 .or. ierr /= 0) then
         goto 888
      end if

      ! Success
      ierr = ug_noerr
      return

888   continue
      ! Some error occurred

   end function ug_get_1d_mesh_edge_coordinates

!
! Cloning functions
!

! we assume  global attributes are already present in the file (it is needed to read the file back in later)
   function ug_clone_mesh_definition(ncidin, ncidout, meshidsin, meshidsout) result(ierr)
      use netcdf_utils, only: ncu_copy_atts

      integer, intent(in) :: ncidin, ncidout
      type(t_ug_mesh), intent(in) :: meshidsin
      type(t_ug_mesh), intent(inout) :: meshidsout
      integer :: i, ierr, xtype, ndims, nAtts, dimvalue
      integer :: attval
      logical :: isMeshTopo
      integer, dimension(nf90_max_var_dims) :: dimids
      character(len=nf90_max_name) :: name
      integer, dimension(nf90_max_dims) :: dimmap, outdimids

      ierr = UG_SOMEERR
      ierr = nf90_redef(ncidout) !open NetCDF in define mode

      !copy dimensions
      do i = mdim_start + 1, mdim_end - 1
         if (meshidsin%dimids(i) /= -1) then
            !get variable attributes
            ierr = nf90_inquire_dimension(ncidin, meshidsin%dimids(i), name=name, len=dimvalue)
            if (ierr /= nf90_noerr) then
               return
            end if
            !define variable in the new file, first check if it is already present.
            !if is not present we will get an error, and we kniw we have to define the variable.
            ierr = nf90_inq_dimid(ncidout, name, meshidsout%dimids(i))
            if (ierr /= nf90_noerr) then
               ierr = nf90_def_dim(ncidout, name, dimvalue, meshidsout%dimids(i))
            end if
            if (ierr /= nf90_noerr) then
               return
            end if
            !now maps the dimensions
            dimmap(meshidsin%dimids(i)) = meshidsout%dimids(i); 
         end if
      end do

      !copy variables and attrubutes
      do i = mid_start + 1, mid_end - 1
         if (meshidsin%varids(i) /= -1) then
            !get variable attributes
            dimids = 0
            outdimids = 0
            ierr = nf90_inquire_variable(ncidin, meshidsin%varids(i), name=name, xtype=xtype, ndims=ndims, dimids=dimids, nAtts=nAtts)
            if (ierr /= nf90_noerr) then
               return
            end if
            !inquire if the variable is already present
            outdimids(1:ndims) = dimmap(dimids(1:ndims))
            ierr = nf90_inquire_variable(ncidout, meshidsout%varids(i), name=name, xtype=xtype, ndims=ndims, dimids=dimids, nAtts=nAtts)
            if (ierr == nf90_noerr) then
               !the variable is already present, here we should issue an error
               return
            end if
            if (ndims > 0) then
               ierr = nf90_def_var(ncidout, trim(name), xtype, outdimids(1:ndims), meshidsout%varids(i))
            else
               ierr = nf90_def_var(ncidout, trim(name), xtype, meshidsout%varids(i))
            end if
            if (ierr /= nf90_noerr) then
               !the variable will not be copied because not present
               return
            end if

            ! if is a 1d mesh we need to refer in the coordinate_space to the varid of the network geometry

            isMeshTopo = ug_is_mesh_topology(ncidin, meshidsin%varids(i))
            ierr = nf90_get_att(ncidin, meshidsin%varids(mid_meshtopo), 'topology_dimension', attval)
            if (ierr == nf90_noerr .and. isMeshTopo .and. attval == 1) then
               !we have a 1d mesh topology, we need to put the right index for the coordinate variable.
               ierr = ug_copy_var_atts_mesh1d(ncidin, ncidout, meshidsin%varids(i), meshidsout%varids(i), meshidsout%varids(mid_1dtopo))
            else
               ierr = ncu_copy_atts(ncidin, ncidout, meshidsin%varids(i), meshidsout%varids(i))
            end if
            if (ierr /= nf90_noerr) then
               return
            end if
         end if
      end do

      !end definition for ncidout
      ierr = nf90_enddef(ncidout)

   end function ug_clone_mesh_definition

   function ug_clone_mesh_data(ncidin, ncidout, meshidsin, meshidsout) result(ierr)

      integer, intent(in) :: ncidin, ncidout
      type(t_ug_mesh), intent(in) :: meshidsin, meshidsout
      integer :: i, dim, ierr, xtype, ndims, nAtts
      integer, dimension(nf90_max_var_dims) :: dimids, dimsizes
      character(len=nf90_max_name) :: name

      ! end definition
      ierr = UG_SOMEERR
      ierr = nf90_enddef(ncidout)

      do i = mid_start + 1, mid_end - 1
         if (meshidsin%varids(i) /= -1) then

            !get the variable attributes
            ierr = nf90_inquire_variable(ncidin, meshidsin%varids(i), name=name, xtype=xtype, ndims=ndims, dimids=dimids, nAtts=nAtts)
            if (ierr /= nf90_noerr) then
               return
            end if
            !inquire the variable dimensions
            dimsizes = 0
            do dim = 1, ndims
               ierr = nf90_inquire_dimension(ncidin, dimids(dim), len=dimsizes(dim))
            end do

            !get and write the variables
            select case (xtype)
            case (nf90_int)
               ierr = ug_copy_int_var(ncidin, ncidout, meshidsin%varids(i), meshidsout%varids(i), ndims, dimsizes)
            case (nf90_real)
               ierr = ug_copy_real_var(ncidin, ncidout, meshidsin%varids(i), meshidsout%varids(i), ndims, dimsizes)
            case (nf90_double)
               ierr = ug_copy_double_var(ncidin, ncidout, meshidsin%varids(i), meshidsout%varids(i), ndims, dimsizes)
            case (nf90_char)
               ierr = ug_copy_char_var(ncidin, ncidout, meshidsin%varids(i), meshidsout%varids(i), ndims, dimsizes)
            case default
               ierr = -1
            end select

            if (ierr /= nf90_noerr) then
               return
            end if

         end if
      end do

   end function ug_clone_mesh_data

!> Clones a network topology variable from an input dataset into an output dataset,
!! including all underlying dimensions and variables.
!!
!! The cloning of the actual data must be done later, using ug_clone_network_data().
   function ug_clone_network_definition(ncidin, ncidout, netidsin, netidsout) result(ierr)
      use netcdf_utils, only: ncu_copy_atts

      integer, intent(in) :: ncidin    !< NetCDF id of input dataset
      integer, intent(in) :: ncidout   !< NetCDF id of output dataset
      type(t_ug_network), intent(in) :: netidsin  !< network struct with dim+varids in input dataset
      type(t_ug_network), intent(inout) :: netidsout !< network struct with newly created dim+varids in output dataset

      integer :: i, ierr, xtype, ndims, nAtts, dimvalue
      integer, dimension(nf90_max_var_dims) :: dimids
      character(len=nf90_max_name) :: name
      integer, dimension(nf90_max_dims) :: dimmap, outdimids

      logical :: wasInDefineMode

      ierr = UG_SOMEERR

      ierr = nf90_redef(ncidout) !open NetCDF in define mode
      wasInDefineMode = (ierr == nf90_eindefine) ! Was already in define mode.

      !copy dimensions
      do i = ntdim_start + 1, ntdim_end - 1
         if (netidsin%dimids(i) /= -1) then
            !get variable attributes
            ierr = nf90_inquire_dimension(ncidin, netidsin%dimids(i), name=name, len=dimvalue)
            if (ierr /= nf90_noerr) then
               return
            end if
            !define variable in the new file, first check if it is already present.
            !if is not present we will get an error, and we know we have to define the variable.
            ierr = nf90_inq_dimid(ncidout, name, netidsout%dimids(i))
            if (ierr /= nf90_noerr) then
               ierr = nf90_def_dim(ncidout, name, dimvalue, netidsout%dimids(i))
            end if
            if (ierr /= nf90_noerr) then
               return
            end if
            !now maps the dimensions
            dimmap(netidsin%dimids(i)) = netidsout%dimids(i); 
         end if
      end do

      !copy variables and attributes
      do i = ntid_start + 1, ntid_end - 1
         if (netidsin%varids(i) /= -1) then
            !get variable attributes
            dimids = 0
            outdimids = 0
            ierr = nf90_inquire_variable(ncidin, netidsin%varids(i), name=name, xtype=xtype, ndims=ndims, dimids=dimids, nAtts=nAtts)
            if (ierr /= nf90_noerr) then
               return
            end if
            !inquire if the variable is already present
            outdimids(1:ndims) = dimmap(dimids(1:ndims))
            ierr = nf90_inquire_variable(ncidout, netidsout%varids(i), name=name, xtype=xtype, ndims=ndims, dimids=dimids, nAtts=nAtts)
            if (ierr == nf90_noerr) then
               !the variable is already present, here we should issue an error
               return
            end if
            if (ndims > 0) then
               ierr = nf90_def_var(ncidout, trim(name), xtype, outdimids(1:ndims), netidsout%varids(i))
            else
               ierr = nf90_def_var(ncidout, trim(name), xtype, netidsout%varids(i))
            end if
            if (ierr /= nf90_noerr) then
               !the variable will not be copied because not present
               return
            end if

            ierr = ncu_copy_atts(ncidin, ncidout, netidsin%varids(i), netidsout%varids(i))

            if (ierr /= nf90_noerr) then
               return
            end if
         end if
      end do

      ! Leave the dataset in the same mode as we got it.
      if (.not. wasInDefineMode) then
         ierr = nf90_enddef(ncidout)
      end if

   end function ug_clone_network_definition

!> Clones the data for a network topology variable from an input dataset into an output dataset.
!!
!! The cloning of the dim+var dimensions must have been done before, using ug_clone_network_definition().
   function ug_clone_network_data(ncidin, ncidout, netidsin, netidsout) result(ierr)
      integer, intent(in) :: ncidin    !< NetCDF id of input dataset
      integer, intent(in) :: ncidout   !< NetCDF id of output dataset
      type(t_ug_network), intent(in) :: netidsin  !< network struct with dim+varids in input dataset
      type(t_ug_network), intent(in) :: netidsout !< network struct with already existing dim+varids in output dataset

      integer :: i, dim, ierr, xtype, ndims, nAtts
      integer, dimension(nf90_max_var_dims) :: dimids, dimsizes
      character(len=nf90_max_name) :: name

      logical :: wasInDefineMode

      ierr = UG_SOMEERR

      ierr = nf90_enddef(ncidout) ! end definition phase
      wasInDefineMode = (ierr /= nf90_enotindefine) ! Was in define mode.

      do i = ntid_start + 1, ntid_end - 1
         if (netidsin%varids(i) /= -1) then

            !get the variable attributes
            ierr = nf90_inquire_variable(ncidin, netidsin%varids(i), name=name, xtype=xtype, ndims=ndims, dimids=dimids, nAtts=nAtts)
            if (ierr /= nf90_noerr) then
               return
            end if
            !inquire the variable dimensions
            dimsizes = 0
            do dim = 1, ndims
               ierr = nf90_inquire_dimension(ncidin, dimids(dim), len=dimsizes(dim))
            end do

            !get and write the variables
            select case (xtype)
            case (nf90_int)
               ierr = ug_copy_int_var(ncidin, ncidout, netidsin%varids(i), netidsout%varids(i), ndims, dimsizes)
            case (nf90_real)
               ierr = ug_copy_real_var(ncidin, ncidout, netidsin%varids(i), netidsout%varids(i), ndims, dimsizes)
            case (nf90_double)
               ierr = ug_copy_double_var(ncidin, ncidout, netidsin%varids(i), netidsout%varids(i), ndims, dimsizes)
            case (nf90_char)
               ierr = ug_copy_char_var(ncidin, ncidout, netidsin%varids(i), netidsout%varids(i), ndims, dimsizes)
            case default
               ierr = UG_SOMEERR
            end select

            if (ierr /= nf90_noerr) then
               return
            end if

         end if
      end do

      ! Leave the dataset in the same mode as we got it.
      if (wasInDefineMode) then
         ierr = nf90_redef(ncidout)
      end if

   end function ug_clone_network_data

!> Clones a mesh topology contact variable from an input dataset into an output dataset,
!! including all underlying dimensions. The related variables to be cloned are specified
!! by the caller in the varidmap(:) array.
!! All dimesions and variables definition are copied from an input file to the output file,
!! except the mesh contact variable, whose second dimension (the merged link number) is obtained before calling this subroutine.
   function ug_clone_contact_definition(ncidin, ncidout, numl1d2d, nvars, varidmap, cidsin, cidsout, id_outvars) result(ierr)
      use netcdf_utils, only: ncu_copy_atts

      integer, intent(in) :: ncidin     !< NetCDF id of input dataset
      integer, intent(in) :: ncidout    !< NetCDF id of output dataset
      integer, intent(in) :: numl1d2d   !< Number of 1d2d links
      integer, intent(in) :: nvars      !< Number of variables
      integer, dimension(nvars), intent(in) :: varidmap   !< Mapping array of contact variable ids to all variables
      type(t_ug_contacts), intent(in) :: cidsin     !< Contact struct with dim+varids in input dataset
      type(t_ug_contacts), intent(inout) :: cidsout    !< Contact struct with newly created dim+varids in output dataset
      integer, dimension(nvars), intent(out) :: id_outvars !< Ids of contact variables in the output file

      integer :: i, j, ierr, xtype, ndims, nAtts, dimvalue, id, idout
      integer, dimension(nf90_max_var_dims) :: dimids
      character(len=nf90_max_name) :: varname, dimname
      integer, dimension(nf90_max_dims) :: outdimids

      logical :: wasInDefineMode

      ierr = UG_SOMEERR

      ierr = nf90_redef(ncidout) !open NetCDF in define mode
      wasInDefineMode = (ierr == nf90_eindefine) ! Was already in define mode.

      !copy dimensions
      do i = cdim_start + 1, cdim_end - 1
         ! get dimension id from an input file
         if (cidsin%dimids(i) /= -1) then
            !get dimension attributes
            ierr = nf90_inquire_dimension(ncidin, cidsin%dimids(i), name=dimname, len=dimvalue)
            if (ierr /= nf90_noerr) then
               return
            end if
            !define dimension in the new file, first check if it is already present.
            !if is not present we will get an error, and we know we have to define the dimension.
            ierr = nf90_inq_dimid(ncidout, dimname, cidsout%dimids(i))
            if (ierr /= nf90_noerr) then
               if (i == cdim_ncontacts) then ! The second dimension of the contact connectivity variable, its length is the number of merged 1d2d links
                  ierr = nf90_def_dim(ncidout, dimname, numl1d2d, cidsout%dimids(i))
               else
                  ierr = nf90_def_dim(ncidout, dimname, dimvalue, cidsout%dimids(i))
               end if
            end if
            if (ierr /= nf90_noerr) then
               return
            end if
         end if
      end do

      !copy variables and attributes
      do i = 1, nvars
         id = varidmap(i) ! the variable id in the input file
         !get variable attributes
         dimids = 0
         outdimids = 0
         ierr = nf90_inquire_variable(ncidin, id, name=varname, xtype=xtype, ndims=ndims, dimids=dimids, nAtts=nAtts)
         if (ierr /= nf90_noerr) then
            return
         end if

         !inquire if the variable is already present
         ierr = nf90_inq_varid(ncidout, varname, idout)
         if (ierr == nf90_noerr) then
            !the variable is already present
            cycle
         end if
         ! Gets the dimension ids of this variable from the output file
         do j = 1, ndims
            ierr = nf90_inquire_dimension(ncidin, dimids(j), dimname)
            ierr = nf90_inq_dimid(ncidout, dimname, outdimids(j))
         end do

         ierr = nf90_def_var(ncidout, trim(varname), xtype, outdimids(1:ndims), id_outvars(i))

         if (ierr /= nf90_noerr) then
            !the variable will not be copied because not present
            return
         end if

         ierr = ncu_copy_atts(ncidin, ncidout, id, id_outvars(i))

         if (ierr /= nf90_noerr) then
            return
         end if
      end do

      ! Leave the dataset in the same mode as we got it.
      if (.not. wasInDefineMode) then
         ierr = nf90_enddef(ncidout)
      end if

   end function ug_clone_contact_definition

!integer copy function
   function ug_copy_int_var(ncidin, ncidout, meshidin, meshidout, ndims, dimsizes) result(ierr)

      integer, intent(in) :: ncidin, ncidout, meshidin, meshidout, ndims, dimsizes(:)
      integer :: ierr
      integer, allocatable :: value(:), value2d(:, :)

      if (ndims == 0) then
         !scalar
         allocate (value(1))
         ierr = nf90_get_var(ncidin, meshidin, value)
         ierr = nf90_put_var(ncidout, meshidout, value)
      else if (ndims == 1) then
         !vector
         allocate (value(dimsizes(1)))
         ierr = nf90_get_var(ncidin, meshidin, value)
         ierr = nf90_put_var(ncidout, meshidout, value)
      else if (ndims == 2) then
         !matrix
         allocate (value2d(dimsizes(1), dimsizes(2)))
         ierr = nf90_get_var(ncidin, meshidin, value2d)
         ierr = nf90_put_var(ncidout, meshidout, value2d)
      end if

   end function

!real copy function
   function ug_copy_real_var(ncidin, ncidout, meshidin, meshidout, ndims, dimsizes) result(ierr)

      integer, intent(in) :: ncidin, ncidout, meshidin, meshidout, ndims, dimsizes(:)
      integer :: ierr
      real, allocatable :: value(:), value2d(:, :)

      if (ndims == 0) then
         !scalar
         allocate (value(1))
         ierr = nf90_get_var(ncidin, meshidin, value)
         ierr = nf90_put_var(ncidout, meshidout, value)
      else if (ndims == 1) then
         !vector
         allocate (value(dimsizes(1)))
         ierr = nf90_get_var(ncidin, meshidin, value)
         ierr = nf90_put_var(ncidout, meshidout, value)
      else if (ndims == 2) then
         !matrix
         allocate (value2d(dimsizes(1), dimsizes(2)))
         ierr = nf90_get_var(ncidin, meshidin, value2d)
         ierr = nf90_put_var(ncidout, meshidout, value2d)
      end if

   end function

!double copy function
   function ug_copy_double_var(ncidin, ncidout, meshidin, meshidout, ndims, dimsizes) result(ierr)

      integer, intent(in) :: ncidin, ncidout, meshidin, meshidout, ndims, dimsizes(:)
      integer :: ierr
      real(kind=kind(1.0d0)), allocatable :: value(:), value2d(:, :)

      if (ndims == 0) then
         !scalar
         allocate (value(1))
         ierr = nf90_get_var(ncidin, meshidin, value)
         ierr = nf90_put_var(ncidout, meshidout, value)
      else if (ndims == 1) then
         !vector
         allocate (value(dimsizes(1)))
         ierr = nf90_get_var(ncidin, meshidin, value)
         ierr = nf90_put_var(ncidout, meshidout, value)
      else if (ndims == 2) then
         !matrix
         allocate (value2d(dimsizes(1), dimsizes(2)))
         ierr = nf90_get_var(ncidin, meshidin, value2d)
         ierr = nf90_put_var(ncidout, meshidout, value2d)
      end if

   end function

!For characters we always assume size 2
   function ug_copy_char_var(ncidin, ncidout, meshidin, meshidout, ndims, dimsizes) result(ierr)

      integer, intent(in) :: ncidin, ncidout, meshidin, meshidout, ndims, dimsizes(:)
      integer :: ierr
      character(len=dimsizes(1)) :: value2d(dimsizes(2))

      ierr = nf90_get_var(ncidin, meshidin, value2d)
      ierr = nf90_put_var(ncidout, meshidout, value2d)

   end function

!copy the variable attributes
   function ug_copy_var_atts_mesh1d(ncidin, ncidout, varidin, varidout, coordvarid) result(ierr)

      integer, intent(in) :: ncidin, ncidout, varidin, varidout, coordvarid
      integer :: ierr
      integer :: i
      character(len=nf90_max_name) :: attname
      integer :: natts

      ierr = -1
      ierr = nf90_inquire_variable(ncidin, varidin, nAtts=natts)
      if (ierr == nf90_enotvar) then
         ierr = nf90_inquire(ncidin, nAttributes=natts)
      end if
      if (ierr /= nf90_noerr) then
         return
      end if

      do i = 1, natts
         ierr = nf90_inq_attname(ncidin, varidin, i, attname)
         if (ierr /= nf90_noerr) then
            return
         end if

         !is a mapped mesh, we need to put the right variable here
         if (trim(attname) == 'coordinate_space') then
            ierr = nf90_put_att(ncidout, varidout, 'coordinate_space', coordvarid)
            if (ierr /= nf90_noerr) then
               return
            end if
            cycle
         end if

         ierr = nf90_copy_att(ncidin, varidin, attname, ncidout, varidout)
         if (ierr /= nf90_noerr) then
            return
         end if
      end do

   end function ug_copy_var_atts_mesh1d

!
! Get mesh ids
!

   function ug_ionc_get_number_of_networks(ug_file, nnumNetworks) result(ierr)

      type(t_ug_file), intent(in) :: ug_file      !< ID of already opened data set.
      integer, intent(inout) :: nnumNetworks
      integer :: ierr

      ierr = UG_NOERR
      nnumNetworks = 0
      if (allocated(ug_file%netids)) then
         if (size(ug_file%netids) > 0) then
            nnumNetworks = size(ug_file%netids)
         end if
      end if

   end function ug_ionc_get_number_of_networks

   function ug_get_number_of_meshes(ncid, ug_file, meshType, numMeshes) result(ierr)

      type(t_ug_file), intent(in) :: ug_file      !< ID of already opened data set.
      integer, intent(in) :: ncid, meshType
      integer, intent(inout) :: numMeshes
      integer :: ierr, i, attval

      ierr = UG_NOERR
      numMeshes = 0
      if (size(ug_file%meshids) > 0) then

         !All meshes
         if (meshType == 0) then
            numMeshes = size(ug_file%meshids)
            return
         else
            !Only meshType
            do i = 1, size(ug_file%meshids)
               ierr = nf90_get_att(ncid, ug_file%meshids(i)%varids(mid_meshtopo), 'topology_dimension', attval)
               if (attval /= meshType) then
                  cycle
               end if
               numMeshes = numMeshes + 1
            end do
         end if
      end if

   end function ug_get_number_of_meshes

   function ug_get_network_ids(ug_file, networkids) result(ierr)

      type(t_ug_file), intent(in) :: ug_file      !< ID of already opened data set.
      integer, intent(inout) :: networkids(:)
      integer :: ierr, i, nnumNetworks

      ierr = UG_NOERR
      ierr = ug_ionc_get_number_of_networks(ug_file, nnumNetworks)

      if (nnumNetworks /= size(networkids)) then
         ierr = -1
         return
      end if

      do i = 1, size(networkids)
         networkids(i) = i
      end do

   end function ug_get_network_ids

   function ug_get_mesh_ids(ncid, ug_file, meshType, meshids) result(ierr)

      type(t_ug_file), intent(in) :: ug_file      !< ID of already opened data set.
      integer, intent(in) :: ncid, meshType
      integer, intent(inout) :: meshids(:)
      integer :: ierr, numMeshes, i, ind, attval

      ierr = UG_NOERR
      numMeshes = 0; 
      ind = 0; 
      !All meshes
      if (meshType == 0) then
         do i = 1, size(meshids)
            meshids(i) = i
         end do
         return
      else
         !Only meshType
         do i = 1, size(ug_file%meshids)
            numMeshes = numMeshes + 1
            ierr = nf90_get_att(ncid, ug_file%meshids(i)%varids(mid_meshtopo), 'topology_dimension', attval)
            if (attval /= meshType) then
               cycle
            else
               ind = ind + 1; 
               meshids(ind) = numMeshes
            end if
         end do
      end if

   end function ug_get_mesh_ids

   function ug_get_contact_id(ncid, ug_file, contactid) result(ierr)

      integer, intent(in) :: ncid
      type(t_ug_file), intent(in) :: ug_file
      integer, intent(out) :: contactid
      integer :: ierr

      ierr = 0
      contactid = -1
      if (size(ug_file%contactids) > 0) then
         contactid = 1
      end if

   end function ug_get_contact_id

   function ug_get_1d_network_id(ncid, ug_file, networkid) result(ierr)

      integer, intent(in) :: ncid                                      !< ID of already opened data set.
      integer :: i, ierr, attval         !< Result status (UG_NOERR if successful).
      integer, intent(inout) :: networkid
      type(t_ug_file), intent(in) :: ug_file

      networkid = -1
      do i = 1, size(ug_file%netids)

         !check if it has a geometry and its dimension is 1
         if (ug_file%netids(i)%varids(ntid_1dtopo) == -1) then
            cycle
         end if

         ierr = nf90_get_att(ncid, ug_file%netids(i)%varids(ntid_1dtopo), 'topology_dimension', attval)
         if (ierr /= UG_NOERR .or. attval /= 1) then
            cycle
         end if

         networkid = i
         return

      end do

      ! nothing found, all fields to -1
      ierr = -1
      networkid = -1

   end function ug_get_1d_network_id

   function ug_get_mesh_id(ncid, ug_file, meshid, dim) result(ierr)

      integer, intent(in) :: ncid, dim
      integer :: i, ierr, attval    !< Result status (UG_NOERR if successful).
      integer, intent(inout) :: meshid
      type(t_ug_file), intent(in) :: ug_file
      logical :: isMeshTopo

      meshid = -1
      do i = 1, size(ug_file%meshids)

         if (ug_file%meshids(i)%varids(mid_meshtopo) == -1) then
            cycle
         end if
         isMeshTopo = ug_is_mesh_topology(ncid, ug_file%meshids(i)%varids(mid_meshtopo))
         if (.not. isMeshTopo) then
            cycle
         end if
         ierr = nf90_get_att(ncid, ug_file%meshids(i)%varids(mid_meshtopo), 'topology_dimension', attval)
         if (ierr /= UG_NOERR .or. attval /= dim) then
            cycle
         end if

         ! meshid found, return
         meshid = i
         return

      end do

      ! nothing found, all fields to -1
      ierr = -1
      meshid = -1

   end function ug_get_mesh_id

   function ug_count_mesh_ids_from_network_id(ncid, ug_file, netid, nmeshids) result(ierr)

      integer, intent(in) :: ncid
      integer, intent(in) :: netid
      integer, intent(inout) :: nmeshids
      type(t_ug_file), intent(in) :: ug_file
      character(len=nf90_max_name) :: networkname   !< the network name
      character(len=nf90_max_name) :: networkmeshname      !< the mesh name
      integer :: i, ierr

      ierr = UG_NOERR
      !get the variable mesh id
      networkname = ug_file%networksnames(netid); 
      nmeshids = 0
      do i = 1, size(ug_file%meshnames)
         ierr = ug_get_mesh_network_name(ncid, ug_file%meshids(i), networkmeshname)
         if (trim(networkname) == trim(networkmeshname)) then
            nmeshids = nmeshids + 1; 
         end if
      end do

   end function ug_count_mesh_ids_from_network_id

   function ug_get_mesh_ids_from_network_id(ncid, ug_file, netid, meshids) result(ierr)

      integer, intent(in) :: ncid
      integer, intent(in) :: netid
      integer, intent(inout) :: meshids(:)
      type(t_ug_file), intent(in) :: ug_file
      character(len=nf90_max_name) :: networkname   !< the network name
      character(len=nf90_max_name) :: networkmeshname      !< the mesh name
      integer :: i, ierr

      ierr = UG_NOERR
      !get the variable mesh id
      networkname = ug_file%networksnames(netid); 
      do i = 1, size(ug_file%meshnames)
         ierr = ug_get_mesh_network_name(ncid, ug_file%meshids(i), networkmeshname)
         if (trim(networkname) == trim(networkmeshname)) then
            meshids(i) = i; 
         end if
      end do

   end function ug_get_mesh_ids_from_network_id

   function ug_get_network_id_from_mesh_id(ncid, meshids, ug_file, networkid) result(ierr)

      integer, intent(in) :: ncid
      type(t_ug_mesh), intent(in) :: meshids
      type(t_ug_file), intent(in) :: ug_file
      integer, intent(inout) :: networkid
      character(len=nf90_max_name) :: networkname          !< the network name
      character(len=nf90_max_name) :: networkmeshname      !< the network name associated with the mesh
      integer :: i, ierr

      ierr = ug_get_mesh_network_name(ncid, meshids, networkmeshname)
      ierr = UG_NOERR
      networkid = -1

      if (ierr == 0 .and. len_trim(networkmeshname) > 0) then
         do i = 1, size(ug_file%netids)
            ierr = ug_get_network_name(ncid, ug_file%netids(i), networkname)
            if (ierr /= nf90_noerr) then
               exit
            end if
            if (trim(networkmeshname) == trim(networkname)) then
               networkid = i
               exit
            end if
         end do
      end if

   end function ug_get_network_id_from_mesh_id

!< Gets contact id from the name of a mesh contact.
   function ug_get_contact_id_from_meshcontactname(ncid, ug_file, meshContactName, contactid) result(ierr)

      integer, intent(in) :: ncid                 !< NetCDF dataset id, should be already open.
      type(t_ug_file), intent(in) :: ug_file              !< Ug_file struct
      character(len=*), intent(in) :: meshContactName      !< the contact name associated with the mesh
      integer, intent(inout) :: contactid            !< the requrestd contact id
      character(len=nf90_max_name) :: contactname          !< the contact name

      integer :: i, ierr

      ierr = UG_NOERR
      contactid = -1

      if (ierr == 0) then
         do i = 1, size(ug_file%contactids)
            ierr = ug_get_contact_name(ncid, ug_file%contactids(i), contactname)
            if (trim(meshContactName) == trim(contactname)) then
               contactid = i
               exit
            end if
         end do
      end if

   end function ug_get_contact_id_from_meshcontactname

!> Gets the name of the contact topology variable in an open dataset.
   function ug_get_contact_name(ncid, contactids, meshContactName) result(ierr)
      integer, intent(in) :: ncid            !< NetCDF dataset id, should be already open.
      type(t_ug_contacts), intent(in) :: contactids      !< Set of NetCDF-ids for all contact ids.
      character(len=*), intent(out) :: meshContactName !< The name of the mesh topology variable.
      integer :: ierr            !< Result status, ug_noerr if successful.

      meshContactName = ''
      ierr = nf90_inquire_variable(ncid, contactids%varids(cid_contacttopo), name=meshContactName)
      if (ierr /= nf90_noerr) then
         write (ug_messagestr, '(a,i0)') 'ug_get_mesh_name: could not find meshContactName for topology var id ', contactids%varids(cid_contacttopo)
         ierr = UG_INVALID_MESHNAME
         call SetMessage(Level_Fatal, ug_messagestr)
      end if
   end function ug_get_contact_name

!> Returns the total number of variables that are available on the specified mesh, which
!! could be a 1D or 2D mesh, or a mesh contact.
!! For example, for "mesh1d", all variables whose names contain "mesh1d" will be collected.
!! If jaids > 0, then also returns the ids of these variables.
   function ug_get_var_total_count(ncid, mids, cids, jaid, nvars, id_vars) result(ierr)
      use string_module
      integer, intent(in) :: ncid       !< NetCDF dataset id, should be already open.
      type(t_ug_mesh), optional, intent(in) :: mids       !< Set of NetCDF-ids for all mesh geometry arrays.
      type(t_ug_contacts), optional, intent(in) :: cids       !< Set of NetCDF-ids for all contact geometry arrays.
      integer, intent(in) :: jaid       !< Returns variable ids (1) or not (0)
      integer, intent(out) :: nvars      !< Number of variables defined on the requested mesh.
      integer, optional, intent(out) :: id_vars(:) !< Array to store the variable ids in.
      integer :: ierr       !< Result status, ug_noerr if successful.

      integer :: numVar
      character(len=nf90_max_name) :: str, meshname
      integer :: varid         !< NetCDF variable ID (1-based).
      str = ''
      meshname = ''
      if (present(mids)) then ! A 1D or 2D mesh
         ierr = nf90_inquire_variable(ncid, mids%varids(mid_meshtopo), name=meshname)
         if (ierr /= nf90_noerr) then
            write (ug_messagestr, '(a,i0)') 'ug_get_var_total_count: could not find mesh name for topology var id ', mids%varids(mid_meshtopo)
            ierr = UG_INVALID_MESHNAME
            call SetMessage(Level_Fatal, ug_messagestr)
            goto 999
         end if
      else if (present(cids)) then ! A mesh contact
         ierr = nf90_inquire_variable(ncid, cids%varids(cid_contacttopo), name=meshname)
         if (ierr /= nf90_noerr) then
            write (ug_messagestr, '(a,i0)') 'ug_get_var_total_count: could not find mesh contact name for topology var id ', cids%varids(cid_contacttopo)
            ierr = UG_INVALID_CONTACTNAME
            call SetMessage(Level_Fatal, ug_messagestr)
            goto 999
         end if
      else
         write (ug_messagestr, '(a,i0)') 'ug_get_var_total_count: either mids or cids must be given '
         ierr = UG_SOMEERR
         call SetMessage(Level_Fatal, ug_messagestr)
         goto 999
      end if

      ! Now check all variables and if they're data variables on the right mesh.
      ierr = nf90_inquire(ncid, nVariables=numVar)

      nvars = 0
      do varid = 1, numVar
         ! check mesh name
         str = ''
         ierr = nf90_inquire_variable(ncid, varid, name=str)
         if (ierr /= nf90_noerr) then
            exit
         end if

         ! TODO: this should actually be based on a "mesh:" attribute (not on variable name substring) to match UGRID conventions.
         if (index(str, trim(meshname)) > 0) then
            nvars = nvars + 1
            if (jaid > 0) then
               id_vars(nvars) = varid
            end if
         end if
      end do

      if (ierr /= nf90_noerr) then
         write (ug_messagestr, '(a)') 'ug_get_var_total_count: error occurred when inquiring variable names '
         ierr = UG_ENOTVAR
         call SetMessage(Level_Fatal, ug_messagestr)
         goto 999
      else
         ierr = UG_NOERR
      end if

999   continue

   end function ug_get_var_total_count
end module io_ugrid
