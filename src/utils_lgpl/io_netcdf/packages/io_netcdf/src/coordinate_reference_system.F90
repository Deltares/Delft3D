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

!> Module for utility types and functions for working with coordinates in different coordinate systems.
module coordinate_reference_system
   use messagehandling
   use m_ug_crs
   use netcdf

   implicit none

   character(len=48),  parameter :: WGS84_PROJ_STRING         = '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'
         !< Projection string for WGS84 system. See http://www.spatialreference.org/ref/epsg/4326/proj4/

   character(len=226), parameter :: RIJKSDRIEHOEK_PROJ_STRING = '+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.999908 +x_0=155000 +y_0=463000 +ellps=bessel +units=m' &
      // ' +towgs84=565.4174,50.3319,465.5542,-0.398957388243134,0.343987817378283,-1.87740163998045,4.0725 +no_defs'
         !< Projection string for Dutch RijksDriehoek system. See https://publicwiki.deltares.nl/display/NETCDF/Coordinates :
         !! "note that the default proj4 (epsg) string for the Dutch RD system (EPSG:28992 & EPSG:7415) is wrong, it contains an erroneous ellipse reference, hence the full ellipse values need to be supplied."

   contains

!!
!! Inquiry functions: detecting coordinate reference systems, grid mappings, etc.
!!

!> Finds the (first eligible) grid_mapping variable in a NetCDF dataset.
!! Search is in the following order:
!! 1. user-specified preferred_name (if given)
!! 2. 'projected_coordinate_system'
!! 3. 'wgs84'
!! 4. The first variable that has an attribute :grid_mapping_name
function find_grid_mapping_var(ncid, varid, preferred_name) result(ierr)
   integer,                    intent(in   ) :: ncid           !< NetCDF dataset id
   integer,                    intent(  out) :: varid          !< The NetCDF variable ID pointing to the grid mapping variable, if found (1-based).
   character(len=*), optional, intent(in   ) :: preferred_name !< Searches first for the given variable name, before trying the defaults.
   integer                                   :: ierr           !< Result status (IONC_NOERR==NF90_NOERR) if successful.

   integer :: numvar        !< number of variables in the netCDF file.
   integer :: loc_varid     !< The NetCDF variable ID used in loop (1-based).
   logical :: found

   ierr = 0 ! TODO: AvD: into separate ionc_constants.F90
   found = .false.

   ! 1. preferred_name
   if (present(preferred_name)) then
      ierr = nf90_inq_varid(ncid, preferred_name, varid)
      if (ierr == nf90_noerr) then
         found = is_grid_mapping(ncid, varid)
      end if
   end if

   if (found) then
      return
   end if

   ! 2. projected_coordinate_system
   ierr = nf90_inq_varid(ncid, 'projected_coordinate_system', varid)
   if (ierr == nf90_noerr) then
      found = is_grid_mapping(ncid, varid)
   end if

   if (found) then
      return
   end if

   ! 3. wgs84
   ierr = nf90_inq_varid(ncid, 'wgs84', varid)
   if (ierr /= nf90_noerr) then
      ierr = nf90_inq_varid(ncid, 'WGS84', varid)  ! needed for DIMR sets 2.0.6, 2.0.7 and 2.0.8
   end if
   if (ierr == nf90_noerr) then
      found = is_grid_mapping(ncid, varid)
   end if

   if (found) then
      return
   end if

   ! 4. remaining variables
   ierr = nf90_inquire(ncid, nVariables = numvar)
   do loc_varid = 1,numvar
      found = is_grid_mapping(ncid, loc_varid)
      if (found) then
         varid = loc_varid
         return
      end if
   end do

   ! X. Nothing found
   ierr = 123 ! TODO: AvD: make a separate ionc_constants.F90 for this

end function find_grid_mapping_var


!> Returns whether the specified variable is a grid_mapping variable.
!! A variable is considered a grid_mapping variable if it has the
!! attribute :grid_mapping_name.
function is_grid_mapping(ncid, varid)
   integer,                    intent(in   ) :: ncid           !< NetCDF dataset id
   integer,                    intent(in   ) :: varid          !< NetCDF variable id
   logical :: is_grid_mapping !< Indicates whether the variable is a grid_mapping variable.

   integer :: ierr
   integer :: attlen

   ierr = nf90_inquire_attribute(ncid, varid, name='grid_mapping_name', len=attlen)
   is_grid_mapping = (ierr == nf90_noerr) ! Note: we don't check on the actual attribute value.
end function is_grid_mapping


!> Detects and initializes the PROJ-string in a given coordinate reference system.
!! Stored in the crs%proj_string attribute, for repeated use later.
function detect_proj_string(crs) result(ierr)
   use string_module, only: strcmpi, char_array_to_string_by_len
   implicit none

   type(t_crs),         intent(inout) :: crs         !< The coordinate reference system container.
   integer                            :: ierr        !< Result status (IONC_NOERR==NF90_NOERR) if successful.

   integer :: i, natts
   logical :: found

   ierr = 0 ! TODO: AvD
   found = .false.
   natts = size(crs%attset)
   do i=1,natts
      if (strcmpi(crs%attset(i)%attname, 'proj4_params') .and. crs%attset(i)%len > 0) then
         crs%proj_string = char_array_to_string_by_len(crs%attset(i)%strvalue, crs%attset(i)%len)
         found = .true.
      end if
   end do

   if (.not. found) then
      ierr = get_proj_string_from_epsg(crs%epsg_code, crs%proj_string)
   end if
end function detect_proj_string


!> Gives the PROJ-string for a given EPSG code.
!! NOTE: This routine is a convenience callback. Preferrably, the strings come directly from attribute values in a data file.
function get_proj_string_from_epsg(epsg, proj_string) result(ierr)
   use string_module
   implicit none

   integer,             intent(in   ) :: epsg        !< The EPSG code for the coordinate reference system.
   character(len=1024), intent(  out) :: proj_string !< The PROJ-string for the given crs.
   integer                            :: ierr        !< Result status (IONC_NOERR==NF90_NOERR) if successful.

   ierr = 0 ! TODO: AvD

   select case(epsg)
   case (4326)
      proj_string = WGS84_PROJ_STRING
   case (28992)
      proj_string = RIJKSDRIEHOEK_PROJ_STRING
   case (25832)
      proj_string = '+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs'
   case(31467)
      proj_string = '+proj=tmerc +lat_0=0 +lon_0=9 +k=1 +x_0=3500000 +y_0=0 +ellps=bessel +datum=potsdam +units=m +no_defs '
   case default
      ierr = 13 ! TODO: AvD
   end select

end function get_proj_string_from_epsg


!!
!! Transformation functions: the actual coordinate transformations
!!


#ifdef HAVE_PROJ
   !> Transforms the given coordinates using the provided coordinate transformation.
   !! This subroutine uses the proj library for coordinate transformations.
   subroutine transform(coord_trans, src_x, src_y, dst_x, dst_y)
      use proj6

      implicit none

      type(pj_object), intent(in)                        :: coord_trans !< source coordinate system object.
      real(kind=kind(1.0d00)), dimension(:), intent(in)  :: src_x       !< x coordinates to transform in degrees/meters.
      real(kind=kind(1.0d00)), dimension(:), intent(in)  :: src_y       !< y coordinates to transform in degrees/meters.
      real(kind=kind(1.0d00)), dimension(:), intent(out) :: dst_x       !< transformed x coordinates in degrees/meters.
      real(kind=kind(1.0d00)), dimension(:), intent(out) :: dst_y       !< transformed y coordinates in degrees/meters.

      integer                 :: error, i
      character(len=1024)     :: message !< Temporary variable for writing log messages.
      type(pj_coord_object)   :: coords(size(src_x))

      coords = [(pj_coord_object(x=src_x(i), y=src_y(i)), i = 1, size(src_x))]
      error = proj_trans_f(coord_trans, PJ_FWD, coords)
      if (error /= 0) then
         ! Put back original coordinates.
         dst_x = src_x
         dst_y = src_y
         write(message, *) 'Error (', error, ') while transforming coordinates.'
         call mess(LEVEL_ERROR, trim(message))
         return
      endif

      ! Copy results to destination.
      do i = 1, size(coords)
         dst_x(i) = coords(i)%x
         dst_y(i) = coords(i)%y
      end do
   end subroutine

   !> Transforms the given coordinates from the given source coordinate system to the given destination coordinate system.
   ! This subroutine uses the proj library for coordinate transformations.
   subroutine transform_coordinates(src_proj_string, dst_proj_string, src_x, src_y, dst_x, dst_y)
      use proj6

      implicit none

      character(len=*),                      intent(in)  :: src_proj_string !< proj string describing source coordinate system.
      character(len=*),                      intent(in)  :: dst_proj_string !< proj string describing destination coordinate system.
      real(kind=kind(1.0d00)), dimension(:), intent(in)  :: src_x           !< x coordinates to transform in degrees/meters.
      real(kind=kind(1.0d00)), dimension(:), intent(in)  :: src_y           !< y coordinates to transform in degrees/meters.
      real(kind=kind(1.0d00)), dimension(:), intent(out) :: dst_x           !< transformed x coordinates in degrees/meters.
      real(kind=kind(1.0d00)), dimension(:), intent(out) :: dst_y           !< transformed y coordinates in degrees/meters.

      type(pj_object) :: coord_trans !< Proj coordinate transformation.

      ! Initialize coordinate transformation
      coord_trans = proj_create_crs_to_crs(pj_default_ctx, src_proj_string, dst_proj_string, pj_area_object())

      if (proj_associated_pj(coord_trans)) then
         call transform(coord_trans, src_x, src_y, dst_x, dst_y)
      end if

      coord_trans = proj_destroy(coord_trans)
   end subroutine

   !> Transforms input arrays of projected x/y coordinates into lon/lat coordinates and directly writes them to a NetCDF dataset.
   subroutine transform_and_put_latlon_coordinates(ncid, varid_lon, varid_lat, src_proj_string, src_x, src_y, start, count)
      implicit none

      integer,                                         intent(in) :: ncid            !< NetCDF data set id.
      integer,                                         intent(in) :: varid_lon       !< NetCDF varid for longitude coordinates.
      integer,                                         intent(in) :: varid_lat       !< NetCDF varid for latitude coordinates.
      character(len=*),                                intent(in) :: src_proj_string !< proj4 string describing source coordinate system.
      real(kind=kind(1.0d00)), dimension(:),           intent(in) :: src_x           !< x coordinates to transform in degrees/meters.
      real(kind=kind(1.0d00)), dimension(:),           intent(in) :: src_y           !< y coordinates to transform in degrees/meters.
      integer,                 dimension(:), optional, intent(in) :: start           !< start index array for writing the lon/lat coordinates
      integer,                 dimension(:), optional, intent(in) :: count           !< count array for writing the lon/lat coordinates


      real(kind=kind(1d0)), dimension(:), allocatable :: lon, lat
      integer :: ierr
      integer, dimension(:), allocatable :: start_, count_

      if (present(start)) then
         allocate(start_(size(start)))
         start_ = start
      else
         allocate(start_(1))
         start_ = 1
      end if
      if (present(count)) then
         allocate(count_(size(count)))
         count_ = count
      else
         allocate(count_(1))
         count_ = size(src_x)
      end if

      allocate(lon(size(src_x)))
      allocate(lat(size(src_y)))

!   if (add_latlon) then ! If x,y are not in WGS84 system, then add mandatory additional lon/lat coordinates.
      call transform_coordinates(src_proj_string, WGS84_PROJ_STRING, src_x, src_y, lon, lat)
      ierr = nf90_put_var(ncid, varid_lon, lon, start = start_, count = count_)
      ierr = nf90_put_var(ncid, varid_lat, lat, start = start_, count = count_)
!   end if
   end subroutine
#endif

end module coordinate_reference_system
