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

!> I/O module for writing ACDD information (Attribute Convention for Dataset Discovery)
!! into NetCDF files.
!! ACDD Conventions website: https://wiki.esipfed.org/Category:Attribute_Conventions_Dataset_Discovery

module io_netcdf_acdd
use meshdata, only: ug_strLenMeta
use io_netcdf, only: IONC_NOERR
use netcdf
use netcdf_utils, only: ncu_ensure_define_mode, ncu_restore_mode, ncu_append_atts

implicit none

private

public :: ionc_add_geospatial_bounds
public :: ionc_add_time_coverage

character(len=8), parameter :: IONC_CONV_ACDD     = 'ACDD-1.3'      !< Version of ACDD conventions currently adopted (Attribute Convention for Dataset Discovery).

contains

!> Convenience function to set the ACDD version into a dataset's :Convevtions attribute.

function ionc_acdd_add_conventions(ncid) result(ierr)
   integer,          intent(in   ) :: ncid    !< Already opened NetCDF id to set Conventions global attribute into.
   integer                         :: ierr    !< Result status (IONC_NOERR==NF90_NOERR) if successful.

   ierr = ncu_append_atts(ncid, nf90_global, 'Conventions', trim(IONC_CONV_ACDD), separator = ' ', check_presence = .true.)
end function ionc_acdd_add_conventions  


!> Add geospatial bounding box global attributes to an open NetCDF dataset.
function ionc_add_geospatial_bounds(ncid, lat_min, lat_max, lon_min, lon_max) result(ierr)
   integer,          intent(in   ) :: ncid    !< Already opened NetCDF id to put global attributes into.
   double precision, intent(in   ) :: lat_min !< Geospatial_lat_min specifies the southernmost latitude covered by the dataset.
   double precision, intent(in   ) :: lat_max !< Geospatial_lat_max specifies the northernmost latitude covered by the dataset.
   double precision, intent(in   ) :: lon_min !< Geospatial_lon_min specifies the westernmost longitude covered by the dataset.
   double precision, intent(in   ) :: lon_max !< Geospatial_lon_max specifies the easternmost longitude covered by the dataset.
   integer                         :: ierr    !< Result status (IONC_NOERR==NF90_NOERR) if successful.

   logical :: InDefine
   character(len=ug_strLenMeta) :: geospatial_lat_units = "degrees_north"
   character(len=ug_strLenMeta) :: geospatial_lon_units = "degrees_east"

   ierr = IONC_NOERR

   ierr = ncu_ensure_define_mode(ncid, InDefine)

   ierr = nf90_put_att(ncid, nf90_global, 'geospatial_lat_min',   lat_min)
   ierr = nf90_put_att(ncid, nf90_global, 'geospatial_lat_max',   lat_max)
   ierr = nf90_put_att(ncid, nf90_global, 'geospatial_lat_units', trim(geospatial_lat_units))
   ierr = nf90_put_att(ncid, nf90_global, 'geospatial_lon_min',   lon_min)
   ierr = nf90_put_att(ncid, nf90_global, 'geospatial_lon_max',   lon_max)
   ierr = nf90_put_att(ncid, nf90_global, 'geospatial_lon_units', trim(geospatial_lon_units))

   ! Record this also in the Conventions attribute
   ierr = ionc_acdd_add_conventions(ncid)

   ! Leave the dataset in the same mode as we got it.
   ierr = ncu_restore_mode(ncid, InDefine)

end function ionc_add_geospatial_bounds


!> Add time coverage box global attributes to an open NetCDF dataset.
function ionc_add_time_coverage(ncid, time_coverage_start, time_coverage_end, time_coverage_duration, time_coverage_resolution) result(ierr)
   integer,          intent(in   ) :: ncid                     !< Already opened NetCDF id to put global attributes into.
   character(len=*), intent(in   ) :: time_coverage_start      !< Start datetime of the dataset, as string in ISO 8601 date and time format.
   character(len=*), intent(in   ) :: time_coverage_end        !< End datetime of the dataset, as string in ISO 8601 date and time format.
   character(len=*), intent(in   ) :: time_coverage_duration   !< Duration of the dataset, as string in ISO 8601 duration format.
   character(len=*), intent(in   ) :: time_coverage_resolution !< Time period between each value in the dataset, as string in ISO 8601 duration format.
   integer                         :: ierr                     !< Result status, 0 if successful.

   logical :: InDefine

   ierr = IONC_NOERR

   ierr = ncu_ensure_define_mode(ncid, InDefine)

   ! The followings use the ISO 8601:2004 date format, preferably the extended format as
   ! recommended in the Attribute Content Guidance section.
   ! time_coverage_start: Describes the time of the first data point in the data set.
   ierr = nf90_put_att(ncid, nf90_global, 'time_coverage_start', trim(time_coverage_start))

   ! time_coverage_end: Describes the time of the last data point in the data set.
   ierr = nf90_put_att(ncid, nf90_global, 'time_coverage_end', trim(time_coverage_end))

   ! time_coverage_duration: Describes the duration of the data set.
   ierr = nf90_put_att(ncid, nf90_global, 'time_coverage_duration' , trim(time_coverage_duration))

   ! time_coverage_resolution Describes the targeted time period between each value in the data set.
   ierr = nf90_put_att(ncid, nf90_global, 'time_coverage_resolution' , trim(time_coverage_resolution))

   ! Record this also in the Conventions attribute
   ierr = ionc_acdd_add_conventions(ncid)

   ! Leave the dataset in the same mode as we got it.
   ierr = ncu_restore_mode(ncid, InDefine)

end function ionc_add_time_coverage

end module io_netcdf_acdd

