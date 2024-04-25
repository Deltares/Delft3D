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

module m_ug_crs

   use m_ug_nc_attribute
   implicit none

!> Container for information about coordinate reference system in a netCDF-file.
   type t_crs
      character(len=64)               :: varname = ' '              !< Name of the netCDF variable containing this CRS
      character(len=64)               :: name = ' '                 !< Name of the coordinate reference system, like "Amersfoort / RD New"
      character(len=64)               :: grid_mapping_name = ' '    !< Name of the grid mapping
      integer                         :: epsg_code                  !< EPSG code (more info: http://spatialreference.org/)
      character(len=1024)             :: proj_string = ' '          !< PROJ-string (more info: http://proj4.org)
      character(len=1024)             :: wkt = ' '                  !< Well Known Text
      type(ug_nc_attribute), allocatable :: attset(:)                  !< General set with all/any attributes about this CRS.
   end type t_crs

end module m_ug_crs
