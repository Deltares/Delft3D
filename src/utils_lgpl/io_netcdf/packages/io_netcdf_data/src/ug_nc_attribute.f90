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

module m_ug_nc_attribute

   implicit none

!> Container for information for a NetCDF attribute. Used inside t_crs.
   type ug_nc_attribute
      character(len=64)             :: attname     !< Name of the attribute.
      integer                       :: xtype       !< Type: one of NF90_CHAR, NF90_INT, NF90_FLOAT, NF90_DOUBLE, NF90_BYTE, NF90_SHORT.
      integer                       :: len         !< Length of the attribute value (string length/array length)
      character(len=1), allocatable :: strvalue(:) !< Contains value if xtype==NF90_CHAR.
      double precision, allocatable :: dblvalue(:) !< Contains value if xtype==NF90_DOUBLE.
      real, allocatable             :: fltvalue(:) !< Contains value if xtype==NF90_FLOAT.
      integer, allocatable          :: intvalue(:) !< Contains value if xtype==NF90_INT.
      ! TODO: AvD: support BYTE/short as well?
   end type ug_nc_attribute

end module m_ug_nc_attribute
