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

module m_enum_network_dimension_ids

   implicit none

!network dimension
   enum, bind(C)
      enumerator::ntdim_start = 1
      enumerator::ntdim_1dnodes          !< Dimension ID for the number of network nodes
      enumerator::ntdim_1dgeopoints      !< Dimension ID for the geometry points
      enumerator::ntdim_1dedges          !< Dimension ID for 1d network edges (i.e., branches)
      enumerator::ntdim_idstring         !< Dimension ID for the string id
      enumerator::ntdim_longnamestring   !< Dimension ID for the string longnames
      enumerator::ntdim_two
      enumerator::ntdim_end
   end enum

end module m_enum_network_dimension_ids
