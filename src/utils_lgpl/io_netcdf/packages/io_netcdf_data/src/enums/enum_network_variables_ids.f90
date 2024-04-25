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

module m_enum_network_variables_ids

   implicit none

!network variables
   enum, bind(C)
      enumerator::ntid_start = 1
      enumerator::ntid_1dtopo                     !< Top-level variable for 1d network topology
      enumerator::ntid_1dgeometry                 !< Variable ID for 1d geometry points
      enumerator::ntid_1dbranchids                !< Variable ID for 1d branches ids
      enumerator::ntid_1dbranchlongnames          !< Variable ID for 1d branches long names
      enumerator::ntid_1dbranchlengths            !< Variable ID for 1d branches lengths
      enumerator::ntid_1dgeopointsperbranch       !< Variable ID for number of geometry points per branch
      enumerator::ntid_1dgeox                     !< Coordinate variable ID for 1d geometry points x-coordinate
      enumerator::ntid_1dgeoy                     !< Coordinate variable ID for 1d geometry points y-coordinate
      enumerator::ntid_1dnodex
      enumerator::ntid_1dnodey
      enumerator::ntid_1dnodids
      enumerator::ntid_1dnodlongnames
      enumerator::ntid_1dedgenodes
      enumerator::ntid_1dbranchorder              !< Coordinate variable for the branch order
      enumerator::ntid_1dbranchtype              !< Coordinate variable for the branch order
      enumerator::ntid_end
   end enum

end module m_enum_network_variables_ids
