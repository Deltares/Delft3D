!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2024.
!
!  This file is part of Delft3D (D-Flow Flexible Mesh component).
!
!  Delft3D is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!  Delft3D  is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Affero General Public License for more details.
!
!  You should have received a copy of the GNU Affero General Public License
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D",
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------

!
!

!> Finds indices of netcells that relate to structures.
!! The indices will be used when partitioning the mesh with METIS, by giving a special weight on the netcells.
!! As a result, structures will not intercross the partition boundaries
!! NOTE: This functionality ONLY supports when using "polylinefile" to specify the structure location
!! TODO: extend it to support other ways of specifying the structure location.
module m_find_netcells_for_structures

implicit none

private

public :: find_netcells_for_structures

interface

module subroutine find_netcells_for_structures(size_istrucells, nstrucells, istrucells)
   implicit none
   integer, intent(in) :: size_istrucells !< size of istrucells array
   integer, intent(inout) :: nstrucells !< Number of the netcells that are related to structures
   integer, dimension(size_istrucells), intent(inout) :: istrucells !< Indices of the netcells that are related to structures
end subroutine find_netcells_for_structures
   
end interface
    
end module m_find_netcells_for_structures
