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

module m_subsidence
   use precision, only: dp
   logical :: sdu_first !< Flag indicating whether this is the first call to obtain the 'bedrock_surface_elevation'
   integer :: sdu_update_s1 !< Flag indicating whether water levels at wet point should be updated (0 = no, 1 = yes)
   integer :: jasubsupl !< Flag indicating whether subsidence and uplift is included in the simulation (0 = no, 1 = yes)
   real(kind=dp), dimension(:), allocatable, target :: subsupl !< Latest field of 'bedrock_surface_elevation' interpolated onto the mesh at the location at which the initial bed levels are prescribed
   real(kind=dp), dimension(:), allocatable :: subsupl_t0 !< Initial field of 'bedrock_surface_elevation'
   real(kind=dp), dimension(:), allocatable :: subsupl_tp !< Previous field of 'bedrock_surface_elevation'
   real(kind=dp), dimension(:), allocatable :: subsout !< Output field of subsidence/uplift: latest field - initial field
   real(kind=dp), dimension(:), allocatable :: sdu_blp !< Previous field of bed level values at cell centres (temporary copy of bl(:))

contains

   subroutine default_subsupl()
      jasubsupl = 0
      sdu_first = .true.
      sdu_update_s1 = 0
   end subroutine

end module m_subsidence
