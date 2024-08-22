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

!> find common neighboring cell of two links (0: no cell found)
integer function common_cell_for_two_net_links(L1, L2)
   use network_data, only: lnn, lne
   implicit none

   integer, intent(in) :: L1, L2 !< net link numbers

   integer :: i, j

   common_cell_for_two_net_links = 0

   do i = 1, lnn(L1)
      do j = 1, lnn(L2)
         if (lne(i, L1) == lne(j, L2)) then
            common_cell_for_two_net_links = lne(i, L1)
            exit
         end if
      end do
   end do

   return
end function common_cell_for_two_net_links
