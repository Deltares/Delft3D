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

!> get upstream downstream cell numbers
subroutine get_upstream_downstream_cell_numbers(left_cell_upstream, left_cell, right_cell, &
                                                upstream_cell_index, upstream_cell, downstream_cell, direction_sign)

   implicit none

   logical, intent(in) :: left_cell_upstream !< condition for considering if left cell is the upstream cell, true implies that the left cell is internal and vice-versa
   integer, intent(in) :: left_cell !< flow node of cell left of the link
   integer, intent(in) :: right_cell !< flow node of cell right of the link
   integer, intent(out) :: upstream_cell_index !< index of the flow node upstream of the link, 1 = LEFT, 2 = RIGHT
   integer, intent(out) :: upstream_cell !< flow node of cell upstream of the link
   integer, intent(out) :: downstream_cell !< flow node of cell downstream of the link
   integer, intent(out) :: direction_sign !< sign of the direction, 1=from LEFT to RIGHT, -1=from RIGHT to LEFT

   integer, parameter :: LEFT = 1
   integer, parameter :: RIGHT = 2

   if (left_cell_upstream) then
      upstream_cell_index = LEFT
      upstream_cell = left_cell
      downstream_cell = right_cell
      direction_sign = 1
   else
      upstream_cell_index = RIGHT
      upstream_cell = right_cell
      downstream_cell = left_cell
      direction_sign = -1
   end if

end subroutine get_upstream_downstream_cell_numbers
