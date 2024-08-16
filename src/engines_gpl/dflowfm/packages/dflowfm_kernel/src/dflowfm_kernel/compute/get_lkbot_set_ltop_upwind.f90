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
!> Get 3D flow link and node indices for a given "2D" flow link based on given upwind node.
!! Also sets global Lbot array.
subroutine get_lkbot_set_ltop_upwind(link, upstream_cell, upstream_cell_index, Lb, kb, kt)
   use m_flow, only: Lbot, ktop, Ltop
   use m_turbulence, only: ln0

   implicit none

   integer, intent(in) :: link ! flow link index
   integer, intent(in) :: upstream_cell ! upstream flow node
   integer, intent(in) :: upstream_cell_index ! index in flowlink to flownode array
   integer, intent(out) :: Lb ! 3D flow link index at bed
   integer, intent(out) :: kb ! 3D flow node index at bed
   integer, intent(out) :: kt ! 3D flow node index at top

   Lb = Lbot(link)
   kt = ktop(upstream_cell)
   kb = min(ln0(upstream_cell_index, Lb), kt)
   Ltop(link) = Lb + kt - kb

end subroutine get_lkbot_set_ltop_upwind
