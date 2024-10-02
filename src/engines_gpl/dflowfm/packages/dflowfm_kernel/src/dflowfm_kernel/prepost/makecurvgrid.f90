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

!> generate the curvi-grid from the node indices (ic,jc)
subroutine makecurvgrid(ic, jc)

   use m_netw
   use m_grid
   use m_missing
   use m_alloc
   use m_increase_grid

   implicit none

   integer, dimension(1:numk) :: ic, jc !< indices (i,j) of the nodes

   integer :: imin, jmin ! minimum of the indices

   integer :: i, j, node

! integer, parameter         :: IMISS = -999999

!---------------------------------------------------------
! compute grid sizes and renumber
!---------------------------------------------------------
   imin = minval(ic, ic /= IMISS)
   jmin = minval(jc, jc /= IMISS)

   ic = ic - imin + 1
   jc = jc - jmin + 1

   mc = maxval(ic)
   nc = maxval(jc)

!---------------------------------------------------------
! allocate and initialize arrays
!---------------------------------------------------------
   call increasegrid(mc, nc)
   xc = dmiss
   yc = dmiss
   zc = dmiss

!---------------------------------------------------------
! compose the grid
!---------------------------------------------------------
   do node = 1, numk
      i = ic(node)
      j = jc(node)
      if (i > 0) then
         xc(i, j) = xk(node)
         yc(i, j) = yk(node)
         zc(i, j) = zk(node)
      end if
   end do

end subroutine makecurvgrid
