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

!> get left and right neighboring grid layer points
subroutine get_LR(mc, xc, yc, i, iL, iR)

   use m_missing
   use m_spline2curvi
   use geometry_module, only: dbdistance
   use m_sferic, only: jsferic, jasfer3D

   implicit none

   integer, intent(in) :: mc !< grid layer size
   double precision, dimension(mc), intent(in) :: xc, yc !< grid layer point coordinates
   integer, intent(in) :: i !< grid layer point

   integer, intent(out) :: iL, iR ! left and right neighboring grid layer points

!   double precision, parameter                  :: dtolLR = 1d-1

   integer :: jstart, jend, jacirc_loc

!  check for circular connectivity
   jacirc_loc = jacirc

   jstart = 1
   jend = mc

!  grid points may be on top of each other: find left neighboring point
   iL = i
   do while (dbdistance(xc(iL), yc(iL), xc(i), yc(i), jsferic, jasfer3D, dmiss) <= dtolLR)
      if (jacirc_loc == 0) then
         if (iL - 1 < 1) exit
      else
         if (iL - 1 < jstart) then
            iL = jend + 1
            jacirc_loc = 0 ! only once
         end if
      end if
      if (xc(iL - 1) == DMISS .or. yc(iL - 1) == DMISS) exit
      iL = iL - 1
   end do

!  find right neighboring node
   iR = i
   do while (dbdistance(xc(iR), yc(iR), xc(i), yc(i), jsferic, jasfer3D, dmiss) <= dtolLR)
      if (jacirc_loc == 0) then
         if (iR + 1 > mc) exit
      else
         if (iR + 1 > jend) then
            iR = jstart - 1
            jacirc_loc = 0 ! only once
         end if
      end if
      if (xc(iR + 1) == DMISS .or. yc(iR + 1) == DMISS) exit
      iR = iR + 1
   end do

   return
end subroutine get_LR
