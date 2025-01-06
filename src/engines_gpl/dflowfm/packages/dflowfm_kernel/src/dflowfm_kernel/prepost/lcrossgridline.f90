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

!> check if a line segment crosses the gridline on the center spline
module m_lcrossgridline

implicit none

private

public :: lcrossgridline

contains

logical function Lcrossgridline(x1, x2, j)
   use precision, only: dp
   use m_grid
   use m_missing
   use m_sferic, only: jsferic
   use geometry_module, only: cross

   real(kind=dp), dimension(2), intent(in) :: x1, x2 !< coordinates of begin and end point of line segment
   integer, intent(in) :: j !< gridline index

   real(kind=dp), dimension(2) :: x3, x4

   real(kind=dp) :: sL, sm, xcr, ycr, crp

   integer :: i, jacross

   Lcrossgridline = .false.

!   return

   do i = 1, mc - 1 ! loop over the edges
      x3 = (/xc(i, j), yc(i, j)/)
      x4 = (/xc(i + 1, j), yc(i + 1, j)/)

      if (x3(1) == DMISS .or. x4(1) == DMISS) cycle

      call cross(x1(1), x1(2), x2(1), x2(2), x3(1), x3(2), x4(1), x4(2), jacross, sL, sm, xcr, ycr, crp, jsferic, dmiss)

      if (jacross == 1) then
         Lcrossgridline = .true.
         return
      end if
   end do

   return
end function ! Lcrosscenterspline

end module m_lcrossgridline
