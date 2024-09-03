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

!> approximate spline length
double precision function splinelength(num, xspl, yspl)

   use geometry_module, only: dbdistance
   use m_missing, only: dmiss
   use m_sferic, only: jsferic, jasfer3D
   use m_splinxy
   
   implicit none

   integer, intent(in) :: num !< number of spline control points
   double precision, dimension(num), intent(in) :: xspl, yspl !< coordinates of slpine control points

   double precision, dimension(num) :: xspl2, yspl2 !  second order derivates of spline coordinates

   double precision :: xL, yL, xR, yR, tL, tR, dt

   integer :: i, j

   integer, parameter :: NSAM = 100 ! sample factor

   call splinxy(xspl, yspl, xspl2, yspl2, num)

   tR = 0d0
   call splintxy(xspl, yspl, xspl2, yspl2, num, tR, xR, yR)

   dt = 1d0 / dble(NSAM)

   splinelength = 0d0
   do i = 1, num - 1
      tR = dble(i - 1)
      do j = 1, NSAM
         tL = tR
         xL = xR
         yL = yR
         tR = tR + dt
         call splintxy(xspl, yspl, xspl2, yspl2, num, tR, xR, yR)
         splinelength = splinelength + dbdistance(xL, yL, xR, yR, jsferic, jasfer3D, dmiss)
      end do
   end do

   return
end function splinelength
