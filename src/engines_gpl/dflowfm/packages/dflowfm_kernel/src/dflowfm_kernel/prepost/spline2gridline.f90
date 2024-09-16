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

!> make a gridline on the spline
subroutine spline2gridline(mc, num, xsp, ysp, xc, yc, sc, h)
   use m_makespl

   implicit none

   integer, intent(in) :: mc !< number of gridnodes
   integer, intent(in) :: num !< number of splinenodes
   double precision, dimension(num), intent(in) :: xsp, ysp !< splinenode coordinates
   double precision, dimension(mc), intent(out) :: xc, yc !< coordinates of grid points
   double precision, dimension(mc), intent(out) :: sc !< spline-coordinates of grid points
   double precision, intent(in) :: h !< for curvature adapted meshing (>0) or disable (<=0)

   double precision, dimension(2) :: startstop

   integer :: kmax

   if (mc < 2) return ! no curvigrid possible

   startstop = (/0d0, dble(num - 1)/)
   call makespl(startstop, xsp, ysp, max(mc, num), num, 2, mc - 1, xc, yc, kmax, sc, h)

   if (kmax /= mc) then
      continue
   end if

   return
end subroutine spline2gridline
