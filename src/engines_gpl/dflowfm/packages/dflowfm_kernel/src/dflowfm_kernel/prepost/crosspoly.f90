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
module m_crosspoly
   implicit none
contains
   subroutine crosspoly(xa, ya, xb, yb, xpl, ypl, npl, XM, YM, CRPM, JA, isec, distanceStartPolygon)
      use precision, only: dp

      use m_missing
      use m_sferic, only: jsferic, jasfer3D
      use geometry_module, only: crossinbox, dbdistance

      integer :: npl, ja
      integer, intent(out) :: isec !< crossed polyline section (>0) or not crossed (0)
      !locals
      real(kind=dp) :: xa, xb, ya, yb, xm, ym, crpm
      real(kind=dp) :: xpl(npl), ypl(npl)
      real(kind=dp), intent(inout) :: distanceStartPolygon !< distance from the start point of the polygon

      integer :: jacros
      integer :: k
      integer :: ku
      real(kind=dp) :: XP1, YP1, XP2, YP2, sl, sm, XCR, YCR, CRP, currentSegmentLength

      isec = 0
      JA = 0
      distanceStartPolygon = 0.0d0
      do K = 1, NPL - 1
         KU = K + 1
         XP1 = XPL(K); YP1 = YPL(K)
         XP2 = XPL(KU); YP2 = YPL(KU)
         currentSegmentLength = dbdistance(xp1, yp1, xp2, yp2, jsferic, jasfer3D, dmiss)

         if (xp1 == DMISS .or. yp1 == DMISS .or. xp2 == DMISS .or. yp2 == DMISS) cycle ! SPvdP: added

         call CROSSinbox(XP1, YP1, XP2, YP2, Xa, Ya, Xb, Yb, jacros, sl, sm, XCR, YCR, CRP, jsferic, dmiss)

         if (jacros == 1) then
            JA = JA + 1
            XM = XCR
            YM = YCR
            crpm = crp
            isec = k
            distanceStartPolygon = distanceStartPolygon + currentSegmentLength * sl
            return ! SPvdP: added
         end if
         distanceStartPolygon = distanceStartPolygon + currentSegmentLength
      end do

   end subroutine crosspoly
end module m_crosspoly
