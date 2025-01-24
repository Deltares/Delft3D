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
module m_interpolateOnPolyline
   implicit none
contains
   !> Find a point on a polyline at a certain distance from the start.
      !! The distance is measured along the consecutive polyline segments.
   subroutine interpolateOnPolyline(X, Y, Z, T, MMAX, XP, YP, ZP, TP, JA)
      use precision, only: dp
      integer, intent(in) :: mmax !< Nr. of polyline points.
      real(kind=dp), intent(in) :: X(MMAX), Y(MMAX), Z(mmax) !< The polyline coordinates.
      real(kind=dp), intent(in) :: T(MMAX) !< Accumulated segment lengths at all points.
      real(kind=dp), intent(out) :: XP, YP, ZP !< interpolated point coordinates at distance TP.
      real(kind=dp), intent(in) :: TP !< Distance from polyline start at which to place point XP,YP.
      integer, intent(out) :: ja !< Whether distance is within polyline length (1) or not (0).

      integer :: i
      real(kind=dp) :: DT, TI
      I = 0
10    continue
      I = I + 1
      JA = 0
      if (T(I) <= TP) then
         if (I <= MMAX - 1) then
            goto 10
         end if
      end if
      JA = 1
      DT = T(I) - T(I - 1)
      TI = 0d0
      if (DT /= 0d0) TI = (TP - T(I - 1)) / DT
      XP = (1d0 - TI) * X(I - 1) + TI * X(I)
      YP = (1d0 - TI) * Y(I - 1) + TI * Y(I)
      ZP = (1d0 - TI) * Z(I - 1) + TI * Z(I)
      return
   end subroutine interpolateOnPolyline
end module m_interpolateOnPolyline
