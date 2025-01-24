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
module m_jgrline8
   use m_polyline

   implicit none
contains
   subroutine JGRLINE8(X, Y, N) ! TEKEN LIJN, INCL XYMISSEN, GEBRUIK VAN INVIEW EN PROJECTIE
      use precision, only: dp

      use m_missing
      use m_inview2

      integer :: n
      real(kind=dp) :: X(N), Y(N)

      integer :: i
      integer :: in
      integer :: k
      integer :: l
      real(kind=dp) :: XA, YA
      integer, parameter :: KMAX = 4096 ! BEPERKING VAN INTERACTER
      real :: XX(KMAX), YY(KMAX)

      K = 0
      L = 0
      IN = 0
      I = 0
      do while (I < N)
         I = I + 1
         if (X(I) /= dXYMIS) then
            if (INVIEW2(X(I), Y(I), XA, YA)) IN = 1
            if (K == 0 .or. IN == 1 .or. I == L + 1) K = K + 1
            if (K == 1 .or. IN == 1 .or. I == L + 1) then
               XX(K) = XA
               YY(K) = YA
            end if
            if (IN == 1) L = I
         end if
         if (I == N .or. X(I) == dXYMIS .or. K == KMAX) then
            if (K /= 0) then
               call POLYLINE(XX, YY, K)
               if (K == KMAX) I = I - 1
               K = 0
               L = 0
               IN = 0
            end if
         end if
      end do
      return
   end subroutine JGRLINE8
end module m_jgrline8
