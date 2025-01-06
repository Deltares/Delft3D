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

module m_shrinkyzprof

implicit none

private

public :: shrinkyzprof

contains

   subroutine SHRINKYZPROF(Y, Z, N, NX)
      use precision, only: dp
      use M_MISSING

      integer :: N, NX, NACT
      real(kind=dp) :: Y(N), Z(N)

      real(kind=dp), allocatable :: YH(:), ZH(:)

      integer :: NH, K, KM
      real(kind=dp) :: ZMIN, D01, D02, Z01, AT, ZD, ZDMIN, A, B

      allocate (YH(N), ZH(N))

      if (NX > N) then
         return
      end if

      NACT = N ! MAX NR
      NH = N; YH(1:N) = Y(1:N); ZH(1:N) = Z(1:N)

      ZMIN = 9d9
      do K = 1, NACT
         ZMIN = min(ZMIN, Z(K))
      end do

      do K = 1, NACT
         Z(K) = Z(K) - ZMIN
      end do

      AT = 0d0
      do K = 2, NACT
         D01 = Y(K) - Y(K - 1)
         Z01 = 0.5d0 * (Z(K) + Z(K - 1))
         AT = AT + D01 * Z01
      end do

      do while (NACT > NX + 1)

         ZDMIN = 9d9; KM = 0
         do K = 2, NACT - 1
            D01 = Y(K) - Y(K - 1)
            if (D01 == 0d0) then
               Y(K) = DMISS
               exit
            end if
            D02 = Y(K + 1) - Y(K - 1)
            A = D01 / D02; B = 1d0 - A
            ZD = (A * Z(K + 1) + B * Z(K - 1)) * D02
            if (abs(ZD) < ZDMIN) then
               KM = K; ZDMIN = ZD
            end if
         end do

         if (ZDMIN < 0.01 * AT) then

            do K = 2, NACT - 1

            end do

         end if

      end do

   end subroutine SHRINKYZPROF

end module m_shrinkyzprof
