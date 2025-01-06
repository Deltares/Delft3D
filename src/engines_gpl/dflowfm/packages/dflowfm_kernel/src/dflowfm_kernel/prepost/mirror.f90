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

module m_mirror

implicit none

private

public :: mirror

contains

  subroutine MIRROR()
     use m_mirr, only: mirr
     use m_netw
     use m_qnerror

     integer :: k
     integer :: k0
     integer :: kk
     integer :: l
     integer :: l0
     integer :: ll
     integer :: n

     K0 = NUMK
     L0 = NUML

     if (K0 + NUMK > KMAX) then
        call QNERROR('TOO MANY NODES: CALL KERN', ' ', ' ')
        return
     end if
     if (L0 + NUML > LMAX) then
        call QNERROR('TOO MANY ELEMENTS: CALL KERN', ' ', ' ')
        return
     end if

     do K = K0 + 1, K0 + NUMK
        KK = K - NUMK
        call MIRR(XK(KK), YK(KK), ZK(KK), XK(K), YK(K), ZK(K))
!     RM(K) = RM(KK)
        KC(K) = KC(KK)
        NMK(K) = NMK(KK)
        do N = 1, NMK(K)
           NOD(K)%LIN(N) = NOD(KK)%LIN(N) + L0
        end do
     end do
     NUMK = K0 + NUMK

     do L = L0 + 1, L0 + NUML
        LL = L - NUML
!     EA(L)   = EA(LL)
!     RL(L)   = RL(LL)
        KN(:, L) = KN(:, LL) + K0
     end do
     NUML = L0 + NUML

     return
  end subroutine MIRROR

end module m_mirror
