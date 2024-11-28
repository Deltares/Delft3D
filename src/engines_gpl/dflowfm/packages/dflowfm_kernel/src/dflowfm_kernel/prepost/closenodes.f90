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

module m_closenodes

implicit none

private

public :: closenodes

contains

  subroutine CLOSENODES(K, KK, JA) ! ARE THESE NODES CLOSE, BUT UNCONNECTED?

     use m_closeenough, only: closeenough
     use m_netw
     use m_wearelt
     use gridoperations
     use m_dlength, only: dlength

     integer :: K, KK, JA

     integer :: k2
     integer :: l1
     integer :: n
     integer :: nx

     double precision :: R0, R1, R2, SHORTESTLINK
     JA = 0
     R0 = DLENGTH(K, KK)
     if (R0 > 6d0 * RCIR) return

     L1 = NOD(K)%LIN(1)
     R1 = SHORTESTLINK(K); R2 = SHORTESTLINK(KK); R1 = min(R1, R2) * 0.4d0
     call CLOSEENOUGH(XK(K), YK(K), XK(KK), YK(KK), R1, JA)
     if (JA == 0) return

     JA = 0
     NX = size(NOD(K)%LIN)
     do N = 1, NX
        L1 = NOD(K)%LIN(N)
        call OTHERNODE(K, L1, K2)
        if (K2 == KK) then
           JA = 0; return
        end if
     end do

     NX = size(NOD(KK)%LIN)
     do N = 1, NX
        L1 = NOD(KK)%LIN(N)
        call OTHERNODE(KK, L1, K2)
        if (K2 == K) then
           JA = 0; return
        end if
     end do
     JA = 1 ! KENNELIJK UNCONNECTED

     return
  end subroutine CLOSENODES

end module m_closenodes
