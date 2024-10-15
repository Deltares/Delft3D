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
module m_del_elem
   implicit none
contains
    subroutine DELELEM(K1, K2, LNU)
       use m_netw

       integer :: K1, K2, LNU
       integer :: l1
       integer :: l2
       integer :: nm1
       integer :: nm2

       LNU = 0
       do L1 = 1, NMK(K1)
          do L2 = 1, NMK(K2)
             if (LNU == 0 .and. NOD(K1)%LIN(L1) == NOD(K2)%LIN(L2)) then
                LNU = NOD(K1)%LIN(L1)
                NOD(K1)%LIN(L1) = 0
                NOD(K2)%LIN(L2) = 0
             end if
          end do
       end do
       if (LNU == 0) then
!       KN(1,LNU) = 0
!       KN(2,LNU) = 0
          return
       end if

       do L1 = 1, NMK(K1)
          if (NOD(K1)%LIN(L1) == 0) then
             NMK(K1) = NMK(K1) - 1
             do NM1 = L1, NMK(K1)
                NOD(K1)%LIN(NM1) = NOD(K1)%LIN(NM1 + 1)
             end do
             exit
          end if
       end do
       if (NMK(K1) == 0) KC(K1) = 0

       do L2 = 1, NMK(K2)
          if (NOD(K2)%LIN(L2) == 0) then
             NMK(K2) = NMK(K2) - 1
             do NM2 = L2, NMK(K2)
                NOD(K2)%LIN(NM2) = NOD(K2)%LIN(NM2 + 1)
             end do
             exit
          end if
       end do
       if (NMK(K2) == 0) KC(K2) = 0

       KN(1, LNU) = 0
       KN(2, LNU) = 0

       ! RMAS      = RHO*RL(LNU)*EA(LNU)*1D-6
       ! RM(K1)    = RM(K1) - RMAS/2
       ! RM(K2)    = RM(K2) - RMAS/2

       ! EA(LNU)   = 0
       ! RL(LNU)   = 0

       return
    end subroutine DELELEM
end module m_del_elem
