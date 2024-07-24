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

  subroutine GETMIDDLEKNOT(K1, K2, K12, A12, R12)
     use m_netw
     use gridoperations
     implicit none
     integer :: K1, K2, K12, K22
     double precision :: A12, R12

     integer :: l1
     integer :: l2
     integer :: n1
     integer :: n2

     do N1 = 1, NMK(K1)
        L1 = NOD(K1)%LIN(N1)
        call OTHERNODE(K1, L1, K12)
        do N2 = 1, NMK(K2)
           L2 = NOD(K2)%LIN(N2)
           call OTHERNODE(K2, L2, K22)
           if (K12 == K22) then
              A12 = 0 ! ( EA(L1) + EA(L2) ) /2
              R12 = 0 ! ( RL(L1) + RL(L2) ) /2
              return
           end if
        end do
     end do
     K12 = 0
     return
  end subroutine GETMIDDLEKNOT
