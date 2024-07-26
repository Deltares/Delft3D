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

  recursive subroutine WALK1D(K1, IBR, NRL, JASTOP, KN316)

     use m_netw
     use gridoperations

     implicit none
     integer :: K1, K2, IBR, NRL, JASTOP, KN316

     integer :: KK, L, KA

     JASTOP = 0
     do KK = 1, NMK(K1)
        L = NOD(K1)%LIN(KK)
        if (LC(L) == 0 .and. KN(3, L) == KN316) then

           call OTHERNODE(K1, L, K2)
           call GAANWESTOPPEN(K2, KN316, JASTOP, L)

           LC(L) = IBR; NRL = NRL + 1
           LIB(NRL) = L; K1BR(NRL) = K1; IBN(NRL) = IBR; NRLB(L) = NRL

           if (JASTOP == 1) then
              return
           end if

           KA = K2
           call WALK1D(KA, IBR, NRL, JASTOP, KN316)

           if (JASTOP == 1) then
              return
           end if
        end if
     end do
  end subroutine WALK1D
