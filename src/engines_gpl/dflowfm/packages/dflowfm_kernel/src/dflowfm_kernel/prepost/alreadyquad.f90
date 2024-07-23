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

  subroutine ALREADYQUAD(K1, K2, K3, K4, JA)
     use m_netw
     implicit none
     integer :: K1, K2, K3, K4, JA

     integer :: n1
     integer :: n2
     integer :: n3
     integer :: n4
     integer :: np
     JA = 0

     do NP = NUMP, 1, -1
        if (netcell(NP)%N == 4) then
           N1 = netcell(NP)%NOD(1)
           N2 = netcell(NP)%NOD(2)
           N3 = netcell(NP)%NOD(3)
           N4 = netcell(NP)%NOD(4)
           if ((K1 == N1 .or. K1 == N2 .or. K1 == N3 .or. K1 == N4) .and. &
               (K2 == N1 .or. K2 == N2 .or. K2 == N3 .or. K2 == N4) .and. &
               (K3 == N1 .or. K3 == N2 .or. K3 == N3 .or. K3 == N4) .and. &
               (K4 == N1 .or. K4 == N2 .or. K4 == N3 .or. K4 == N4)) then
              JA = np
              call qnerror('already 4', ' ', ' ')
              return
           end if
        end if
     end do
     return
  end subroutine ALREADYQUAD
