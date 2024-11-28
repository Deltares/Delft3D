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

module m_alreadyhexa

implicit none

private

public :: alreadyhexa

contains

  subroutine ALREADYHEXA(K1, K2, K3, K4, K5, K6, JA)
     use m_netw
     use m_qnerror

     integer :: K1, K2, K3, K4, K5, K6, JA

     integer :: n1
     integer :: n2
     integer :: n3
     integer :: n4
     integer :: n5
     integer :: n6
     integer :: np
     JA = 0

!  IF (K1 .EQ. 61 .OR. K2 .EQ. 61 .OR. K3 .EQ. 61 .OR. K4 .EQ. 61 .OR. K5 .EQ. 61 .OR. K6 .EQ. 61) THEN
!     JA = 1
!     RETURN ! FOOTBALL
!  ENDIF

     do NP = NUMP, 1, -1
        if (netcell(NP)%N == 6) then
           N1 = netcell(NP)%NOD(1)
           N2 = netcell(NP)%NOD(2)
           N3 = netcell(NP)%NOD(3)
           N4 = netcell(NP)%NOD(4)
           N5 = netcell(NP)%NOD(5)
           N6 = netcell(NP)%NOD(6)
           if ((K1 == N1 .or. K1 == N2 .or. K1 == N3 .or. K1 == N4 .or. K1 == N5 .or. K1 == N6) .and. &
               (K2 == N1 .or. K2 == N2 .or. K2 == N3 .or. K2 == N4 .or. K2 == N5 .or. K2 == N6) .and. &
               (K3 == N1 .or. K3 == N2 .or. K3 == N3 .or. K3 == N4 .or. K3 == N5 .or. K3 == N6) .and. &
               (K4 == N1 .or. K4 == N2 .or. K4 == N3 .or. K4 == N4 .or. K4 == N5 .or. K4 == N6) .and. &
               (K5 == N1 .or. K5 == N2 .or. K5 == N3 .or. K5 == N4 .or. K5 == N5 .or. K5 == N6) .and. &
               (K6 == N1 .or. K6 == N2 .or. K6 == N3 .or. K6 == N4 .or. K6 == N5 .or. K6 == N6)) then
              JA = np
              call qnerror('already 6', ' ', ' ')
              return
           end if
        end if
     end do

  end subroutine ALREADYHEXA

end module m_alreadyhexa
