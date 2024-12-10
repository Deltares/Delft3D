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

  subroutine POLTOLINES()
     use precision, only: dp

     use m_connect, only: connect
     use m_netw
     use m_afmeting
     use gridoperations
     use m_cconstants
     use m_dlength, only: dlength

     implicit none
     real(kind=dp) :: ael
     integer :: k
     integer :: k1
     integer :: k2
     real(kind=dp) :: rml
     real(kind=dp) :: zp

     AEL = PI * RDIAM * RDIAM / 4 ! RDIAM in mm
     do K = 1, NPL - 1
        call ISNODE(K1, XPL(K), YPL(K), ZP)
        if (K1 == 0) then
           call GIVENEWNODENUM(K1)
           call SETPOINT(XPL(K), YPL(K), ZP, K1)
        end if
        call ISNODE(K2, XPL(K + 1), YPL(K + 1), ZP)
        if (K2 == 0) then
           call GIVENEWNODENUM(K2)
           call SETPOINT(XPL(K + 1), YPL(K + 1), ZP, K2)
        end if
        RML = DLENGTH(K1, K2)
        call CONNECT(K1, K2, LFAC, RML)
     end do
     return
  end subroutine POLTOLINES
