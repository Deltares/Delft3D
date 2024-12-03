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

module m_connect

implicit none

private

public :: connect

contains

  subroutine CONNECT(K1, K2, LFAC, R00)
     use m_netw
     use gridoperations
     use m_cconstants
     use m_dlength, only: dlength

     integer :: K1, K2, LFAC
     double precision :: R00
     integer :: ja
     integer :: kl
     integer :: kr
     integer :: l
     integer :: ll
     integer :: lnu
     double precision :: r0

     do L = 1, NUML
        if (KN(1, L) == K1 .and. KN(2, L) == K2 .or. &
            KN(1, L) == K2 .and. KN(2, L) == K1) then
           ! CALL CONFRM('POINTS ALREADY CONNECTED, CONTINUE', JA)
           ! IF (JA .NE. 1) RETURN
           return
        end if
     end do

     R0 = R00
     if (R0 <= 0) R0 = DLENGTH(K1, K2)

     do LL = 1, LFAC

        ! CALL GIVENEWLINKNUM(LNU)   ! En increase NUML als nodig

        if (LL == 1) then
           KL = K1
           if (LFAC > 1) then ! LUS EIGENLIJK ANDERS STARTEN
              call GIVENEWNODENUM(KR)
           else
              KR = K2
           end if
        else if (LL == LFAC) then
           KL = KR
           KR = K2
        else
           KL = KR
           call GIVENEWNODENUM(KR)
        end if

        !CALL ADDLINKTONODES(KL,KR,LNU)
        !CALL CONNECTDB(KL,KR,LNU)

        call CONNECTDBN(KL, KR, LNU)

        KN(3, LNU) = KN3TYP

        XK(KL) = XK(K1) + (XK(K2) - XK(K1)) * dble(LL - 1) / dble(LFAC)
        YK(KL) = YK(K1) + (YK(K2) - YK(K1)) * dble(LL - 1) / dble(LFAC)
        ZK(KL) = ZK(K1) + (ZK(K2) - ZK(K1)) * dble(LL - 1) / dble(LFAC)
        XK(KR) = XK(K1) + (XK(K2) - XK(K1)) * dble(LL) / dble(LFAC)
        YK(KR) = YK(K1) + (YK(K2) - YK(K1)) * dble(LL) / dble(LFAC)
        ZK(KR) = ZK(K1) + (ZK(K2) - ZK(K1)) * dble(LL) / dble(LFAC)
     end do

     JA = 1

     return
  end subroutine CONNECT

end module m_connect
