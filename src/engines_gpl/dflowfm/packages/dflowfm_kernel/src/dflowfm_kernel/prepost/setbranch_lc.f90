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

  subroutine SETBRANCH_LC(nrl1d)
     use M_NETW
     use gridoperations
     use m_okay

     implicit none

     integer :: NRL1D, NRL, NRLO, L, K, K1, K2, K3, IBR, N, JASTOP, JASTART, IERR, IBX, KS, KK, KE, ja
     integer :: NRL1D6, KN316, NRL1D16, J

     call setnodadm(0)

     if (allocated(NMK0)) deallocate (NMK0); allocate (NMK0(NUMK)); NMK0 = 0

     LC = 0; NRL1D = 0; NRL1D6 = 0
     do L = 1, NUML
        if (KN(3, L) == 1 .or. KN(3, L) == 6) then
           K1 = KN(1, L); K2 = KN(2, L); K3 = KN(3, L)
           NMK0(K1) = NMK0(K1) + 1
           NMK0(K2) = NMK0(K2) + 1
           if (KN(3, L) == 1) then
              NRL1D = NRL1D + 1 ! count 1D links
           else if (KN(3, L) == 6) then
              NRL1D6 = NRL1D6 + 1
           end if
        else
           LC(L) = -1
        end if
     end do

     if (NRL1D + NRL1D6 == 0) then
        netstat = NETSTAT_OK; return
     end if

     if (allocated(IBN)) deallocate (IBN, LIB, K1BR, NRLB)
     allocate (IBN(NUML), LIB(NUML), K1BR(NUML), NRLB(NUML)); IBN = 0; LIB = 0; K1BR = 0; NRLB = 0

     IBR = 0; NRL = 0

     do J = 1, 2

        if (J == 1) then
           KN316 = 6; NRL1D16 = NRL1D6
        else
           KN316 = 1; NRL1D16 = NRL1D6 + NRL1D
        end if

        do while (NRL < NRL1D16)

           NRLO = NRL
           do L = 1, NUML
              if (LC(L) == 0) then
                 JASTART = 0
                 call GAANWESTARTEN(L, K1, KN316, JASTART)
                 if (JASTART == 1) then
                    IBR = IBR + 1
                    call WALK1D(K1, IBR, NRL, JASTOP, KN316)
                 end if
              end if
           end do

           if (NRL == NRLO) then ! REPAIR CODE, FILL IN ISOLATED BRANCHES
              do L = 1, NUML
                 if (LC(L) == 0 .and. KN316 == KN(3, L)) then
                    IBR = IBR + 1
                    LC(L) = IBR; NRL = NRL + 1
                    LIB(NRL) = L; K1BR(NRL) = KN(1, L); IBN(NRL) = IBR; NRLB(L) = NRL
                 end if
              end do
           end if

        end do

     end do

     IBX = IBR; MXNETBR = IBR

     if (allocated(NETBR)) deallocate (NETBR)
     allocate (NETBR(IBX), STAT=IERR)
     call AERR('NETBR(IBX)', IERR, NUML)

     IBR = 1
     KS = 1
     NRL1D = NRL1D16

     do K = 1, NRL1D

        ja = 0
        if (k < NRL1D) then
           if (IBR /= IBN(K + 1)) then
              ja = 1
           end if
        else
           ja = 1
        end if
        if (ja == 1) then
           KE = K
           N = KE - KS + 1
           allocate (NETBR(IBR)%LN(N), STAT=IERR)
           call AERR('NETBR(IBR)%LN(N)', IERR, IBR)
           NETBR(IBR)%NX = N
           do KK = KS, KE
              L = LIB(KK)
              K1 = K1BR(KK)
              if (K1 == KN(1, L)) then
                 NETBR(IBR)%LN(KK - KS + 1) = L
              else if (K1 == KN(2, L)) then
                 NETBR(IBR)%LN(KK - KS + 1) = -L
              else
                 call OKAY(0) ! PROGRAMMING NO GOOD
              end if

           end do

           if (k < NRL1D) then
              IBR = IBN(K + 1)
              KS = K + 1
           end if

        end if

     end do

     netstat = NETSTAT_OK

  end subroutine SETBRANCH_LC
