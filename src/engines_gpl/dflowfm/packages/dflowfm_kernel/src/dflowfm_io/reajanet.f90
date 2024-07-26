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

      subroutine REAJANET(MNET, JA, JADOORLADEN)
         use m_netw
         use gridoperations

         implicit none
         integer :: MNET, JA, JADOORLADEN

         integer :: k
         integer :: k0
         integer :: l
         integer :: l0
         integer :: n1
         integer :: numkn
         integer :: numln
         double precision :: x10

         character REC * 3320

         if (JADOORLADEN == 0) then
            K0 = 0
            L0 = 0
         else
            K0 = NUMK
            L0 = NUML
         end if

         JA = 0
         read (MNET, '(A)', end=777) REC ! COMMENT

         read (MNET, '(A)', end=777) REC
         N1 = index(REC, '=') + 1
         read (REC(N1:), *, err=555) NUMKN

         read (MNET, '(A)', end=777) REC
         N1 = index(REC, '=') + 1
         read (REC(N1:), *, err=555) NUMP

         read (MNET, '(A)', end=777) REC

         read (MNET, '(A)', end=777) REC
         N1 = index(REC, '=') + 1
         read (REC(N1:), *, err=555) NUMLN

         read (MNET, '(A)', end=777) REC

         read (MNET, '(A)', end=777) REC

         read (MNET, '(A)', end=777) REC

         do K = 1, 4
            read (MNET, '(A)', end=777) REC
         end do

         call INCREASENETW(K0 + NUMKN, L0 + NUMLN)

         do K = K0 + 1, K0 + NUMKN
            read (MNET, '(A)', end=777) REC
            read (REC, *, ERR=999) XK(K), YK(K)
         end do
         !XK   = XK - 270000
         !YK   = YK - 2700000

         NUMK = K0 + NUMKN
         KC = 1

         do K = 1, NUMP
            read (MNET, *)
         end do

         do L = L0 + 1, L0 + NUMLN
            read (MNET, '(A)', end=777) REC
            read (REC, *, ERR=888) x10, KN(1, L), KN(2, L)
            KN(1, L) = KN(1, L) + K0
            KN(2, L) = KN(2, L) + K0
            KN(3, L) = 2
         end do
         NUML = L0 + NUMLN

         call SETNODADM(0)

         ja = 1
         return

999      call QNREADERROR('READING NETNODES, BUT GETTING ', REC, MNET)
         return

888      call QNREADERROR('READING NETLINKS, BUT GETTING ', REC, MNET)

777      call QNEOFERROR(MNET)
         return

555      call QNREADERROR('READING NR OF NETNODES, BUT GETTING ', REC, MNET)
         return

444      call QNREADERROR('READING NR OF NETLINKS, BUT GETTING ', REC, MNET)
         return

      end subroutine REAJANET
