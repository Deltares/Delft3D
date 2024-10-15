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

      subroutine REABOT(MMDD, JA)
         use M_GRID
         use m_readyy
         use m_qn_read_error
         use m_qn_eof_error
         implicit none

         integer :: mmdd, ja, m1, n1, m2, n2, L1, L2, L3, L4, L5
         integer :: m, n
         double precision :: af

         character REC * 132
         call READYY('Reading SIMONA *.bottom File', 0d0)

5        continue

         read (MMDD, '(A)', end=777) REC
         if (REC(1:3) /= 'BOX') then
            goto 5
         else
            L1 = index(REC, '=(')
            read (REC(L1 + 2:), *) M1

            L2 = L1 + index(REC(L1:), ',')
            L3 = index(REC(:), ';') - 1

            read (REC(L2:L3), *) N1

            L3 = index(REC, ';')
            read (REC(L3 + 1:), *) M2

            L4 = L3 + index(REC(L3:), ',')
            L5 = index(REC, ')') - 1

            read (REC(L4:L5), *) N2

         end if

         do M = M1, M2
            AF = dble(M) / dble(MC)
            call READYY('Reading SIMONA *.bottom File', AF)

            read (MMDD, '(A)', end=777) REC
            backspace (MMDD)

            read (MMDD, *, end=999, ERR=888) (ZC(M, N), N=N1, N2)
         end do
         goto 5

777      call READYY('Reading SIMONA *.bottom File', -1d0)
         call DOCLOSE(MMDD)
         JA = 1
         return

999      continue
         call QNEOFERROR(MMDD)
         call READYY('Reading SIMONA *.bottom File', -1d0)
         call DOCLOSE(MMDD)
         JA = 0
         return

888      call QNREADERROR('Reading ERROR SIMONA bottom File With Wrong Dimensions', ' ', MMDD)
         call READYY('Reading *.bottom File', -1d0)
         call DOCLOSE(MMDD)
         JA = 0
      end subroutine REABOT
