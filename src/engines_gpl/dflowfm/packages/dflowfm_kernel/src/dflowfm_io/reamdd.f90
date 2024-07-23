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

      subroutine REAMDD(MMDD, RD1, MC, NC, JA)
         implicit none

         integer :: mmdd, mc, nc, ja
         double precision :: RD1(MC, NC)
         integer :: m, n
         double precision :: af

         character REC * 132
         call READYY('Reading md-Dept File', 0d0)
5        continue
         read (MMDD, '(A)', end=999) REC
         if (REC(1:1) == '*') goto 5
         backspace (MMDD)

         do N = 1, NC
            AF = dble(N) / dble(NC)
            call READYY('Reading md-Dept File', AF)
            read (MMDD, *, end=999, ERR=888) (RD1(M, N), M=1, MC)
         end do
         call READYY('Reading md-Dept File', -1d0)
         call DOCLOSE(MMDD)
         JA = 1
         return

999      continue
         call QNEOFERROR(MMDD)
         call READYY('Reading md-Dept File', -1d0)
         call DOCLOSE(MMDD)
         JA = 0
         return

888      call QNREADERROR('Reading, DD Depth File With Wrong Dimensions', ' ', MMDD)
         call READYY('Reading md-Dept File', -1d0)
         call DOCLOSE(MMDD)
         JA = 0
      end subroutine REAMDD
