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

module m_reacrs

   implicit none

contains

   subroutine REAcrs(MMDD, JA)
      use M_GRID
      use m_missing
      use m_readyy
      use m_qn_read_error
      use m_qn_eof_error
      use m_filez, only: doclose, newfil

      implicit none

      integer :: mmdd, ja, m1, n1, m2, n2, MH, NH, NR2

      integer :: m, n, MOUT

      character REC * 132

      JA = 0
      NR2 = 2

      call NEWFIL(MOUT, 'sections_crs.pli')

5     continue

      read (MMDD, '(A)', end=777) REC

      if (index(rec, '#') == 0) then

         read (REC(21:), *, ERR=999) M1, N1, M2, N2

         if (M1 > M2) then
            MH = M2; M2 = M1; M1 = MH
         end if
         if (N1 > N2) then
            NH = N2; N2 = N1; N1 = NH
         end if

         !     WRITE(MOUT,'(A  )') REC(1:20)
         if (M1 == M2) then
            write (MOUT, '(A  )') REC(1:20)
            write (MOUT, '(2I8)') N2 - N1 + 2, NR2
            do N = N1 - 1, N2
               write (MOUT, *) XC(M1, N), YC(M1, N)
            end do
         end if

         if (N1 == N2) then
            if (m1 == m2) then
               write (MOUT, '(A  )') REC(1:20)//'b'
            else
               write (MOUT, '(A  )') REC(1:20)
            end if
            write (MOUT, '(2I8)') M2 - M1 + 2, NR2
            do M = M1 - 1, M2
               write (MOUT, *) XC(M, N1), YC(M, N1)
            end do
         end if

      end if

      goto 5

777   call DOCLOSE(MMDD)
      call DOCLOSE(MOUT)
      JA = 1
      return

999   continue
      call QNEOFERROR(MMDD)
      call READYY('Reading SIMONA *.bottom File', -1d0)
      call DOCLOSE(MMDD)
      JA = 0
      return

888   call QNREADERROR('Reading ERROR SIMONA WEIR File', REC, MMDD)
      call DOCLOSE(MMDD)
      JA = 0
   end subroutine REAcrs

end module m_reacrs
