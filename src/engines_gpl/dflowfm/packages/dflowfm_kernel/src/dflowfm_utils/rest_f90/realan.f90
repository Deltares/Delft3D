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

    subroutine REALAN(MLAN, ANTOT)
       use m_polygon
       use M_landboundary
       use M_MISSING
       use m_readyy
       use m_qn_read_error
       use m_qn_eof_error
       implicit none
       integer, intent(inout) :: mlan
       integer, intent(inout), optional :: antot

       integer :: i
       integer :: ncl
       integer :: newlin
       integer :: nkol
       integer :: nrow
       integer :: ntot, n, k, kd, ku
       double precision :: xlr

       character MATR * 4, REC * 132
       double precision :: XL, YL, ZL

       if (present(antot)) then
          NTOT = antot
       else
          NTOT = 0
       end if

       if (ntot == 0) then
          call increaselan(10000)
       end if

       call READYY('READING land boundary', 0d0)
10     continue
       read (MLAN, '(A)', end=777, ERR=887) MATR
       if (MATR(1:1) == '*') goto 10

       read (MLAN, '(A)', end=777) REC
       read (REC, *, ERR=666) NROW, NKOL

       NEWLIN = 0
       do I = 1, NROW
          if (NTOT >= MAXLAN - 1) then
             call increaselan(NTOT + 1)
          end if
          read (MLAN, '(A)', end=999) REC
          NCL = 0
          ZL = 0
          if (nkol == 2) then
             read (REC, *, ERR=881) XL, YL
          else if (nkol == 3) then
             read (REC, *, ERR=881) XL, YL, NCL
          else if (nkol == 4) then
             read (REC, *, ERR=881) XL, YL, ZL, NCL
          end if

          XLR = XL

881       if (XL == 999.999d0 .or. XLR == 999.999d0) then
             XL = dmiss
             YL = dmiss
             ZL = dmiss
             NCL = 0
          end if
          if (NTOT == 0) then
             NTOT = NTOT + 1
             MXLAN = NTOT
             XLAN(NTOT) = XL
             YLAN(NTOT) = YL
             ZLAN(NTOT) = ZL
             NCLAN(NTOT) = NCL
          else if (XL /= XLAN(NTOT) .or. YL /= YLAN(NTOT)) then
             NTOT = NTOT + 1
             MXLAN = NTOT
             XLAN(NTOT) = XL
             YLAN(NTOT) = YL
             ZLAN(NTOT) = ZL
             NCLAN(NTOT) = NCL
          end if
          if (mod(I, 1000) == 0) then
             call READYY(' ', min(1d0, dble(I) / MAXLAN))
          end if
       end do
       NTOT = NTOT + 1
       MXLAN = NTOT
       XLAN(NTOT) = dmiss
       YLAN(NTOT) = dmiss
       ZLAN(NTOT) = dmiss

       goto 10

777    continue
       MXLAN = NTOT
       call READYY(' ', 1d0)
       call READYY(' ', -1d0)
       call doclose(MLAN)

       if (present(antot)) then
          antot = NTOT
       end if

       return

       n = 1 ! remove double points in lineseg oriented files
       xpl(n) = xlan(1); ypl(n) = ylan(1)
       do k = 2, mxlan - 1
          kd = k - 1; ku = k + 1
          if (xlan(k) == dmiss .and. xlan(kd) == xlan(ku) .and. ylan(kd) == ylan(ku)) then

          else
             n = n + 1
             xpl(n) = xlan(k); ypl(n) = ylan(k)
          end if
       end do
       n = n + 1
       xpl(n) = xlan(mxlan); ypl(n) = ylan(mxlan)

       npl = n

       return

666    call QNREADERROR('SEARCHING NROWS,NCOLS, BUT GETTING', REC, MLAN)
       MXLAN = NTOT
       call READYY(' ', 1d0)
       call READYY(' ', -1d0)
       call doclose(MLAN)
       return

888    call QNREADERROR('SEARCHING COORDINATES, BUT GETTING', REC, MLAN)
       MXLAN = NTOT
       call READYY(' ', 1d0)
       call READYY(' ', -1d0)
       call doclose(MLAN)
       return

887    call QNREADERROR('EXPECTING 4 CHAR, BUT GETTING', MATR, MLAN)
       MXLAN = NTOT
       call READYY(' ', 1d0)
       call READYY(' ', -1d0)
       call doclose(MLAN)
       return

999    call QNEOFERROR(MLAN)
       MXLAN = NTOT
       call READYY(' ', 1d0)
       call READYY(' ', -1d0)
       call doclose(MLAN)
       return

    end
