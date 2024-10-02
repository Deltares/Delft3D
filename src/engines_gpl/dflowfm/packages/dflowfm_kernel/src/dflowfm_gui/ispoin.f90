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

module m_ispoin

implicit none

contains

    subroutine ISPOIN(X, Y, mmax, nmax, MC, NC, RD1, &
                      XL, YL, MV, NV)
       use m_disval
       use m_missing, only: xymis
       use m_wearelt

       integer, intent(in) :: mmax, nmax, mc, nc
       integer, intent(out) :: mv, nv
       double precision :: X(MMAX, NMAX), Y(MMAX, NMAX), RD1(MMAX, NMAX)
       double precision :: xl, yl

       integer :: m1, n1, m2, n2, ishot, mvol, nvol, i, j

       data MVOL/0/, NVOL/0/
       MV = 0
       NV = 0
       ISHOT = 0

666    continue
       if (ISHOT == 0 .and. MVOL /= 0) then
          M1 = max(1, MVOL - 3)
          N1 = max(1, NVOL - 3)
          M2 = min(MC, MVOL + 3)
          N2 = min(NC, NVOL + 3)
          ISHOT = 1
       else
          M1 = 1
          N1 = 1
          M2 = MC
          N2 = NC
          ISHOT = 0
       end if

       do I = M1, M2
          do J = N1, N2
             if (X(I, J) /= XYMIS) then
                if (abs(XL - X(I, J)) < RCIR) then
                   if (abs(YL - Y(I, J)) < RCIR) then
                      MV = I
                      NV = J
                      XL = X(I, J)
                      YL = Y(I, J)
                      MVOL = MV
                      NVOL = NV
                      call DISVAL(MV, NV, RD1(MV, NV))
                      return
                   end if
                end if
             end if
          end do
       end do
       if (ISHOT == 1) goto 666
       MVOL = 0
       call DISVAL(0, 0, 0d0)
       return
    end subroutine ispoin

end module m_ispoin
