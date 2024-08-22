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

  subroutine TEKLAN(NCOL)
     use M_LANDBOUNDARY
     use m_wearelt
     use unstruc_colors
     use unstruc_display

     implicit none
     integer :: NCOL
     integer :: NDRAW
     common / DRAWTHIS / ndraw(50)

     integer :: j1
     integer :: k
     integer :: ncl
     integer :: ncold
     double precision :: rh
     logical inview

     if (NDRAW(3) == 0) return

     if (NDRAW(3) == 4 .or. NDRAW(3) == 8) then
        call linewidth(3)
     end if

     call DISP3C(XLAN, YLAN, ZLAN, NCLAN, MXLAN, 0d0, NCOL)

     NCOLD = 0
     do K = 1, MXLAN
        NCL = NCLAN(K)
        if (NCL < 0) then
           if (NCOLD == 0) then
              NCOLD = abs(NCL)
              J1 = K
           else if (abs(NCL) /= NCOLD) then
              call PFILLER(XLAN(J1), YLAN(J1), K - J1, NCOLD, NCOLD)
              NCOLD = 0
           end if
        else if (NCOLD /= 0) then
           call PFILLER(XLAN(J1), YLAN(J1), K - J1, NCOLD, NCOLD)
           NCOLD = 0
        end if
     end do
     if (ndraw(3) == 2 .or. ndraw(3) == 6) then
        call SETCOL(NCOLDG)
        rh = 0.2 * rcir
        do K = 1, MXLAN
           if (inview(xlan(k), ylan(k))) then
              !CALL PTABS( XLAN(K), YLAN(K) )
              call fbox(xlan(k) - rh, ylan(k) - rh, xlan(k) + rh, ylan(k) + rh)
           end if
        end do
     end if

     if (ndraw(3) == 3 .or. ndraw(3) == 7) then
        call SETCOL(NCOLDG)
        do K = 1, MXLAN
           if (inview(xlan(k), ylan(k))) then
              RH = 0
              call DHITEXT(K, XLAN(K), YLAN(K))
           end if
        end do
     end if

     call linewidth(1)

     return
  end subroutine TEKLAN
