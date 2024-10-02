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

  subroutine TEKNODE(KP, NCOL)
     use m_dptabs
     use m_dmovabs
     use m_dlnabs
     use m_dcirr
     use m_netw
     use unstruc_colors
     use m_set_col
     implicit none
     integer :: KP, NCOL
     integer :: k1
     integer :: k2
     integer :: l
     integer :: n

     call SETCOL(NCOL)
     do N = 1, NMK(KP)
        L = NOD(KP)%LIN(N)
        K1 = KN(1, L)
        K2 = KN(2, L)
        if (k1 > 0 .and. k2 > 0) then
           call DMOVABS(XK(K1), YK(K1), ZK(K1))
           call DLNABS(XK(K2), YK(K2), ZK(K2))
        end if
     end do

     if (NCOL > 0) then
        call SETCOL(NCOLNN)
        do N = 1, NMK(KP)
           L = NOD(KP)%LIN(N)
           K1 = KN(1, L)
           K2 = KN(2, L)
           if (k1 > 0) then
              call DPTABS(XK(K1), YK(K1), ZK(K1))
           end if
           if (k2 > 0) then
              call DPTABS(XK(K2), YK(K2), ZK(K2))
           end if
        end do
     end if

     if (KC(KP) == -1) call DCIRR(XK(KP), YK(KP), ZK(KP), NCOL)
     return
  end subroutine TEKNODE
