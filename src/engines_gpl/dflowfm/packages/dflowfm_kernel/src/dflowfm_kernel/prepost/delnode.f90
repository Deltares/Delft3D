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

  subroutine DELNODE(KP)
     use m_netw
     use m_missing
     implicit none
     integer :: KP

     double precision :: ag
     double precision :: cfl
     double precision :: e0
     double precision :: eps
     integer :: k1
     integer :: k2
     integer :: l1
     integer :: lnu
     integer :: nm1
     double precision :: pi
     double precision :: rho
     double precision :: rhow

     common / CONSTANTS / E0, RHO, RHOW, CFL, EPS, AG, PI

     do NM1 = NMK(KP), 1, -1
        L1 = NOD(KP)%LIN(NM1)
        K1 = KN(1, L1)
        K2 = KN(2, L1)
        call DELELEM(K1, K2, LNU)
     end do
     NMK(KP) = 0
     KC(KP) = 0
     XK(KP) = dmiss
     YK(KP) = dmiss
     ZK(KP) = dmiss
     ! RM(KP)  = 0

     return
  end subroutine DELNODE
