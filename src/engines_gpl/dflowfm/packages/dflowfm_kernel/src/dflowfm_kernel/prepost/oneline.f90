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

  subroutine ONELINE(K) ! TWEE LIJNTJES WORDEN 1
     use m_netw
     use gridoperations
     implicit none
     integer :: K

     double precision :: a0
     double precision :: ag
     double precision :: cdflow
     double precision :: cfl
     double precision :: cfric
     double precision :: e0
     double precision :: eps
     double precision :: fbouy
     double precision :: fdyn
     integer :: ja
     integer :: janet
     integer :: k1
     integer :: k2
     integer :: l1
     integer :: l2
     integer :: lfa
     integer :: moments
     integer :: nm
     double precision :: pi
     double precision :: r0
     double precision :: rho
     double precision :: rhow

     common / SETTINGS / FDYN, FBOUY, CDFLOW, CFRIC, MOMENTS, JANET
     common / CONSTANTS / E0, RHO, RHOW, CFL, EPS, AG, PI
     JA = 0
     NM = NMK(K)
     if (NM == 2) then
        L1 = NOD(K)%LIN(1)
        L2 = NOD(K)%LIN(2)
        ! IF (RL(L1) .LT. RD .OR. RL(L2) .LT. RD) THEN
        call OTHERNODE(K, L1, K1)
        call OTHERNODE(K, L2, K2)
        R0 = 0 !  RL(L1) + RL(L2)
        A0 = 0 !(EA(L1) + EA(L2)) / 2d0
        LFA = 1
        call DELNODE(K)
        call CONNECT(K1, K2, LFA, A0, R0)
        ! ENDIF
     end if
     return
  end subroutine ONELINE
