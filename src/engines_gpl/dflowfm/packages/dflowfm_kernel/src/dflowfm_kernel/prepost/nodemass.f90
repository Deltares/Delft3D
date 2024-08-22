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

  subroutine NODEMASS()
     use m_netw
     use M_AFMETING
     use m_missing, only: jins, dmiss
     use geometry_module, only: dpinpok
     implicit none

     double precision :: ag
     double precision :: cfl
     double precision :: e0
     double precision :: eps
     integer :: in1
     integer :: in2
     integer :: k1
     integer :: k2
     integer :: l
     double precision :: pi
     double precision :: rho
     double precision :: rhow
     common / CONSTANTS / E0, RHO, RHOW, CFL, EPS, AG, PI

!  DO K = 1,NUMK
!     RM(K) = 0
!  ENDDO

     do L = 1, NUML
        K1 = KN(1, L)
        K2 = KN(2, L)
        if (K1 /= 0 .and. K2 /= 0) then
           call DPINPOK(XK(K1), YK(K1), ZK(K1), NPL, XPL, YPL, IN1, jins, dmiss)
           call DPINPOK(XK(K2), YK(K2), ZK(K2), NPL, XPL, YPL, IN2, jins, dmiss)
!        IF (IN1 .EQ. 1 .AND. IN2 .EQ. 1) THEN
!           RMAS   = RHO*RL(L)*EA(L)*1D-6
!           RM(K1) = RM(K1) + RMAS/2
!           RM(K2) = RM(K2) + RMAS/2
!        ENDIF
        end if
     end do
     return
  end subroutine NODEMASS
